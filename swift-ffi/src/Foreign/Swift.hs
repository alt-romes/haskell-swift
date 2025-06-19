{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
module Foreign.Swift
  ( foreignExportSwift

    -- * Marshaling
  , swiftMarshal
  , ForeignValKind(..)
  , PtrMarshal(..)
  , JSONMarshal(..)

    -- ** Re-exports
    -- | You're using TH already: might as well use it for deriving JSON.
  , deriveJSON, defaultOptions

    -- ** Internal utils
  , tyFunArgsTy, tyFunResTy
  )
  where

import Foreign.Swift.Marshal

import Data.Aeson.TH
import qualified Data.ByteString.Lazy as BS
import Control.Monad.State
import Data.Aeson
import Foreign.C
import Foreign.StablePtr
import Foreign.Marshal
import Foreign.Ptr
import Language.Haskell.TH
import Control.Monad
import Data.ByteString.Unsafe
import Foreign.Storable

-- | Export a Haskell function to be called by Swift.
--
-- This is similar to the @foreign export@ declaration, however, it
-- automatically creates a wrapper for the idiomatic Haskell function that
-- serializes inputs and outputs into buffers which are exposed as arguments of
-- a C-friendly function that is ultimately exposed.
--
-- @
-- data User
--  = User { birthYear :: Int
--         , age       :: Int
--         }
--         deriving stock    Generic
--         deriving anyclass FromJSON
--         deriving anyclass ToJSON
--
-- birthday :: User -> User
-- birthday User{age=x, birthYear=y} = User{age=x+1, birthYear=y}
--
-- $(foreignExportSwift 'birthday)
-- @
--
-- You likely want to use this function in conjunction with the Swift macro
-- @\@ForeignImportHaskell@ from the Swift side, which will handle
-- (de)serialization and calling the Haskell wrapper function.
--
-- Arguments and results are encoded in the following manner:
--  * StablePtr a is kept as a Stable pointer for both args and result
--  * Anything else is serialized to JSON
--    * Each argument corresponds to a buffer (char*) and its size (int)
--    * The result being serialized requires two additional args: a buffer to
--    put the result, and a pointer to the size of that buffer that is changed
--    to the required size
foreignExportSwift :: Name -> Q [Dec]
foreignExportSwift fun_name = do
  let wrapper_name_str = 'h':nameBase fun_name
      wrapper_name = mkName wrapper_name_str
  callresult_name <- newName "callresult"
  fun_ty     <- reifyType fun_name
  wrapper_ty <- makeWrapperTy fun_ty

  let
    fexp = ForeignD $ ExportF CCall wrapper_name_str wrapper_name wrapper_ty
    fsig = SigD wrapper_name wrapper_ty

  -- Vars of original function
  orgVars <- mapM (const $ newName "orig")
                  [1..tyFunArity fun_ty]

  -- Vars of the wrapper function
  -- We make no assumptions about how results are returned (serialized vs
  -- directly), we simply allocate names for every argument to the wrapper
  -- function, and take them on demand when constructing the function body.
  argVars <- mapM (const $ newName "wrap")
                  [1..tyFunArity wrapper_ty]

  fun <- FunD wrapper_name . (:[]) . (\b -> Clause (map VarP argVars) (NormalB b) []) . DoE Nothing <$> do
    buildBodyWithArgs argVars $ do
      binds <- zipWithM decodeArg orgVars (tyFunArgsTy fun_ty)
      let applyF = foldl AppE (VarE fun_name) (map VarE orgVars)
          resultBind
           | resultIsIO fun_ty = BindS (VarP callresult_name) applyF
           | otherwise = LetS [ValD (VarP callresult_name) (NormalB applyF) []]
      final <- encodeRes (tyFunResTy fun_ty) callresult_name
      return (binds++[resultBind]++[NoBindS final])
  return [fsig, fun, fexp]

--------------------------------------------------------------------------------
-- Foreign-wrapper builder monad
--------------------------------------------------------------------------------

-- | A monad that helps build the body of the foreign function wrapper that is
-- exposed.
--
-- In essence, the number of arguments the wrapper receives for each argument
-- of the original function and return result depend on the types of these
-- arguments, so, when constructing a result or reading an argument, we must
-- lazily take in order the amount of variables we expect to be available to
-- the function to convert that single argument from the list of function
-- arguments.
--
-- By the time we handled all arguments and result the list of allocated names
-- for the foreign function wrapper should be empty.
newtype BodyBuilder a = BodyBuilder (StateT [Name] Q a)
  deriving newtype (Functor, Applicative, Monad, MonadFail)

instance Quote BodyBuilder where
  newName = BodyBuilder . lift . newName

-- | Run a body building action with the given allocated foreign wrapper
-- functions.  By the time we are done body building, we should have used
-- exactly all allocated names.
buildBodyWithArgs :: [Name] -> BodyBuilder a -> Q a
buildBodyWithArgs names (BodyBuilder bb) = do
  (a, []) <- runStateT bb names
  return a

-- | Take @N@ arguments from the arguments allocated for the foreign wrapper function
needArgs :: Int -> BodyBuilder [Name]
needArgs n = BodyBuilder $ do
  needed <- gets (take n)
  modify (drop n)
  return needed

--------------------------------------------------------------------------------
-- Encoding and Decoding arguments
--------------------------------------------------------------------------------

-- | Decodes an argument from the wrapper functions and binds it to the name of
-- the original argument (to eventually pass to the function call that we are
-- wrapping)
decodeArg :: Name
          -- ^ The name to which the decoded argument should be bound
          -> Type
          -- ^ The type of the decoded argument
          -> BodyBuilder Stmt
decodeArg orig (AppT (ConT n) _a)
  | n == ''StablePtr
  = do
    -- Stable pointers are kept unchanged
    [stable_ptr] <- needArgs 1
    return $ LetS [ValD (VarP orig) (NormalB (VarE stable_ptr)) []]
decodeArg orig _ty
  = do
    -- Otherwise we decode using JSON
    [cstr, clen] <- needArgs 2
    BindS (VarP orig) <$> [| unsafePackCStringLen ($(varE cstr), $(varE clen)) >>= throwDecodeStrict |]


-- | Encodes the result from the wrapped function into the wrapper's return format.
-- For most data types, the result will be serialized into the final two arguments.
-- However, some, like 'StablePtr', will be simply returned.
encodeRes :: Type
          -- ^ The original function's return type
          -> Name
          -- ^ The name to which the result of evaluating the wrapped function is bound
          -> BodyBuilder Exp
encodeRes (stripTyIO -> AppT (ConT n) _b) callresult_name
  -- StablePtrs are simply returned
  | n == ''StablePtr
  = [| return $(varE callresult_name) |]
encodeRes _ callresult_name
  -- In all other cases try to serialize the result
  = do
  [buffer, sizeptr] <- needArgs 2
  [| unsafeUseAsCStringLen (toStrict $ encode $(varE callresult_name)) $ \(ptr,len) -> do
        size_avail <- peek $(varE sizeptr)
        -- Write actual size to intptr
        -- We always do this, either to see if we've overshot the buffer, or to
        -- know the size of what has been written.
        poke $(varE sizeptr) len
        if size_avail < len
           then do
             -- We need @len@ bytes available
             -- The caller has to retry
             return ()
           else do
             moveBytes $(varE buffer) ptr len
   |]

--------------------------------------------------------------------------------
-- Foreign exported types
--------------------------------------------------------------------------------

-- | Create the type of the function that will be exported from the type of the
-- original function which will be wrapped.
--
-- The implementation simply uses the 'ForeignTypeOf' type family to determine
-- the final foreign export signature. 
makeWrapperTy :: Type -> Q Type
makeWrapperTy ty = [t| ForeignTypeOf $(pure ty) |]

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

-- | ROMES:TODO: Still incomplete (e.g. ForallT)
tyFunArity :: Type -> Int
tyFunArity (AppT (AppT ArrowT _) b) = 1 + tyFunArity b
tyFunArity _ = 0

-- | Extract the types of each argument of the given function type into a list
tyFunArgsTy :: Type -> [Type]
tyFunArgsTy (AppT (AppT ArrowT a) b) = a : tyFunArgsTy b
tyFunArgsTy _ = []

-- | Extract the result type of the given function type
tyFunResTy :: Type -> Type
tyFunResTy (AppT (AppT ArrowT _) b) = tyFunResTy b
tyFunResTy x = x

-- | Remove an IO tycon from the given type. This is used to normalize the
-- return type before determining how to return it in the wrapper.
stripTyIO :: Type -> Type
stripTyIO (AppT (ConT n) t) | n == ''IO = t
stripTyIO t = t

resultIsIO :: Type -> Bool
resultIsIO (AppT (AppT ArrowT _) b) = resultIsIO b
resultIsIO (AppT (ConT n) _) | n == ''IO = True
resultIsIO _ = False

