{-# LANGUAGE CPP #-}
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
  )
  where

import Foreign.Swift.Marshal
import Foreign.Swift.Lib

import Control.Monad.State
import Foreign.Marshal
import Language.Haskell.TH
import Control.Monad
import Data.ByteString.Unsafe
import Foreign.Storable

import GHC.Tc.Gen.Splice () -- Quasi TcM

import GHC.Core.FamInstEnv
import GHC.Tc.Utils.Monad (TcM, TcLclEnv (..))
import Unsafe.Coerce (unsafeCoerce)
import qualified GHC.Tc.Instance.Family as TcM
import GHC.ThToHs (convertToHsType)
import GHC.Types.SrcLoc (generatedSrcSpan)
import GHC.Types.Basic (DoPmc(DoPmc), GenReason (OtherExpansion), Origin (Generated))
import GHC.Rename.HsType (rnLHsType)
import qualified GHC.Utils.Outputable as Ppr
import GHC.Tc.Errors.Types (HsDocContext(GenericCtx))
#if __GLASGOW_HASKELL__ >= 912
import GHC.Tc.Gen.HsType (tcCheckLHsTypeInContext, ContextKind (..))
#else
import GHC.Tc.Gen.HsType (tcCheckLHsType, ContextKind (..))
#endif
import qualified GHC.Tc.Utils.TcType as Core
import qualified GHC.Core.TyCo.Rep as Core
import Data.Bifunctor (Bifunctor(..))
import GHC.Core.Coercion (Role(Nominal))
import GHC.Core.Reduction (Reduction(..))
import GHC.Types.Name (getOccString)
import qualified GHC.Tc.Utils.Monad as TcM
import Data.IORef

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
  origin_ty <- reifyType fun_name
  (foreign_ty, normalized_ty) <- makeWrapperTy origin_ty

  let
    fexp = ForeignD $ ExportF CCall wrapper_name_str wrapper_name foreign_ty
    fsig = SigD wrapper_name foreign_ty

  let
      -- to make arg variables for the wrapper
      (wrapperArgsTy, _wrapperResTy) = tyFunSplitCoreTy normalized_ty
      wrapperTyArity  = length wrapperArgsTy

      (origArgsTy, origResTy) = tyFunSplitTy origin_ty
      origTyArity  = length origArgsTy

  -- Vars of original function
  orgVars <- mapM (const $ newName "orig")
                  [1..origTyArity]

  -- Vars of the wrapper function
  -- We make no assumptions about how results are returned (serialized vs
  -- directly), we simply allocate names for every argument to the wrapper
  -- function, and take them on demand when constructing the function body.
  wrapperVars <- mapM (const $ newName "wrap")
                  [1..wrapperTyArity]

  fun <- FunD wrapper_name . (:[]) . (\b -> Clause (map VarP wrapperVars) (NormalB b) []) . DoE Nothing <$> do
    buildBodyWithArgs wrapperVars $ do
      binds <- zipWithM decodeArg orgVars origArgsTy
      let applyF = foldl AppE (VarE fun_name) (map VarE orgVars)
          resultBind = BindS (VarP callresult_name) applyF
      final <- encodeRes origResTy callresult_name
      return (binds++[resultBind]++[NoBindS final])

  gens <- genSwiftActionAndAnn (yieldFunction (origArgsTy, origResTy)) fun_name (dropResultIO origin_ty) [|| ExportSwiftFunction ||]

  return ([fsig, fun, fexp] ++ gens)

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

liftBB :: Q a -> BodyBuilder a
liftBB = BodyBuilder . lift

instance Quote BodyBuilder where
  newName = liftBB . newName

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
decodeArg orig ty = do
  liftBB (foreignArgKind ty) >>= \case
    PtrKind -> do
      -- Stable pointers
      [stable_ptr] <- needArgs 1
      BindS (VarP orig) <$> [| fromSwift $(varE stable_ptr) |]
    JSONKind -> do
      -- Decode using JSON
      [cstr, clen] <- needArgs 2
      BindS (VarP orig) <$> [| unsafePackCStringLen ($(varE cstr), $(varE clen)) >>= fromSwift |]


-- | Encodes the result from the wrapped function into the wrapper's return format.
-- For most data types, the result will be serialized into the final two arguments.
-- However, some, like 'StablePtr', will be simply returned.
encodeRes :: Type
          -- ^ The original function's return type
          -> Name
          -- ^ The name to which the result of evaluating the wrapped function is bound
          -> BodyBuilder Exp
encodeRes ty callresult_name = do
  liftBB (foreignResultKind ty) >>= \case
    PtrKind ->
      -- StablePtrs are simply returned
      [| toSwift $(varE callresult_name) |]
    JSONKind -> do
      -- In all other cases try to serialize the result
      [buffer, sizeptr] <- needArgs 2
      [| (toSwift $(varE callresult_name) >>=) $ \bs -> unsafeUseAsCStringLen bs $ \(ptr,len) -> do
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
makeWrapperTy :: Type -> Q (Type, Core.Type)
makeWrapperTy ty = do
  fty <- [t| ForeignTypeOf $(pure ty) |]
  normal_fty <- normaliseTy fty
  return (fty, normal_fty)

foreignArgKind :: Type -> Q ForeignValKind
foreignArgKind ty = foreignValKind [t| ForeignArgKind |] ty

foreignResultKind :: Type -> Q ForeignValKind
foreignResultKind (stripTyIO -> ty) = foreignValKind [t| ForeignResultKind |] ty

foreignValKind :: Q Type -> Type -> Q ForeignValKind
foreignValKind con ty = do
  nty <- normaliseTy =<< [t| $con $(pure ty) |]
  case nty of
    -- I think result is always headed by cast bc kind /= *
    Core.CastTy (Core.TyConApp tc _) _co
      -> do
        if      getOccString tc == "JSONKind" then return JSONKind
        else if getOccString tc == "PtrKind"  then return PtrKind
        else                                           error "unexpected kind"

    _ -> error "Unexpected, but we could default to JSONKind"

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

-- | Normalise a TH Type to a Core Type
--
-- If at some point we can't do this directly, maybe we can have a plugin
-- inject some information into the splices or just generate this itself. Glad
-- we can for now though!
normaliseTy :: Type -> Q (Core.Type)
normaliseTy ty = unsafeRunTcM $ do
  -- Important: save Wanted to restore them because this tcChecking introduces
  -- bad coercion variables
  TcLclEnv{tcl_lie} <- TcM.getLclEnv
  savedWanted <- liftIO $ readIORef tcl_lie

  env <- TcM.tcGetFamInstEnvs
#if __GLASGOW_HASKELL__ >= 913
  case convertToHsType mempty generatedOrigin generatedSrcSpan ty of
#else
  case convertToHsType generatedOrigin generatedSrcSpan ty of
#endif
    Left _ -> fail "convertToHsType failed during swift-ffi normalisation"
    Right hsTyPs -> do
      (hsTyRn, _) <- rnLHsType (GenericCtx (Ppr.text "foreign.swift")) hsTyPs
#if __GLASGOW_HASKELL__ >= 912
      hsTy <- tcCheckLHsTypeInContext hsTyRn OpenKind
#else
      hsTy <- tcCheckLHsType hsTyRn OpenKind
#endif
      let Reduction{reductionReducedType} = normaliseType env Nominal hsTy

      liftIO $ writeIORef tcl_lie savedWanted

      return reductionReducedType
  where
      generatedOrigin :: Origin
      generatedOrigin = Generated OtherExpansion DoPmc

-- | Right...
unsafeRunTcM :: TcM a -> Q a
unsafeRunTcM m = unsafeCoerce (\_ -> m)

-- | Extract the types of each argument of the given function type into a list and the result type separate
tyFunSplitTy :: Type -> ([Type], Type)
tyFunSplitTy (AppT (AppT ArrowT a) b) = first (a:) (tyFunSplitTy b)
tyFunSplitTy (ForallT _ _ c) = tyFunSplitTy c -- ghmm..
tyFunSplitTy r = ([], r)

tyFunSplitCoreTy :: Core.Type -> ([Core.Type], Core.Type)
tyFunSplitCoreTy Core.FunTy{Core.ft_arg, Core.ft_res} = first (ft_arg:) (tyFunSplitCoreTy ft_res)
tyFunSplitCoreTy (Core.ForAllTy _ c) = tyFunSplitCoreTy c
tyFunSplitCoreTy r = ([], r)

--- | Remove an IO tycon from the given type
stripTyIO :: Type -> Type
stripTyIO (AppT (ConT n) t) | n == ''IO = t
stripTyIO t = t

-- | Drop `IO` from the result type.
-- We need this because ToMoatType doesn't have an IO instance and we only care
-- that on the swift side we get the right result type (without IO)
dropResultIO :: Type -> Type
dropResultIO (AppT (AppT ArrowT a) b) = AppT (AppT ArrowT a) (dropResultIO b)
dropResultIO (AppT (ConT n) t)
  | n == ''IO = t
  | otherwise = AppT (ConT n) t
dropResultIO t = t
