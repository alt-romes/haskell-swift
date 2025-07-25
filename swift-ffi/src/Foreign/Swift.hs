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
  , deriveJSON, aesonDefaultOptions
  )
  where

import Foreign.Swift.Marshal
import Foreign.Swift.Lib

import Control.Monad.State
import Language.Haskell.TH

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
import GHC.Core.Coercion (Role(Nominal))
import GHC.Core.Reduction (Reduction(..))
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
  origin_ty <- reifyType fun_name
  foreign_ty <- makeWrapperTy origin_ty

  let
    fexp = ForeignD $ ExportF CCall wrapper_name_str wrapper_name foreign_ty
    fsig = SigD wrapper_name foreign_ty

  wrapper_body <- [| toSwift (pure $(varE fun_name)) |]
  let fun = FunD wrapper_name ([Clause [] (NormalB wrapper_body) []])

  gens <- genSwiftActionAndAnn (yieldFunction origin_ty (nameBase fun_name) wrapper_name_str) fun_name [|| ExportSwiftFunction ||]

  return ([fsig, fun, fexp] ++ gens)

--------------------------------------------------------------------------------
-- Foreign exported types
--------------------------------------------------------------------------------

-- | Create the type of the function that will be exported from the type of the
-- original function which will be wrapped.
--
-- The implementation simply uses the 'ForeignTypeOf' type family to determine
-- the final foreign export signature. 
makeWrapperTy :: Type -> Q (Type {-, Core.Type -})
makeWrapperTy ty = do
  fty <- [t| FFIResult $(pure ty) |]
  -- normal_fty <- normaliseTy fty
  return (fty {-, normal_fty -})

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

-- -- | Right...
unsafeRunTcM :: TcM a -> Q a
unsafeRunTcM m = unsafeCoerce (\_ -> m)

