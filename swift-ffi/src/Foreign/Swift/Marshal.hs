{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
module Foreign.Swift.Marshal where

import qualified Language.Haskell.TH as TH
import qualified Data.ByteString as BS
import Foreign
import Data.Aeson
import Data.Coerce (coerce)
import Data.Kind (Type)
import Foreign.C

import Foreign.Swift.Utils
import Moat (ToMoatType)
import Control.Monad

class ToSwift a where
  type ForeignResultKind a :: ForeignValKind

  -- | We care about what the literal type used for the interface should be in
  -- the case when @ForeignValKind != JSONKind@
  type FFIResultLit a :: Type

  toSwift :: a -> IO (FFIResultLit a)

class FromSwift a where

  type ForeignArgKind a :: ForeignValKind

  -- | We care about what the literal type used for the interface should be in
  -- the case when @ForeignValKind != JSONKind@
  type FFIArgLit a :: Type

  fromSwift :: FFIArgLit a -> IO a

data ForeignValKind = JSONKind | PtrKind
  deriving Eq

--------------------------------------------------------------------------------
-- * Deriving via via TH
--------------------------------------------------------------------------------

-- | Derive a @'ToSwift'@ and @'FromSwift'@ instance.
--
-- In the case of deriving with @'JSONKind'@, a JSON instance will be derived if one isn't found.
swiftMarshal :: ForeignValKind -> TH.Name -> TH.Q [TH.Dec]
swiftMarshal kind name = do
  let newty = case kind of
        JSONKind -> pure $ TH.ConT ''JSONMarshal
        PtrKind  -> pure $ TH.ConT ''PtrMarshal
      myty  = pure $ TH.ConT name

  satTy <- getSaturatedType name

  when (kind == JSONKind) $ do
    hasMoatTy <- TH.isInstance ''ToMoatType [satTy]
    when (not hasMoatTy) $ do
      fail $
        "Marshaled JSON type doesn't have a Moat instance. Did you forget to derive and yield the Data for "
          ++ TH.pprint (TH.ConT name) ++ "?"

  dvs <- [d| deriving via ($newty $myty) instance (ToSwift $myty)
             deriving via ($newty $myty) instance (FromSwift $myty)
           |]

  return dvs

--------------------------------------------------------------------------------
-- * Construct foreign interface type
--------------------------------------------------------------------------------

type family ForeignTypeOf (a :: Type) :: Type where
  ForeignTypeOf (b -> c) = AddArg (ForeignArgKind b) b (ForeignTypeOf c)
  ForeignTypeOf (IO a)   = AddResult (ForeignResultKind a) a
  ForeignTypeOf a        = AddResult (ForeignResultKind a) a

-- See notes about foreign interface with marshaling in "Calling Haskell from Swift" (in my blog)
type family AddArg (a :: ForeignValKind) (b :: Type) (c :: Type) :: Type where
  AddArg JSONKind _ c = Ptr CChar -> Int -> c
  AddArg PtrKind  b c = FFIArgLit b -> c
type family AddResult (a :: ForeignValKind) (b :: Type) :: Type where
  AddResult JSONKind b = Ptr CChar -> Int -> IO ()
  AddResult PtrKind  b = IO (FFIResultLit b)

--------------------------------------------------------------------------------

-- | A newtype to refer to a value via its pointer.
--
-- Used for deriving-via ToSwift/FromSwift instances using stable pointers
-- @
-- deriving via (PtrMarshal MyType) instance ToSwift MyType
-- deriving via (PtrMarshal MyType) instance FromSwift MyType
-- @
newtype PtrMarshal a = PtrMarshal a

-- | A newtype to marshal a value using JSON
--
-- Used for deriving-via ToSwift/FromSwift instances
-- @
-- deriving via (JSONMarshal MyType) instance ToSwift MyType
-- deriving via (JSONMarshal MyType) instance FromSwift MyType
-- @
newtype JSONMarshal a = JSONMarshal a
  deriving newtype (ToJSON, FromJSON)

--------------------------------------------------------------------------------

instance ToJSON a => ToSwift (JSONMarshal a) where
  type ForeignResultKind (JSONMarshal a) = JSONKind
  type FFIResultLit      (JSONMarshal a) = BS.ByteString
  toSwift = pure . BS.toStrict . encode

instance FromJSON a => FromSwift (JSONMarshal a) where
  type ForeignArgKind (JSONMarshal a) = JSONKind
  type FFIArgLit      (JSONMarshal a) = BS.ByteString
  fromSwift b = throwDecodeStrict b

instance ToSwift (PtrMarshal a) where
  type ForeignResultKind (PtrMarshal a) = PtrKind
  type FFIResultLit      (PtrMarshal a) = StablePtr a
  toSwift (PtrMarshal a) = do
    ptr <- newStablePtr a
    pure $ coerce ptr

instance FromSwift (PtrMarshal a) where
  type ForeignArgKind (PtrMarshal a) = PtrKind
  type FFIArgLit      (PtrMarshal a) = StablePtr a
  fromSwift p = do
    ptr <- deRefStablePtr p
    pure $ coerce ptr

-- Lists
deriving via (JSONMarshal [a]) instance ToJSON a => ToSwift [a]
deriving via (JSONMarshal [a]) instance FromJSON a => FromSwift [a]
