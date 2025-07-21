{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module Foreign.Swift.Marshal where

import qualified Language.Haskell.TH as TH
import qualified Data.ByteString as BS
import Foreign
import Data.Aeson
import Data.Coerce (coerce)
import Data.Kind (Type)
import Foreign.C

import Data.String.Interpolate
-- import Control.Monad.Tardis (almost got to use it on the continuation shenanigans, but it looks better pure...)
import Foreign.Swift.Utils
import Moat (ToMoatType, MoatType, prettyMoatType)
import Control.Monad
import Data.Proxy
import Data.Functor.Identity (Identity(..))
import Control.Exception

class ToSwift a where
  type ForeignResultKind a :: ForeignValKind
  type FFIResultLit a :: Type
  toSwift :: a -> IO (FFIResultLit a)

  -- | The dual side. 'fromHaskell' returns the Swift code that will be
  -- inserted in the Swift foreign wrapper which reads a result from Haskell
  -- into the matching Swift type.
  fromHaskell :: Proxy a
              -> MoatType -- ^ MoatType for this result
              -> String -- ^ Result name
              -> ([String] {-^ Args to add to the foreign call -} -> SwiftCodeGen String) -- ^ Continuation
              -> SwiftCodeGen String -- ^ Result is code to decode result plus the code returned by the continuation

class FromSwift a where
  type ForeignArgKind a :: ForeignValKind
  type FFIArgLit a :: Type
  fromSwift :: FFIArgLit a -> IO a

  -- | 'toHaskell' returns the Swift code that will be
  -- inserted in the Swift foreign wrapper which encodes a result from Swift
  -- into the Haskell foreign exported function
  toHaskell :: Proxy a
            -> MoatType -- ^ MoatType for this arg
            -> String -- ^ Argument name
            -> ([String] {-^ Foreign call arguments added by this argument -} -> SwiftCodeGen String) -- ^ Continuation takes encoded arguments
            -> SwiftCodeGen String -- ^ Result is code to encode arguments plus the code returned by the continuation

data ForeignValKind
  = JSONKind
  | PtrKind
  deriving Eq

--------------------------------------------------------------------------------
-- * Producing Swift code that encodes/decodes values at boundary
--------------------------------------------------------------------------------

newtype SwiftCodeGen a = SwiftCodeGen { getSwiftCodeGen :: a }
  deriving (Functor, Applicative, Monad) via Identity
    -- currently it does nothing...!

-- | Get the variable name for the shared JSON encoder
-- (One only gets produced if it is used in the code)
getEncoder :: SwiftCodeGen String
getEncoder = return "hs_enc" -- for now, always generate the encoder and decoder

-- | Like 'getEncoder' but for a @JSONDecoder@
getDecoder :: SwiftCodeGen String
getDecoder = return "hs_dec" -- bit wasteful, but let's get this working first.

--------------------------------------------------------------------------------
-- * Deriving via via TH
--------------------------------------------------------------------------------

-- | Derive a @'ToSwift'@ and @'FromSwift'@ instance.
swiftMarshal :: ForeignValKind -> TH.Name -> TH.Q [TH.Dec]
swiftMarshal kind name = do
  let
      viaTy = \case
        JSONKind -> [t| $(TH.conT ''JSONMarshal) $myty |]
        PtrKind  -> [t| $(TH.conT ''PtrMarshal) $myty |]
      myty  = pure $ TH.ConT name

  satTy <- getSaturatedType name

  when (kind == JSONKind) $ do
    -- Ensure there's a MoatType instance (helping ensure the datatypes were
    -- yielded to .swift files)
    hasMoatTy <- TH.isInstance ''ToMoatType [satTy]
    when (not hasMoatTy) $ do
      fail $
        "Marshaled JSON type doesn't have a Moat instance. Did you forget to derive and yield the Data for "
          ++ TH.pprint (TH.ConT name) ++ "?"

  dvs <- [d| deriving via $(viaTy kind) instance (ToSwift $myty)
             deriving via $(viaTy kind) instance (FromSwift $myty)
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
  AddResult JSONKind b = Ptr CChar -> Ptr Int -> IO ()
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

-- | A newtype to wrap a ToSwift value which causes all exceptions to be
-- caught on the Haskell side and thrown as Swift exceptions.
--
-- @
-- -- All IO exceptions thrown in f are caught in the foreign export
-- f :: (FromSwift a, ToSwift b) => a -> CatchExceptionsFFI IO b
-- @
newtype CatchExceptionsFFI io a = CatchExceptionsFFI (io a)

--------------------------------------------------------------------------------
-- JSONMarshal
--------------------------------------------------------------------------------

instance ToJSON a => ToSwift (JSONMarshal a) where
  type ForeignResultKind (JSONMarshal a) = JSONKind
  type FFIResultLit      (JSONMarshal a) = BS.ByteString
  toSwift = pure . BS.toStrict . encode
  fromHaskell _ retType _r{-result value is ignored as the result is read from passed buffers-} getCont = do
    decoder <- getDecoder
    let res_ptr  = "res_ptr"
    let size_ptr = "size_ptr"
    -- print("Read JSON from Haskell: \\(String(bytes: new_data, encoding: .utf8) ?? "???")")
    let decodeAndReturnResult = [__i|
          let new_data = Data(bytesNoCopy: #{res_ptr}.baseAddress!, count: #{size_ptr}.baseAddress?.pointee ?? 0, deallocator: .none)
          do {
            return try #{decoder}.decode(#{prettyMoatType retType}.self, from: new_data)
          } catch let err {
            throw HsFFIError.decodingFailed(String(decoding: new_data, as: UTF8.self), err)
          }
        |] :: String
    cont <- getCont [res_ptr++".baseAddress", size_ptr++".baseAddress"]
    return [__i|
      // Allocate buffer for result and allocate a pointer to an int with the initial size of the buffer
      let buf_size = 1024000
      
      return try withUnsafeTemporaryAllocation(of: Int.self, capacity: 1) { size_ptr in
          #{size_ptr}.baseAddress?.pointee = buf_size
          
          do {
              return try withUnsafeTemporaryAllocation(byteCount: buf_size, alignment: 1) { #{res_ptr} in
                  
      #{indent 12 cont}
                  
                  if let required_size = #{size_ptr}.baseAddress?.pointee {
                      if required_size > buf_size {
                          throw HsFFIError.requiredSizeIs(required_size)
                      }
                  }
          
      #{indent 12 decodeAndReturnResult}
              }
          } catch HsFFIError.requiredSizeIs(let required_size) {
              print("Retrying with required size: \\(required_size)")
              return try withUnsafeTemporaryAllocation(byteCount: required_size, alignment: 1) { #{res_ptr} in
                  #{size_ptr}.baseAddress?.pointee = required_size
                  
      #{indent 12 cont}
      #{indent 12 decodeAndReturnResult}
              }
          }
      }
    |]

instance FromJSON a => FromSwift (JSONMarshal a) where
  type ForeignArgKind (JSONMarshal a) = JSONKind
  type FFIArgLit      (JSONMarshal a) = BS.ByteString
  fromSwift b = throwDecodeStrict b
  toHaskell _ _ty v getCont = do
    let v_data    = v ++ "_data"
    let v_datalen = v ++ "_datalen"
    let v_ptr     = v ++ "_ptr"
    enc <- getEncoder
    cont <- getCont [v_ptr++".baseAddress", v_datalen]
    return [__i|
      var #{v_data} = try #{enc}.encode(#{v})
      let #{v_datalen} = Int64(#{v_data}.count)
      return try #{v_data}.withUnsafeMutableBytes { (#{v_ptr}:UnsafeMutableRawBufferPointer) in
    #{indent 4 cont}
      }
    |]

--------------------------------------------------------------------------------
-- PtrMarshal
--------------------------------------------------------------------------------

instance ToSwift (PtrMarshal a) where
  type ForeignResultKind (PtrMarshal a) = PtrKind
  type FFIResultLit      (PtrMarshal a) = StablePtr a
  toSwift (PtrMarshal a) = do
    ptr <- newStablePtr a
    pure $ coerce ptr
  fromHaskell _ _ty r getCont = do
    -- For a StablePtr, simply return the result of the foreign call
    cont <- getCont []
    return [__i|
      #{cont}
      return #{r}!
    |]

instance FromSwift (PtrMarshal a) where
  type ForeignArgKind (PtrMarshal a) = PtrKind
  type FFIArgLit      (PtrMarshal a) = StablePtr a
  fromSwift p = do
    ptr <- deRefStablePtr p
    pure $ coerce ptr
  toHaskell _ _ty v getCont = do
    cont <- getCont [v] -- StablePtr arguments are passed directly to the foreign call
    return cont

--------------------------------------------------------------------------------
-- CatchExceptionsMarshal
--------------------------------------------------------------------------------

instance ToSwift a => ToSwift (CatchExceptionsFFI IO a) where
  type ForeignResultKind (CatchExceptionsFFI IO a) = -- ForeignResultKind (Either SomeException a) ?
  type FFIResultLit      (CatchExceptionsFFI IO a) = -- FFIResultLit (Either a ?
  toSwift (CatchExceptionsFFI a) = do
    r <- (Right <$> a) `catch` (\(e :: SomeException) -> pure (Left e))
    toSwift r
  fromHaskell _ ty r getCont =
    fromHaskell (Proxy @(Either SomeException a)) ty r getCont 

instance (ToSwift a, ToSwift b) => ToSwift (Either a b)

deriving via (PtrMarshal SomeException) instance ToSwift SomeException
deriving via (PtrMarshal SomeException) instance FromSwift SomeException

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

deriving via (JSONMarshal [a]) instance ToJSON a => ToSwift [a]
deriving via (JSONMarshal [a]) instance FromJSON a => FromSwift [a]

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines
