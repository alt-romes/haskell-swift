{-# OPTIONS_GHC -Wno-orphans #-} -- ToMoatTy (IO a)
{-# LANGUAGE MagicHash #-}
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
import Moat (ToMoatType(..), MoatType (..), prettyMoatType)
import Control.Monad
import Data.Proxy
import Data.Functor.Identity (Identity(..))
import Control.Exception
import Data.ByteString.Unsafe (unsafePackCStringLen, unsafeUseAsCStringLen)
import qualified Data.Monoid as Monoid
import Control.Monad.Writer
import GHC.Exts (unsafeCoerce#)

class ToMoatType a => ToSwift a where
  -- | The FFI return type for a foreign exported function returning @a@ 
  -- TODO: No, how can I get rid of this and the other type family and also of
  -- 'decodeArg' and 'encodeRes'?? those are making tying the Kind to the
  -- expected representation way too much
  type FFIResult a :: Type

  -- | How to convert the @a@ from the original Haskell function into the
  -- foreign wrapper's result type?
  toSwift :: IO a -> FFIResult a

  -- | The dual side. 'fromHaskell' returns the Swift code that will be
  -- inserted in the Swift foreign wrapper which reads a result from Haskell
  -- into the matching Swift type.
  fromHaskell :: Proxy a
              -> String -- ^ Result name
              -> ([String] {-^ Args to add to the foreign call -} -> SwiftCodeGen String) -- ^ Continuation
              -> SwiftCodeGen String -- ^ Result is code to decode result plus the code returned by the continuation

class ToMoatType a => FromSwift a where

  -- | Attach an FFI argument that can be decoded to @a@ using 'fromSwift' to
  -- the continuation type
  type FFIArg a r :: Type

  -- | Marshal a Swift value from the FFI to the original function argument
  -- type @a@, wrapped in IO, and pass it to the continuation.
  fromSwift :: forall r. (IO a -> r) -> FFIArg a r

  -- | 'toHaskell' returns the Swift code that will be
  -- inserted in the Swift foreign wrapper which encodes a result from Swift
  -- into the Haskell foreign exported function
  toHaskell :: Proxy a
            -> String -- ^ Argument name
            -> ([String] {-^ Foreign call arguments added by this argument -} -> SwiftCodeGen String) -- ^ Continuation takes encoded arguments
            -> SwiftCodeGen String -- ^ Result is code to encode arguments plus the code returned by the continuation

--------------------------------------------------------------------------------
-- * Producing Swift code that encodes/decodes values at boundary
--------------------------------------------------------------------------------

newtype SwiftCodeGen a = SwiftCodeGen { getSwiftCodeGen :: (a, Monoid.Any) }
  deriving (Functor, Applicative, Monad, MonadWriter Monoid.Any) via Writer Monoid.Any

-- | Get the variable name for the shared JSON encoder
-- (One only gets produced if it is used in the code)
getEncoder :: SwiftCodeGen String
getEncoder = return "hs_enc" -- for now, always generate the encoder and decoder

-- | Like 'getEncoder' but for a @JSONDecoder@
getDecoder :: SwiftCodeGen String
getDecoder = return "hs_dec" -- bit wasteful, but let's get this working first.

-- | Marks the Swift function with @throws(HaskellException)@
registerThrowsHsException :: SwiftCodeGen ()
registerThrowsHsException = tell (Monoid.Any True)

--------------------------------------------------------------------------------
-- * Deriving via via TH
--------------------------------------------------------------------------------

-- | For choosing how to generate derive instances when using 'swiftMarshal'
data ForeignValKind
  = JSONKind
  | PtrKind
  deriving Eq

-- | Derive a @'ToSwift'@ and @'FromSwift'@ instance.
swiftMarshal :: ForeignValKind -> TH.Name -> TH.Q [TH.Dec]
swiftMarshal kind name = do
  let
      viaTy = \case
        JSONKind -> [t| $(TH.conT ''JSONMarshal) $myty |]
        PtrKind  -> [t| $(TH.conT ''PtrMarshal) $myty |]
      myty  = TH.conT name

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

-- | A newtype to refer to a value via its pointer.
--
-- Used for deriving-via ToSwift/FromSwift instances using stable pointers
-- @
-- deriving via (PtrMarshal MyType) instance ToSwift MyType
-- deriving via (PtrMarshal MyType) instance FromSwift MyType
-- @
newtype PtrMarshal a = PtrMarshal a
  deriving newtype (ToMoatType)

-- | A newtype to marshal a value using JSON
--
-- Used for deriving-via ToSwift/FromSwift instances
-- @
-- deriving via (JSONMarshal MyType) instance ToSwift MyType
-- deriving via (JSONMarshal MyType) instance FromSwift MyType
-- @
newtype JSONMarshal a = JSONMarshal a
  deriving newtype (ToJSON, FromJSON, ToMoatType)

-- | A newtype to wrap a ToSwift value which causes all exceptions to be
-- caught on the Haskell side and thrown as Swift exceptions.
--
-- @
-- -- All IO exceptions thrown in f are caught in the foreign export
-- f :: (FromSwift a, ToSwift b) => a -> CatchExceptionsFFI IO b
-- @
newtype CatchFFI io a = CatchFFI (io a)
  deriving newtype (ToMoatType)

--------------------------------------------------------------------------------
-- JSONMarshal
--------------------------------------------------------------------------------

instance (ToMoatType a, ToJSON a) => ToSwift (JSONMarshal a) where
  type FFIResult (JSONMarshal a) = Ptr CChar -> Ptr Int -> IO ()

  toSwift result buffer sizeptr = flip catch handleCEFFI do
    result_bs <- BS.toStrict . encode <$> result
    unsafeUseAsCStringLen result_bs $ \(ptr,len) -> do
      size_avail <- peek sizeptr
      -- Write actual size to intptr
      -- We always do this, either to see if we've overshot the buffer, or to
      -- know the size of what has been written.
      poke sizeptr len
      if size_avail < len
         then do
           -- We need @len@ bytes available
           -- The caller has to retry
           return ()
         else do
           moveBytes buffer ptr len
    where
      handleCEFFI (CatchExceptionsFFICaught _) = do
        -- dummy results; we won't look bc CatchExceptionsFFICaught means Swift will check for it.
        poke buffer maxBound
        poke sizeptr (-1)

  fromHaskell _ _r{-result value is ignored as the result is read from passed buffers-} getCont = do
    decoder <- getDecoder
    let res_ptr  = "res_ptr"
    let size_ptr = "size_ptr"
    -- print("Read JSON from Haskell: \\(String(bytes: new_data, encoding: .utf8) ?? "???")")
    let decodeAndReturnResult = [__i|
          let new_data = Data(bytesNoCopy: #{res_ptr}.baseAddress!, count: #{size_ptr}.baseAddress?.pointee ?? 0, deallocator: .none)
          do {
            return try #{decoder}.decode(#{prettyMoatType (toMoatType (Proxy @a))}.self, from: new_data)
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

instance (ToMoatType a, FromJSON a) => FromSwift (JSONMarshal a) where
  type FFIArg (JSONMarshal _) r = Ptr CChar -> Int -> r

  fromSwift :: (IO (JSONMarshal a) -> r) -> (Ptr CChar -> Int -> r)
  fromSwift k cstr clen =
    k (unsafePackCStringLen (cstr, clen) >>= throwDecodeStrict)

  toHaskell _ v getCont = do
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

instance ToMoatType a => ToSwift (PtrMarshal a) where

  type FFIResult (PtrMarshal a) = IO (StablePtr a)

  toSwift io = flip catch (\(CatchExceptionsFFICaught _) -> pure (castPtrToStablePtr nullPtr)) $ do
    (PtrMarshal a) <- io
    ptr <- newStablePtr a
    pure $ coerce ptr

  fromHaskell _ r getCont = do
    -- For a StablePtr, simply return the result of the foreign call
    cont <- getCont []
    return [__i|
      #{cont}
      return #{r}!
    |]

instance ToMoatType a => FromSwift (PtrMarshal a) where
  type FFIArg (PtrMarshal a) r = StablePtr a -> r

  fromSwift :: (IO (PtrMarshal a) -> r) -> StablePtr a -> r
  fromSwift k p = k $ do
    ptr <- deRefStablePtr p
    pure $ coerce ptr

  toHaskell _ v getCont = do
    cont <- getCont [v] -- StablePtr arguments are passed directly to the foreign call
    return cont

--------------------------------------------------------------------------------
-- Catch exceptions
--------------------------------------------------------------------------------

instance (ToMoatType a, ToSwift a) => ToSwift (CatchFFI IO a) where
  type FFIResult (CatchFFI IO a) =
        Ptr (StablePtr SomeException) {-^ nullptr if no exception -} -> FFIResult a

  toSwift :: IO (CatchFFI IO a) -> Ptr (StablePtr SomeException) -> FFIResult a
  toSwift io_io_a exceptptr =
    toSwift @a $ do
      CatchFFI io_a <- io_io_a
      r <- (Right <$> io_a) `catch` (\(e :: SomeException) -> pure (Left e))
      case r of
        Left e -> do
          sptre <- newStablePtr e
          poke exceptptr sptre
          throwIO (CatchExceptionsFFICaught e)
        Right x -> do
          poke exceptptr (unsafeCoerce# 0x0#)
          return x

  -- pass an exception ptr and make sure to check it before calling fromHaskell on the proper result
  fromHaskell _ r getCont = do
    registerThrowsHsException
    fromHaskell (Proxy @a) r $ \args -> do
      cont <- getCont $ ["except_ptr.baseAddress"] ++ args
      return [__i|
        try withUnsafeTemporaryAllocation(of: UnsafeMutableRawPointer.self, capacity: 1) { except_ptr in
      #{indent 4 cont}
          if let exception_result = except_ptr.baseAddress?.pointee {
            // Exception result is a non-nil pointer, therefore it points to an exception
            throw HaskellException.exception("\\(exception_result)")
          }
        }
      |]

-- | When an exception is caught by 'CatchFFI, we write the 'Ptr' to
-- the exception and then throw from the IO action a 'CatchExceptionsFFICaught'
-- exception. The 'toSwift' base instances should catch this exception and
-- return a dummy value in the FFI.
-- The generated Swift code is guaranteed to look at the 'Ptr' to exception and
-- check if it is null before trying to read the actual result.
newtype CatchExceptionsFFICaught = CatchExceptionsFFICaught SomeException
  deriving (Show, Exception)

--------------------------------------------------------------------------------
-- ToSwift/FromSwift base instances
--------------------------------------------------------------------------------

instance (ToMoatType b, ToSwift b) => ToSwift (IO b) where
  type FFIResult (IO b) = FFIResult b
  toSwift = toSwift . join

  fromHaskell = error "todo: as below:"

instance (FromSwift a, ToSwift b) => ToSwift (a -> b) where
  type FFIResult (a -> b) = FFIArg a (FFIResult b)

  toSwift iof =
    fromSwift @a @(FFIResult b) $ \ioa ->
      toSwift @b $ do
        f <- iof
        a <- ioa
        return $ f a

  fromHaskell _ _ _ = error "Still done in 'yieldFunction; we should never reach this'"
  --   toHaskell (Proxy @b) _ _
  --   fromHaskell (Proxy @a) _ _
  --   _

-- Lists
deriving via (JSONMarshal [a]) instance (ToMoatType a, ToJSON a) => ToSwift [a]
deriving via (JSONMarshal [a]) instance (ToMoatType a, FromJSON a) => FromSwift [a]

-- SomeException
-- deriving via (PtrMarshal SomeException) instance ToSwift SomeException
-- deriving via (PtrMarshal SomeException) instance FromSwift SomeException

-- | Treat an IO wrapped type transparently as the underlying type
instance ToMoatType a => ToMoatType (IO a) where
  toMoatType _ = toMoatType (Proxy @a)

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines
