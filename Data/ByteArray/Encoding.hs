-- |
-- Module      : Data.ByteArray.Encoding
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Base conversions for 'ByteArray'.
--
module Data.ByteArray.Encoding
    ( convertToBase
    , convertFromBase
    , Base(..)
    ) where

import           Data.ByteArray.Types
import qualified Data.ByteArray.Types        as B
import qualified Data.ByteArray.Methods      as B
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base16      as B16
import qualified Data.ByteString.Base32      as B32
import qualified Data.ByteString.Base64      as B64
import qualified Data.ByteString.Base64.URL  as B64URL
import           Data.Base16.Types           (extractBase16)
import           Data.Base64.Types           (extractBase64)
import qualified Data.Text                   as T
import           Data.Memory.Encoding.Base64 (toBase64OpenBSD, fromBase64OpenBSD,
                                              unBase64LengthUnpadded)
import           Data.Memory.Internal.Compat (unsafeDoIO)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.ByteString

-- | The different bases that can be used.
--
-- See <http://tools.ietf.org/html/rfc4648 RFC4648> for details.
-- In particular, Base64 can be standard or
-- <http://tools.ietf.org/html/rfc4648#section-5 URL-safe>. URL-safe
-- encoding is often used in other specifications without
-- <http://tools.ietf.org/html/rfc4648#section-3.2 padding> characters.
--
-- <https://www.ietf.org/rfc/rfc2045.txt RFC 2045>
-- defines a separate Base64 encoding, which is not supported. This format
-- requires a newline at least every 76 encoded characters, which works around
-- limitations of older email programs that could not handle long lines.
-- Be aware that other languages, such as Ruby, encode the RFC 2045 version
-- by default. To decode their output, remove all newlines before decoding.
--
-- ==== Examples
--
-- A quick example to show the differences:
--
-- >>> let input = "Is 3 > 2?" :: ByteString
-- >>> let convertedTo base = convertToBase base input :: ByteString
-- >>> convertedTo Base16
-- "49732033203e20323f"
-- >>> convertedTo Base32
-- "JFZSAMZAHYQDEPY="
-- >>> convertedTo Base64
-- "SXMgMyA+IDI/"
-- >>> convertedTo Base64URLUnpadded
-- "SXMgMyA-IDI_"
-- >>> convertedTo Base64OpenBSD
-- "QVKeKw.8GBG9"
--
data Base = Base16            -- ^ similar to hexadecimal
          | Base32
          | Base64            -- ^ standard Base64
          | Base64URLUnpadded -- ^ unpadded URL-safe Base64
          | Base64OpenBSD     -- ^ Base64 as used in OpenBSD password encoding (such as bcrypt)
          deriving (Show,Eq)

-- | Encode some bytes to the equivalent representation in a specific 'Base'.
--
-- ==== Examples
--
-- Convert a 'ByteString' to base-64:
--
-- >>> convertToBase Base64 ("foobar" :: ByteString) :: ByteString
-- "Zm9vYmFy"
--
convertToBase :: (ByteArrayAccess bin, ByteArray bout) => Base -> bin -> bout
convertToBase base b =
    let bs = B.convert b :: BS.ByteString
    in case base of
        Base16            -> B.convert $ extractBase16 (B16.encodeBase16' bs)
        Base32            -> B.convert $ B32.encodeBase32' bs
        Base64            -> B.convert $ extractBase64 (B64.encodeBase64' bs)
        Base64URLUnpadded -> B.convert $ extractBase64 (B64URL.encodeBase64Unpadded' bs)
        Base64OpenBSD     ->
            let binLength = B.length b
                (q, r)   = binLength `divMod` 3
                outLen   = 4 * q + (if r == 0 then 0 else r + 1)
            in B.unsafeCreate outLen $ \bout ->
               withByteArray b $ \bin ->
                   toBase64OpenBSD bout bin binLength

-- | Try to decode some bytes from the equivalent representation in a specific 'Base'.
--
-- ==== Examples
--
-- Successfully convert from base-64 to a 'ByteString':
--
-- >>> convertFromBase Base64 ("Zm9vYmFy" :: ByteString) :: Either String ByteString
-- Right "foobar"
--
-- Trying to decode invalid data will return an error string:
--
-- >>> convertFromBase Base64 ("!!!" :: ByteString) :: Either String ByteString
-- Left "Base64-encoded bytestring requires padding"
--
convertFromBase :: (ByteArrayAccess bin, ByteArray bout) => Base -> bin -> Either String bout
convertFromBase base b =
    let bs  = B.convert b :: BS.ByteString
        run = fmap B.convert . mapLeft T.unpack
    in case base of
        Base16            -> run $ B16.decodeBase16Untyped bs
        Base32            -> run $ B32.decodeBase32 bs
        Base64            -> run $ B64.decodeBase64Untyped bs
        Base64URLUnpadded -> run $ B64URL.decodeBase64UnpaddedUntyped bs
        Base64OpenBSD     -> unsafeDoIO $
            withByteArray b $ \bin ->
                case unBase64LengthUnpadded (B.length b) of
                    Nothing     -> return $ Left "base64 unpadded: input: invalid length"
                    Just dstLen -> do
                        (ret, out) <- B.allocRet dstLen $ \bout -> fromBase64OpenBSD bout bin (B.length b)
                        return $ case ret of
                            Nothing  -> Right out
                            Just ofs -> Left ("base64 unpadded: input: invalid encoding at offset: " ++ show ofs)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a)  = Left (f a)
mapLeft _ (Right c) = Right c
