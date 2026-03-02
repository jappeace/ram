-- |
-- Module      : Data.ByteArray.Bytes
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Good
--
-- Simple and efficient byte array types
--
module Data.ByteArray.Bytes
    ( Bytes
    ) where

import           Control.DeepSeq          (NFData(..))
import           Data.ByteArray.Types
import           Data.Char                (chr)
import           Data.Foldable            (toList)
import           Data.Semigroup
import           Data.Typeable
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import           Foreign.ForeignPtr       (withForeignPtr)
import           Foreign.Ptr              (castPtr)

-- | Simplest Byte Array
newtype Bytes = Bytes BS.ByteString
  deriving (Typeable)

instance NFData Bytes where
    rnf b = b `seq` ()

instance Show Bytes where
    showsPrec p (Bytes b) = showsPrec p (map (chr . fromIntegral) (BS.unpack b))

instance Eq Bytes where
    Bytes a == Bytes b = a == b

instance Ord Bytes where
    compare (Bytes a) (Bytes b) = compare a b

instance Semigroup Bytes where
    Bytes a <> Bytes b = Bytes (a <> b)
    sconcat bs = Bytes $ BS.concat [b | Bytes b <- toList bs]

instance Monoid Bytes where
    mempty = Bytes BS.empty

instance ByteArrayAccess Bytes where
    length (Bytes b) = BS.length b
    withByteArray (Bytes b) = withByteArray b

instance ByteArray Bytes where
    allocRet n f = do
        fptr <- BSI.mallocByteString n
        r    <- withForeignPtr fptr (f . castPtr)
        return (r, Bytes (BSI.PS fptr 0 n))
