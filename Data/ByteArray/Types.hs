-- |
-- Module      : Data.ByteArray.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Good
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.ByteArray.Types
    ( ByteArrayAccess(..)
    , ByteArray(..)
    ) where

import           Foreign.Ptr
import           Data.Monoid
import qualified Data.ByteString as Bytestring (length)
import qualified Data.ByteString.Internal as Bytestring
import           Foreign.ForeignPtr (withForeignPtr)

import           Data.Memory.PtrMethods (memCopy)

import Prelude hiding (length)

-- | Class to Access size properties and data of a ByteArray
class ByteArrayAccess ba where
    -- | Return the length in bytes of a bytearray
    length        :: ba -> Int
    -- | Allow to use using a pointer
    withByteArray :: ba -> (Ptr p -> IO a) -> IO a
    -- | Copy the data of a bytearray to a ptr
    copyByteArrayToPtr :: ba -> Ptr p -> IO ()
    copyByteArrayToPtr a dst = withByteArray a $ \src -> memCopy (castPtr dst) src (length a)

-- | Class to allocate new ByteArray of specific size
class (Eq ba, Ord ba, Monoid ba, ByteArrayAccess ba) => ByteArray ba where
    -- | allocate `n` bytes and perform the given operation
    allocRet  :: Int
                -- ^ number of bytes to allocate. i.e. might not match the
                -- size of the given type `ba`.
              -> (Ptr p -> IO a)
              -> IO (a, ba)

instance ByteArrayAccess Bytestring.ByteString where
    length = Bytestring.length
    withByteArray (Bytestring.PS fptr off _) f = withForeignPtr fptr $ \ptr -> f $! (ptr `plusPtr` off)

instance ByteArray Bytestring.ByteString where
    allocRet sz f = do
        fptr <- Bytestring.mallocByteString sz
        r    <- withForeignPtr fptr (f . castPtr)
        return (r, Bytestring.PS fptr 0 sz)

