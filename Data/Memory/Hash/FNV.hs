-- |
-- Module      : Data.Memory.Hash.FNV
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : good
--
-- Fowler Noll Vo Hash (1 and 1a / 32 / 64 bits versions)
-- <http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function>
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns               #-}
module Data.Memory.Hash.FNV
    (
    -- * types
      FnvHash32(..)
    , FnvHash64(..)
    -- * methods
    , fnv1
    , fnv1a
    , fnv1_64
    , fnv1a_64
    ) where

import           Data.Bits               (xor)
import           Data.Word               (Word8, Word32, Word64)
import           Foreign.Ptr             (Ptr)
import           Foreign.Storable        (peekByteOff)
import           Control.DeepSeq         (NFData(..))

-- | FNV1(a) hash (32 bit variants)
newtype FnvHash32 = FnvHash32 Word32
    deriving (Show,Eq,Ord,NFData)

-- | FNV1(a) hash (64 bit variants)
newtype FnvHash64 = FnvHash64 Word64
    deriving (Show,Eq,Ord,NFData)

fnv1_32_Mix8 :: Word8 -> FnvHash32 -> FnvHash32
fnv1_32_Mix8 !w (FnvHash32 acc) = FnvHash32 (xor (0x01000193 * acc) (fromIntegral w))
{-# INLINE fnv1_32_Mix8 #-}

fnv1a_32_Mix8 :: Word8 -> FnvHash32 -> FnvHash32
fnv1a_32_Mix8 !w (FnvHash32 acc) = FnvHash32 (0x01000193 * xor acc (fromIntegral w))
{-# INLINE fnv1a_32_Mix8 #-}

fnv1_64_Mix8 :: Word8 -> FnvHash64 -> FnvHash64
fnv1_64_Mix8 !w (FnvHash64 acc) = FnvHash64 (xor (0x100000001b3 * acc) (fromIntegral w))
{-# INLINE fnv1_64_Mix8 #-}

fnv1a_64_Mix8 :: Word8 -> FnvHash64 -> FnvHash64
fnv1a_64_Mix8 !w (FnvHash64 acc) = FnvHash64 (0x100000001b3 * xor acc (fromIntegral w))
{-# INLINE fnv1a_64_Mix8 #-}

-- | compute FNV1 (32 bit variant) of a raw piece of memory
fnv1 :: Ptr Word8 -> Int -> IO FnvHash32
fnv1 ptr n = loop (FnvHash32 0x811c9dc5) 0
  where
        loop !acc !i
            | i == n    = pure acc
            | otherwise = peekByteOff ptr i >>= \v -> loop (fnv1_32_Mix8 v acc) (i + 1)

-- | compute FNV1a (32 bit variant) of a raw piece of memory
fnv1a :: Ptr Word8 -> Int -> IO FnvHash32
fnv1a ptr n = loop (FnvHash32 0x811c9dc5) 0
  where
        loop !acc !i
            | i == n    = pure acc
            | otherwise = peekByteOff ptr i >>= \v -> loop (fnv1a_32_Mix8 v acc) (i + 1)

-- | compute FNV1 (64 bit variant) of a raw piece of memory
fnv1_64 :: Ptr Word8 -> Int -> IO FnvHash64
fnv1_64 ptr n = loop (FnvHash64 0xcbf29ce484222325) 0
  where
        loop !acc !i
            | i == n    = pure acc
            | otherwise = peekByteOff ptr i >>= \v -> loop (fnv1_64_Mix8 v acc) (i + 1)

-- | compute FNV1a (64 bit variant) of a raw piece of memory
fnv1a_64 :: Ptr Word8 -> Int -> IO FnvHash64
fnv1a_64 ptr n = loop (FnvHash64 0xcbf29ce484222325) 0
  where
        loop !acc !i
            | i == n    = pure acc
            | otherwise = peekByteOff ptr i >>= \v -> loop (fnv1a_64_Mix8 v acc) (i + 1)
