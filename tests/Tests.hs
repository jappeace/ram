{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Imports
import           Utils
import           Data.Char                    (chr)
import           Data.Typeable                (Typeable)
import qualified Data.ByteString         as BS
import           Data.ByteArray               (Bytes, ScrubbedBytes, ByteArray)
import qualified Data.ByteArray          as B
import qualified Data.ByteArray.Encoding as B
import qualified Data.ByteArray.Parse    as Parse

import qualified SipHash

newtype Positive = Positive Word
  deriving (Show, Eq, Ord)
instance Arbitrary Positive where
    arbitrary = Positive . fromIntegral <$> choose (0 :: Int, 255)

data Backend = BackendByte | BackendScrubbedBytes
    deriving (Show, Eq, Bounded, Enum)

allBackends :: [Backend]
allBackends = [minBound..maxBound]

data ArbitraryBS = forall a . (Show a, Eq a, Typeable a, ByteArray a) => ArbitraryBS a

arbitraryBS :: Int -> Gen ArbitraryBS
arbitraryBS n = do
    backend <- elements allBackends
    case backend of
        BackendByte          -> ArbitraryBS <$> (B.pack <$> replicateM n arbitrary :: Gen Bytes)
        BackendScrubbedBytes -> ArbitraryBS <$> (B.pack <$> replicateM n arbitrary :: Gen ScrubbedBytes)

arbitraryBSof :: Int -> Int -> Gen ArbitraryBS
arbitraryBSof minBytes maxBytes = choose (minBytes, maxBytes) >>= arbitraryBS

newtype SmallList a = SmallList [a]
    deriving (Show, Eq)

instance Arbitrary a => Arbitrary (SmallList a) where
    arbitrary = choose (0 :: Int, 8) >>= \n -> SmallList <$> replicateM n arbitrary

instance Arbitrary ArbitraryBS where
    arbitrary = arbitraryBSof 0 259

newtype Words8 = Words8 { unWords8 :: [Word8] }
    deriving (Show, Eq)

instance Arbitrary Words8 where
    arbitrary = choose (0 :: Int, 259) >>= \n -> Words8 <$> replicateM n arbitrary

testGroupBackends :: String
                  -> (forall ba . (Show ba, Eq ba, Typeable ba, ByteArray ba) => (ba -> ba) -> [TestTree])
                  -> TestTree
testGroupBackends x l =
    testGroup x
        [ testGroup "Bytes"         (l withBytesWitness)
        , testGroup "ScrubbedBytes" (l withScrubbedBytesWitness)
        ]

testShowProperty :: String
                 -> (forall ba . (Show ba, Eq ba, Typeable ba, ByteArray ba) => (ba -> ba) -> ([Word8] -> String) -> Words8 -> Bool)
                 -> TestTree
testShowProperty x p =
    testGroup x
        [ testProperty "Bytes"        (p withBytesWitness showLikeString)
        , testProperty "ScrubbedBytes" (p withScrubbedBytesWitness showLikeEmptySB)
        ]
  where
    showLikeString  l = show $ (chr . fromIntegral) <$> l
    showLikeEmptySB _ = show (withScrubbedBytesWitness B.empty)

base64Kats :: [(String, String)]
base64Kats =
    [ ("pleasure.", "cGxlYXN1cmUu")
    , ("leasure.", "bGVhc3VyZS4=")
    , ("easure.", "ZWFzdXJlLg==")
    , ("asure.", "YXN1cmUu")
    , ("sure.", "c3VyZS4=")
    , ("", "")
    ]

base64URLKats :: [(String, String)]
base64URLKats =
    [ ("pleasure.", "cGxlYXN1cmUu")
    , ("leasure.", "bGVhc3VyZS4")
    , ("easure.", "ZWFzdXJlLg")
    , ("asure.", "YXN1cmUu")
    , ("sure.", "c3VyZS4")
    , ("\DC4\251\156\ETX\217~", "FPucA9l-")
    , ("\DC4\251\156\ETX\217\DEL", "FPucA9l_")
    , ("", "")
    ]

base16Kats :: [(String, String)]
base16Kats =
    [ ("this is a string", "74686973206973206120737472696e67") ]

base32Kats :: [(String, String)]
base32Kats =
    [ ("-pleasure.", "FVYGYZLBON2XEZJO")
    , ("pleasure.",  "OBWGKYLTOVZGKLQ=")
    , ("leasure.",   "NRSWC43VOJSS4===")
    , ("easure.",    "MVQXG5LSMUXA====")
    , ("asure.",     "MFZXK4TFFY======")
    , ("sure.",      "ON2XEZJO")
    , ("ure.",       "OVZGKLQ=")
    , ("re.",        "OJSS4===")
    , ("e.",         "MUXA====")
    , (".",          "FY======")
    , ("",           "")
    ]

encodingTests :: (forall ba . (Show ba, Eq ba, Typeable ba, ByteArray ba) => (ba -> ba) -> [TestTree])
encodingTests witnessID =
    [ testGroup "BASE64"
        [ testGroup "encode-KAT" encodeKats64
        , testGroup "decode-KAT" decodeKats64
        ]
    , testGroup "BASE64URL"
        [ testGroup "encode-KAT" encodeKats64URLUnpadded
        , testGroup "decode-KAT" decodeKats64URLUnpadded
        ]
    , testGroup "BASE32"
        [ testGroup "encode-KAT" encodeKats32
        , testGroup "decode-KAT" decodeKats32
        ]
    , testGroup "BASE16"
        [ testGroup "encode-KAT" encodeKats16
        , testGroup "decode-KAT" decodeKats16
        ]
    ]
  where
    encodeKats64            = zipWith (toTest B.Base64)           [1..] base64Kats
    decodeKats64            = zipWith (toBackTest B.Base64)       [1..] base64Kats
    encodeKats32            = zipWith (toTest B.Base32)           [1..] base32Kats
    decodeKats32            = zipWith (toBackTest B.Base32)       [1..] base32Kats
    encodeKats16            = zipWith (toTest B.Base16)           [1..] base16Kats
    decodeKats16            = zipWith (toBackTest B.Base16)       [1..] base16Kats
    encodeKats64URLUnpadded = zipWith (toTest B.Base64URLUnpadded)     [1..] base64URLKats
    decodeKats64URLUnpadded = zipWith (toBackTest B.Base64URLUnpadded) [1..] base64URLKats

    toTest :: B.Base -> Int -> (String, String) -> TestTree
    toTest base i (inp, out) = testCase (show i) $
        let inpbs = witnessID $ B.convertToBase base $ witnessID $ B.pack $ unS inp
            outbs = witnessID $ B.pack $ unS out
         in inpbs @?= outbs

    toBackTest :: B.Base -> Int -> (String, String) -> TestTree
    toBackTest base i (inp, out) = testCase (show i) $
        let inpbs = witnessID $ B.pack $ unS inp
            outbs = B.convertFromBase base $ witnessID $ B.pack $ unS out
         in outbs @?= Right inpbs

bsNullEncodingTest :: TestTree
bsNullEncodingTest =
    testGroup "BS-null"
      [ testGroup "BASE64"
        [ testCase "encode-KAT" $ B.convertToBase B.Base64 BS.empty @?= BS.empty
        , testCase "decode-KAT" $ (B.convertFromBase B.Base64 BS.empty :: Either String BS.ByteString) @?= Right BS.empty
        ]
      , testGroup "BASE32"
        [ testCase "encode-KAT" $ B.convertToBase B.Base32 BS.empty @?= BS.empty
        , testCase "decode-KAT" $ (B.convertFromBase B.Base32 BS.empty :: Either String BS.ByteString) @?= Right BS.empty
        ]
      , testGroup "BASE16"
        [ testCase "encode-KAT" $ B.convertToBase B.Base16 BS.empty @?= BS.empty
        , testCase "decode-KAT" $ (B.convertFromBase B.Base16 BS.empty :: Either String BS.ByteString) @?= Right BS.empty
        ]
      ]

parsingTests :: (forall ba . (Show ba, Eq ba, Typeable ba, ByteArray ba) => (ba -> ba) -> [TestTree])
parsingTests witnessID =
    [ testCase "parse" $ do
        let input  = witnessID $ B.pack $ unS "xx abctest"
            abc    = witnessID $ B.pack $ unS "abc"
            est    = witnessID $ B.pack $ unS "est"
            result = Parse.parse ((,,) <$> Parse.take 2 <*> Parse.byte 0x20 <*> (Parse.bytes abc *> Parse.anyByte)) input
        case result of
            Parse.ParseOK remaining _ -> remaining @?= est
            _                         -> fail "unexpected parse result"
    ]

main :: IO ()
main = defaultMain $ testGroup "memory"
    [ testGroupBackends "basic"    basicProperties
    , bsNullEncodingTest
    , testGroupBackends "encoding" encodingTests
    , testGroupBackends "parsing"  parsingTests
    , testGroupBackends "hashing"  $ \witnessID ->
        [ testGroup "SipHash" $ SipHash.tests witnessID
        ]
    , testShowProperty "showing" $ \witnessID expectedShow (Words8 l) ->
          (show . witnessID . B.pack $ l) == expectedShow l
    ]
  where
    basicProperties witnessID =
        [ testProperty "unpack . pack == id" $ \(Words8 l) -> l == (B.unpack . witnessID . B.pack $ l)
        , testProperty "self-eq" $ \(Words8 l) -> let b = witnessID . B.pack $ l in b == b
        , testProperty "add-empty-eq" $ \(Words8 l) ->
            let b = witnessID $ B.pack l
             in B.append b B.empty == b
        , testProperty "zero" $ \(Positive n) ->
            let expected = witnessID $ B.pack $ replicate (fromIntegral n) 0
             in expected == B.zero (fromIntegral n)
        , testProperty "Ord" $ \(Words8 l1) (Words8 l2) ->
            compare l1 l2 == compare (witnessID $ B.pack l1) (witnessID $ B.pack l2)
        , testProperty "Monoid(mappend)" $ \(Words8 l1) (Words8 l2) ->
            mappend l1 l2 == (B.unpack $ mappend (witnessID $ B.pack l1) (B.pack l2))
        , testProperty "Monoid(mconcat)" $ \(SmallList l) ->
            mconcat (fmap unWords8 l) == (B.unpack $ mconcat $ fmap (witnessID . B.pack . unWords8) l)
        , testProperty "append associativity" $ \(Words8 la) (Words8 lb) (Words8 lc) ->
            let a = witnessID $ B.pack la
                b = witnessID $ B.pack lb
                c = witnessID $ B.pack lc
             in B.append (B.append a b) c == B.append a (B.append b c)
        , testProperty "concat l" $ \(SmallList l) ->
            let chunks   = fmap (witnessID . B.pack . unWords8) l
                expected = concatMap unWords8 l
             in B.pack expected == witnessID (B.concat chunks)
        , testProperty "reverse" $ \(Words8 l) ->
            let b = witnessID (B.pack l)
             in reverse l == B.unpack (B.reverse b)
        , testProperty "cons b (reverse bs) == reverse (snoc bs b)" $ \(Words8 l) b ->
            let a = witnessID (B.pack l)
             in B.cons b (B.reverse a) == B.reverse (B.snoc a b)
        , testProperty "all == Prelude.all" $ \(Words8 l) b ->
            let b1 = witnessID (B.pack l)
                p  = (/= b)
             in B.all p b1 == all p l
        , testProperty "any == Prelude.any" $ \(Words8 l) b ->
            let b1 = witnessID (B.pack l)
                p  = (== b)
             in B.any p b1 == any p l
        , testProperty "singleton b == pack [b]" $ \b ->
            witnessID (B.singleton b) == B.pack [b]
        , testProperty "span" $ \x (Words8 l) ->
            let c = witnessID (B.pack l)
                (a, b) = B.span (== x) c
             in c == B.append a b
        , testProperty "span (const True)" $ \(Words8 l) ->
            let a = witnessID (B.pack l)
             in B.span (const True) a == (a, B.empty)
        , testProperty "span (const False)" $ \(Words8 l) ->
            let b = witnessID (B.pack l)
             in B.span (const False) b == (B.empty, b)
        , testProperty "map f == pack . Prelude.map f . unpack" $ \(Words8 l) (Positive w) ->
            let b = witnessID (B.pack l)
                f x = x + fromIntegral w :: Word8
             in B.map f b == (witnessID . B.pack . Prelude.map f $ l)
        ]
