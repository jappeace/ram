{-# LANGUAGE ScopedTypeVariables #-}
module Imports
    ( module X
    , testProperty
    , testCase
    , assertBool
    , assertEqual
    , (@?=)
    ) where

import Prelude                      as X
import Control.Monad                as X (replicateM)
import Data.Word                    as X (Word8)

import Test.Tasty                   as X (TestTree, testGroup, defaultMain, TestName)
import Test.QuickCheck              as X
    ( Arbitrary(..), Gen, Property
    , (===), (.&&.)
    , elements, choose, forAll, property, ioProperty, (==>)
    , Testable
    )

import Test.Tasty.Providers         (singleTest, IsTest(..), testPassed, testFailed)
import Test.QuickCheck              (quickCheckWithResult, stdArgs, isSuccess, Args(..))
import Control.Exception            (SomeException, try)

-- | QuickCheck property test provider for tasty
newtype QCTest = QCTest Property

instance Show QCTest where
    show _ = "QCTest"

instance IsTest QCTest where
    run _ (QCTest p) _ = do
        r <- quickCheckWithResult stdArgs { chatty = False } p
        return $ if isSuccess r
            then testPassed ""
            else testFailed (show r)
    testOptions = pure []

-- | IO unit test provider for tasty
newtype IOTest = IOTest (IO ())

instance Show IOTest where
    show _ = "IOTest"

instance IsTest IOTest where
    run _ (IOTest act) _ = do
        r <- (try act :: IO (Either SomeException ()))
        return $ case r of
            Left ex  -> testFailed (show ex)
            Right () -> testPassed ""
    testOptions = pure []

testProperty :: Testable p => TestName -> p -> TestTree
testProperty name = singleTest name . QCTest . property

testCase :: TestName -> IO () -> TestTree
testCase name = singleTest name . IOTest

assertBool :: String -> Bool -> IO ()
assertBool msg b = if b then return () else fail msg

assertEqual :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEqual msg expected actual
    | expected == actual = return ()
    | otherwise = fail (msg ++ "\nexpected: " ++ show expected ++ "\n but got: " ++ show actual)

infix 1 @?=
(@?=) :: (Show a, Eq a) => a -> a -> IO ()
actual @?= expected
    | actual == expected = return ()
    | otherwise = fail ("expected: " ++ show expected ++ "\n but got: " ++ show actual)
