{-# LANGUAGE OverloadedStrings #-}

module Account (
    accountTests
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Data.List.NonEmpty (NonEmpty(..))
import Data.Either (isLeft, isRight)
import qualified Brightsidebudget.Journal.Account as A

accountTests :: TestTree
accountTests = testGroup "Account" [qNameTests,
    toShortNamesTest]

qNameTests :: TestTree
qNameTests = testCase "QName" $ do
    assertEqual "textToQname" (A.textToQname "A:B:C") ("A" :| ["B", "C"])
    assertEqual "textToQname" (length $ A.textToQname "A:B:C") 3

    assertEqual "validateQname" True (isRight (A.validateQname $ A.textToQname "A:B:C")) 
    assertEqual "validateQname" True (isLeft (A.validateQname $ A.textToQname "A:B:"))
    assertEqual "validateQname" True (isLeft (A.validateQname $ A.textToQname ":A:B"))
    assertEqual "validateQname" True (isLeft (A.validateQname $ A.textToQname "A::B"))
    assertEqual "validateQname" True (isLeft (A.validateQname $ "A:" :| ["B"]))

    assertEqual "parent" (A.parent ("A" :| ["B", "C"])) (Just ("A" :| ["B"]))
    assertEqual "parent" (A.parent ("A" :| [])) Nothing

    assertEqual "basename" (A.basename ("A" :| ["B", "C"])) "C"
    assertEqual "basename" (A.basename ("A" :| [])) "A"

    assertEqual "isParentOf" (A.isParentOf ("A" :| ["B"]) ("A" :| ["B", "C"])) True
    assertEqual "isParentOf" (A.isParentOf ("A" :| ["C"]) ("A" :| ["B", "C"])) False
    assertEqual "isParentOf" (A.isParentOf ("A" :| ["B", "C"]) ("A" :| ["B"])) False
    assertEqual "isParentOf" (A.isParentOf ("A" :| ["B"]) ("A" :| ["B"])) False

    assertEqual "isChildOf" (A.isChildOf ("A" :| ["B", "C"]) ("A" :| ["B"])) True
    assertEqual "isChildOf" (A.isChildOf ("A" :| ["D", "C"]) ("A" :| ["B", "C"])) False
    assertEqual "isChildOf" (A.isChildOf ("A" :| ["B"]) ("A" :| ["B", "C"])) False
    assertEqual "isChildOf" (A.isChildOf ("A" :| ["B"]) ("A" :| ["B"])) False

toShortNamesTest :: TestTree
toShortNamesTest = testCase "toShortNames" $ do
    let qnames = [ "A" :| ["B", "C"],
                   "A" :| ["B", "D"],
                   "A" :| [],
                   "A" :| ["B"]]
        zeLength ("A" :| ["B", "C"]) = 2
        zeLength _ = 1
    let shortNames = A.toShortNames zeLength qnames
    assertEqual "toShortNames" 4 (length shortNames)
    assertEqual "toShortNames" ("B" :| ["C"]) (shortNames !! 0)
    assertEqual "toShortNames" ("D" :| []) (shortNames !! 1)
    assertEqual "toShortNames" ("A" :| []) (shortNames !! 2)
    assertEqual "toShortNames" ("B" :| []) (shortNames !! 3)