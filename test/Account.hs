{-# LANGUAGE OverloadedStrings #-}

module Account (
    accountTests
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Either (isLeft, isRight)
import Brightsidebudget.Account as A

accountTests :: TestTree
accountTests = testGroup "Account" [qNameTests]

qNameTests :: TestTree
qNameTests = testCase "QName" $ do
    assertEqual "textToQname" (A.textToQname "A:B:C") ["A", "B", "C"]
    assertEqual "textToQname" (length $ A.textToQname "A:B:C") 3

    assertEqual "validateQname" True (isRight (A.validateQname $ A.textToQname "A:B:C")) 
    assertEqual "validateQname" True (isLeft (A.validateQname $ A.textToQname "A:B:"))
    assertEqual "validateQname" True (isLeft (A.validateQname $ A.textToQname ":A:B"))
    assertEqual "validateQname" True (isLeft (A.validateQname $ A.textToQname "A::B"))
    assertEqual "validateQname" True (isLeft (A.validateQname $ ["A:", "B"]))

    assertEqual "parent" (A.parent ["A", "B", "C"]) ["A", "B"]
    assertEqual "parent" (A.parent ["A"]) []

    assertEqual "basename" (A.basename ["A", "B", "C"]) "C"
    assertEqual "basename" (A.basename ["A"]) "A"

    assertEqual "isParentOf" (A.isParentOf ["A", "B"] ["A", "B", "C"]) True
    assertEqual "isParentOf" (A.isParentOf ["A", "C"] ["A", "B", "C"]) False
    assertEqual "isParentOf" (A.isParentOf ["A", "B", "C"] ["A", "B"]) False
    assertEqual "isParentOf" (A.isParentOf ["A", "B"] ["A", "B"]) False

    assertEqual "isChildOf" (A.isChildOf ["A", "B", "C"] ["A", "B"]) True
    assertEqual "isChildOf" (A.isChildOf ["A", "D", "C"] ["A", "B", "C"]) False
    assertEqual "isChildOf" (A.isChildOf ["A", "B"] ["A", "B", "C"]) False
    assertEqual "isChildOf" (A.isChildOf ["A", "B"] ["A", "B"]) False
