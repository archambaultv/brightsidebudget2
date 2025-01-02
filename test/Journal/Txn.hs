{-# LANGUAGE OverloadedStrings #-}

module Journal.Txn (
    txnTests
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Data.List.NonEmpty (NonEmpty(..))
import Data.Either (isLeft)
import Brightsidebudget.Journal (Posting(..), Txn(..))
import qualified Brightsidebudget.Journal as J

txnTests :: TestTree
txnTests = testGroup "Txn" [validateTxnTest]

validateTxnTest :: TestTree
validateTxnTest =
    let accs = ["A" :| [],
                "A" :| ["B"],
                "A" :| ["B", "C"],
                "D" :| []]
        p1 = Posting {pAccount = "B" :| [], pAmount = 1,
                      pComment = "", pStmtDesc = "", pStmtDate = Nothing}
        p2 = Posting {pAccount = "D" :| [], pAmount = -1,
                      pComment = "", pStmtDesc = "", pStmtDate = Nothing}

        txn = Txn {txnId = 1, txnDate = read "2021-01-01", txnPostings = [p1, p2]}
        err = J.validateTxn accs txn

    in testCase "Parent account in txn" $ do
        assertEqual "validateTxn" True (isLeft err)