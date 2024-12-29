{-# LANGUAGE OverloadedStrings #-}

module Journal (
    journalTests
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Except (runExceptT, liftEither)
import Brightsidebudget (JournalConfig(..), Journal(..), Txn(..), Posting(..), loadJournal, validateJournal)

journalTests :: TestTree
journalTests = testGroup "Journal" [loadJournalTest, validateJournalTest1, validateJournalTest2]

config :: JournalConfig
config = JournalConfig {
        jcAccounts = "test/fixtures/comptes.csv",
        jcTxns = ["test/fixtures/txns.csv"],
        jcAssertions = Just "test/fixtures/soldes.csv",
        jcTargets = Just "test/fixtures/budget.csv"
        }

loadJournalTest :: TestTree
loadJournalTest = testCase "loadJournal" $ do
    result <- runExceptT $ loadJournal config
    case result of
        Left err -> assertFailure $ "Failed to load journal: " ++ show err
        Right journal -> do
            assertEqual "Nb of accounts" 17 (length (jAccounts journal))
            assertEqual "Nb of txns" 2 (length (jTxns journal))
            assertEqual "Nb of assertions" 7 (length (jAssertions journal))
            assertEqual "Nb of targets" 4 (length (jTargets journal))
            let txn0 = jTxns journal !! 0
            let ps0 = txnPostings txn0 !! 0
            assertEqual "Short QName" ["Compte courant"] (pAccount ps0)

validateJournalTest1 :: TestTree
validateJournalTest1 = testCase "validateJournal" $ do
    result <- runExceptT $ loadJournal config >>= (liftEither . validateJournal)
    case result of
        Left err -> assertFailure $ "Failed to validate journal: " ++ show err
        Right journal -> do
            let txn0 = jTxns journal !! 0
            let ps0 = txnPostings txn0 !! 0
            assertEqual "Full QName" ["Actifs", "Compte courant"] (pAccount ps0)
