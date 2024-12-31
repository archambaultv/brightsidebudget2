{-# LANGUAGE OverloadedStrings #-}

module Journal (
    journalTests,
    myRunExceptT
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (runExceptT, liftEither, ExceptT)
import Brightsidebudget (JLoadConfig(..), Journal(..), Txn(..), Posting(..),
    loadJournal, validateJournal, saveJournal, JSaveConfig(..), loadAndValidateJournal, failedAssertion)

myRunExceptT :: (Show e) => ExceptT e IO a -> IO ()
myRunExceptT m = do
    result <- runExceptT m
    case result of
        Left err -> assertFailure $ "Failed: " ++ show err
        Right _ -> pure ()

journalTests :: TestTree
journalTests = testGroup "Journal" [
                    loadJournalTest,
                    validateJournalTest1,
                    saveAndReloadJournalTest,
                    txnTest1,
                    assertionsTest1,
                    assertionsTest2,
                    assertionsTest3
                    ]

config :: JLoadConfig
config = JLoadConfig {
        jlAccounts = "test/fixtures/comptes.csv",
        jlTxns = ["test/fixtures/txns.csv"],
        jlAssertions = Just "test/fixtures/soldes.csv",
        jlTargets = Just "test/fixtures/budget.csv"
        }

saveConfig :: JSaveConfig
saveConfig = JSaveConfig {
        jsAccounts = "test/output/comptes.csv",
        jsTxns = \_ -> "test/output/txns.csv",
        jsAssertions = "test/output/soldes.csv",
        jsTargets = "test/output/budget.csv"
        }

reloadConfig :: JLoadConfig
reloadConfig = JLoadConfig {
        jlAccounts = "test/output/comptes.csv",
        jlTxns = ["test/output/txns.csv"],
        jlAssertions = Just "test/output/soldes.csv",
        jlTargets = Just "test/output/budget.csv"
        }

loadJournalTest :: TestTree
loadJournalTest = testCase "loadJournal" $ myRunExceptT $ do
    journal <- loadJournal config
    liftIO $ assertEqual "Nb of accounts" 17 (length (jAccounts journal))
    liftIO $ assertEqual "Nb of txns" 2 (length (jTxns journal))
    liftIO $ assertEqual "Nb of assertions" 7 (length (jAssertions journal))
    liftIO $ assertEqual "Nb of targets" 4 (length (jTargets journal))
    let txn0 = jTxns journal !! 0
    let ps0 = txnPostings txn0 !! 0
    liftIO $ assertEqual "Short QName" ["Compte courant"] (pAccount ps0)

validateJournalTest1 :: TestTree
validateJournalTest1 = testCase "validateJournal" $ myRunExceptT $ do
    journal <- loadJournal config >>= (liftEither . validateJournal)
    let txn0 = jTxns journal !! 0
    let ps0 = txnPostings txn0 !! 0
    liftIO $ assertEqual "Full QName" ["Actifs", "Compte courant"] (pAccount ps0)

saveAndReloadJournalTest :: TestTree
saveAndReloadJournalTest = testCase "saveAndReloadJournal" $ myRunExceptT $ do
    -- Load the journal
    journal <- loadJournal config
    liftIO $ saveJournal saveConfig journal
    reloadedJournal <- loadJournal reloadConfig
    liftIO $ assertEqual "Journals should be equal" journal reloadedJournal

assertionsTest1 :: TestTree
assertionsTest1 = testCase "All assertions OK" $ myRunExceptT $ do
    journal <- loadAndValidateJournal config
    let (_, failed) = failedAssertion journal
    liftIO $ assertEqual "Assertions OK" [] failed 

assertionsTest2 :: TestTree
assertionsTest2 = testCase "Assertions with no txns" $ myRunExceptT $ do
    j <- loadAndValidateJournal config
    let journal = j {jTxns = []}
    let (_, failed) = failedAssertion journal
    liftIO $ assertEqual "Assertions no txns" 7 (length failed)

-- | Test duplicate balance assertion
assertionsTest3 :: TestTree
assertionsTest3 = testCase "duplicate assertion" $ myRunExceptT $ do
    j <- loadAndValidateJournal config
    let dup = jAssertions j !! 0
    let journal = j {jAssertions = [dup, dup]}
    case validateJournal journal of
        Left _ -> pure ()
        Right _ -> liftIO $ assertFailure "Should have failed"

-- | Test duplicate txn id
txnTest1 :: TestTree
txnTest1 = testCase "duplicate txn id" $ myRunExceptT $ do
    j <- loadAndValidateJournal config
    let txn = jTxns j !! 0
    let journal = j {jTxns = [txn, txn]}
    case validateJournal journal of
        Left _ -> pure ()
        Right _ -> liftIO $ assertFailure "Should have failed"