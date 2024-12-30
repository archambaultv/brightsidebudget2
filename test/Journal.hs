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
    loadJournal, validateJournal, saveJournal, JSaveConfig(..))

myRunExceptT :: (Show e) => ExceptT e IO a -> IO ()
myRunExceptT m = do
    result <- runExceptT m
    case result of
        Left err -> assertFailure $ "Failed: " ++ show err
        Right _ -> pure ()

journalTests :: TestTree
journalTests = testGroup "Journal" [loadJournalTest, validateJournalTest1, saveAndReloadJournalTest]

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
