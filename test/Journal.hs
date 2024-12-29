module Journal (
    journalTests
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Except (runExceptT)
import Brightsidebudget.Journal (loadJournal)
import Brightsidebudget.Data (JournalConfig(..), Journal(..))

journalTests :: TestTree
journalTests = testGroup "Journal" [loadJournalTests]

config :: JournalConfig
config = JournalConfig {
        jcAccounts = "test/fixtures/comptes.csv",
        jcTxns = ["test/fixtures/txns.csv"],
        jcAssertions = Just "test/fixtures/soldes.csv",
        jcTargets = Just "test/fixtures/budget.csv"
        }

loadJournalTests :: TestTree
loadJournalTests = testCase "loadJournal" $ do
    result <- runExceptT $ loadJournal config
    case result of
        Left err -> assertFailure $ "Failed to load journal: " ++ show err
        Right journal -> do
            assertEqual "Nb of accounts" 17 (length (jAccounts journal))
            assertEqual "Nb of txns" 2 (length (jTxns journal))
            assertEqual "Nb of assertions" 7 (length (jAssertions journal))
            assertEqual "Nb of targets" 4 (length (jTargets journal))