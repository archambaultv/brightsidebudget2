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
        jcTxns = [], -- ["test/fixtures/txns.csv"],
        jcAssertions = Nothing, -- "test/fixtures/soldes.csv",
        jcTargets = Nothing -- "test/fixtures/budget.csv"}
}

loadJournalTests :: TestTree
loadJournalTests = testCase "loadJournal" $ do
    result <- runExceptT $ loadJournal config
    case result of
        Left err -> assertFailure $ "Failed to load journal: " ++ show err
        Right journal -> do
            assertBool "Nb of accounts" (length (jAccounts journal) == 17)