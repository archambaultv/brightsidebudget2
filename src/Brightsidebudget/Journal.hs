module Brightsidebudget.Journal
    ( 
        loadJournal,
        saveJournal,
        validateJournal
    )
where

import Data.Text (Text)
import Control.Monad.Except (ExceptT)
import Brightsidebudget.Data (Journal(..), Account(..), JournalConfig(..))
import Brightsidebudget.Account (loadAccounts, validateAccounts)
import Brightsidebudget.Txn (loadTxns, validateTxns)
import Brightsidebudget.Assertion (loadAssertions, validateAssertions)
import Brightsidebudget.Budget (loadBudgetTargets, validateBudgetTargets)

-- | Load a journal from a configuration
loadJournal :: JournalConfig -> ExceptT Text IO Journal
loadJournal (JournalConfig {jcAccounts = accs, jcAssertions = as, jcTxns = txns, jcTargets = targets}) = do
    accs' <- loadAccounts accs
    txns' <- loadTxns txns
    as' <-  maybe (pure []) loadAssertions as
    targets' <-  maybe (pure []) loadBudgetTargets targets
    pure $ Journal accs' txns' as' targets'

validateJournal :: Journal -> Either Text Journal
validateJournal (Journal {jAccounts = accs, jTxns = txns, jAssertions = as, jTargets = targets}) = do
    validateAccounts accs
    let fullQn = fmap aName accs
    txns2 <- validateTxns fullQn txns
    as2 <- validateAssertions fullQn as
    targets2 <- validateBudgetTargets fullQn targets
    pure $ Journal accs txns2 as2 targets2

saveJournal :: FilePath -> Journal -> JournalConfig -> IO ()
saveJournal = undefined
