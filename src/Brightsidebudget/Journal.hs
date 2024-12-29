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
import Brightsidebudget.Account (loadAccounts)
import Brightsidebudget.Txn (loadTxns)
import Brightsidebudget.Assertion (loadAssertions)
import Brightsidebudget.Budget (loadBudgetTargets)

loadJournal :: JournalConfig -> ExceptT Text IO Journal
loadJournal (JournalConfig {jcAccounts = accs, jcAssertions = as, jcTxns = txns, jcTargets = targets}) = do
    accs' <- loadAccounts accs
    let fullQn = fmap aName accs'
    txns' <- loadTxns fullQn txns
    as' <-  maybe (pure []) (loadAssertions fullQn) as
    targets' <-  maybe (pure []) (loadBudgetTargets fullQn) targets
    pure $ Journal accs' txns' as' targets'

saveJournal :: FilePath -> Journal -> JournalConfig -> IO ()
saveJournal = undefined

validateJournal :: Journal -> Either Text ()
validateJournal = undefined