module Brightsidebudget.Journal
    ( 
        loadJournal,
        saveJournal,
        validateJournal
    )
where

import Data.Text (Text)
import Control.Monad.Except (ExceptT)
import Brightsidebudget.Data (Journal(..), Txn(..), Assertion(..), Account(..), QName, JournalConfig(..))
import Brightsidebudget.Account (loadAccounts)
import Brightsidebudget.Txn (loadTxns)

loadJournal :: JournalConfig -> ExceptT Text IO Journal
loadJournal (JournalConfig {jcAccounts = accs, jcAssertions = as, jcTxns = txns, jcTargets = targets}) = do
    accs' <- loadAccounts accs
    let fullQn = fmap aName accs'
    txns' <- loadTxns fullQn txns
    pure $ Journal accs' txns' [] []

saveJournal :: FilePath -> Journal -> JournalConfig -> IO ()
saveJournal = undefined

validateJournal :: Journal -> Either Text ()
validateJournal = undefined