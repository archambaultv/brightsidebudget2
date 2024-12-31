module Brightsidebudget.Journal
    ( 
        loadJournal,
        saveJournal,
        validateJournal,
        failedAssertion,
        actualAssertionAmount

    )
where

import Data.Text (Text)
import Control.Monad (unless)
import Data.Time.Calendar (addDays)
import Control.Monad.Except (ExceptT)
import Brightsidebudget.Data (Journal(..), Account(..), JLoadConfig(..), JSaveConfig(..), WhichDate(..),
                             ABalance, Assertion(..), Amount, AssertionType(..))
import Brightsidebudget.Account (loadAccounts, validateAccounts, saveAccounts)
import Brightsidebudget.Txn (loadTxns, validateTxns, saveTxnsMultipleFiles)
import Brightsidebudget.Assertion (loadAssertions, validateAssertions, saveAssertions)
import Brightsidebudget.Budget (loadBudgetTargets, validateBudgetTargets, saveBudgetTargets)
import Brightsidebudget.ABalance (aBalanceMapTxn, aBalance)

-- | Load a journal from a configuration
loadJournal :: JLoadConfig -> ExceptT Text IO Journal
loadJournal (JLoadConfig {jlAccounts = accs, jlAssertions = as, jlTxns = txns, jlTargets = targets}) = do
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

saveJournal :: JSaveConfig -> Journal -> IO ()
saveJournal journalConfig journal = do
    saveAccounts (jsAccounts journalConfig) (jAccounts journal)
    let txns = jTxns journal
    unless (null txns) (saveTxnsMultipleFiles (jsTxns journalConfig) txns)
    let as = jAssertions journal
    unless (null as) (saveAssertions (jsAssertions journalConfig) as)
    let ts = jTargets journal
    unless (null ts) (saveBudgetTargets (jsTargets journalConfig) ts)

-- | Find all the assertions that failed, with the actual balance map
failedAssertion :: Journal -> (ABalance, [Assertion])
failedAssertion (Journal {jTxns = txns, jAssertions = as}) =
    let balance = aBalanceMapTxn StmtDate txns
        failed = filter (not . checkAssertion balance) as
    in (balance, failed)

    where checkAssertion :: ABalance -> Assertion -> Bool
          checkAssertion balance a@(Assertion {baAmount = amt}) =
              let actual = actualAssertionAmount balance a
              in actual == amt

actualAssertionAmount :: ABalance -> Assertion -> Amount
actualAssertionAmount balance (Assertion {baType = at, baAccount = acc}) =
    case at of
        BalanceAssertion d -> aBalance balance acc d
        FlowAssertion d1 d2 ->
            let m1 = aBalance balance acc (addDays (-1) d1)
                m2 = aBalance balance acc d2
            in m2 - m1