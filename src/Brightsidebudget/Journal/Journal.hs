{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Journal.Journal
    ( 
        loadJournal,
        validateJournal,
        loadAndValidateJournal,
        loadValidateAndCheckJournal,
        saveJournal,
        failedAssertions,
        actualAssertionAmount

    )
where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (unless)
import Data.Time.Calendar (addDays)
import qualified Data.HashMap.Strict as HM
import Control.Monad.Except (ExceptT, liftEither, throwError)
import Brightsidebudget.Journal.Data (Journal(..), Account(..), JLoadConfig(..), JSaveConfig(..), WhichDate(..),
                             ABalance, Assertion(..), Amount, AssertionType(..), QName, Txn(..), Posting(..),
                             BudgetTarget(..))
import Brightsidebudget.Journal.Account (loadAccounts, validateAccounts, saveAccounts, toShortNames, qnameToText)
import Brightsidebudget.Journal.Txn (loadTxns, validateTxns, saveTxnsMultipleFiles)
import Brightsidebudget.Journal.Assertion (loadAssertions, validateAssertions, saveAssertions, showDate)
import Brightsidebudget.Journal.Budget (loadBudgetTargets, validateBudgetTargets, saveBudgetTargets)
import Brightsidebudget.Journal.ABalance (aBalanceMapTxn, aBalance)
import Brightsidebudget.Journal.Amount (amountToDouble)

-- | Load a journal from a configuration. This function should be followed by
--   validateJournal, since other functions assume the QName to be full qnames.
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

-- | Load and validate a journal from a configuration
loadAndValidateJournal :: JLoadConfig -> ExceptT Text IO Journal
loadAndValidateJournal config = loadJournal config >>= liftEither . validateJournal

-- | Load and validate a journal from a configuration, and check the assertions
loadValidateAndCheckJournal :: JLoadConfig -> ExceptT Text IO Journal
loadValidateAndCheckJournal config = do
    journal <- loadAndValidateJournal config
    let (bMap, errors) = failedAssertions journal
    if null errors
        then pure journal
        else throwError $ T.intercalate "\n" $ map (assertionErrors bMap) errors

    where
        assertionErrors :: ABalance -> Assertion -> Text
        assertionErrors bMap as =
            let actualAmnt = actualAssertionAmount bMap as
                expectedAmnt = baAmount as
                diff = expectedAmnt - actualAmnt
                acc = qnameToText $ baAccount as
                date = showDate $ baType as
                showAmnt = T.pack . show . amountToDouble
            in T.concat ["Assertion failed: ", acc, " on ", date, " expected ", showAmnt expectedAmnt, " but got ",
                         showAmnt actualAmnt, " (diff: ", showAmnt diff, ")"]

-- | Save journal to files. Works on validated journal
saveJournal :: JSaveConfig -> Journal -> IO ()
saveJournal journalConfig j = 
    let j2 = shortQnameJournal (jsQnameLength journalConfig) j
    in saveJournal' j2
    
    where 
        saveJournal' :: Journal -> IO ()
        saveJournal' journal = do
            saveAccounts (jsAccounts journalConfig) (jAccounts journal)
            let txns = jTxns journal
            unless (null txns) (saveTxnsMultipleFiles (jsTxns journalConfig) txns)
            let as = jAssertions journal
            unless (null as) (saveAssertions (jsAssertions journalConfig) as)
            let ts = jTargets journal
            unless (null ts) (saveBudgetTargets (jsTargets journalConfig) ts)

-- | Shorten the qname of the journal for txns, assertions, and targets
shortQnameJournal :: (QName -> Int) -> Journal -> Journal
shortQnameJournal f (Journal {jAccounts = accs, jTxns = txns, jAssertions = as, jTargets = ts}) =
    let names = map aName accs
        sqns = HM.fromList $ zip names (toShortNames f names)
        txns' = map (shortQnameTxn sqns) txns
        as' = map (shortQnameAssertion sqns) as
        ts' = map (shortQnameTarget sqns) ts
    in Journal accs txns' as' ts'

    where
        shortQnameTxn :: HM.HashMap QName QName -> Txn -> Txn
        shortQnameTxn sqns (Txn {txnPostings = ps, txnId = tid, txnDate = td}) =
            let ps' = map (shortQnamePosting sqns) ps
            in Txn tid td ps'

        shortQnamePosting :: HM.HashMap QName QName -> Posting -> Posting
        shortQnamePosting sqns (Posting {pAccount = acc, pAmount = amt, pComment = cmt, pStmtDesc = sd, pStmtDate = sd2}) =
            if HM.member acc sqns
                then let acc' = sqns HM.! acc
                     in Posting acc' amt cmt sd sd2
                else error $ "shortQnamePosting: " ++ show acc ++ " not found"

        shortQnameAssertion :: HM.HashMap QName QName -> Assertion -> Assertion
        shortQnameAssertion sqns (Assertion {baAccount = acc, baAmount = amt, baType = at, baComment = comment}) =
            let acc' = sqns HM.! acc
            in Assertion at acc' amt comment

        shortQnameTarget :: HM.HashMap QName QName -> BudgetTarget -> BudgetTarget
        shortQnameTarget sqns (BudgetTarget {btAccount = acc, btAmount = amt, btComment = cmt, btStart = st, btFrequency = fr, btInterval = iv, btUntil = un}) =
            let acc' = sqns HM.! acc
            in BudgetTarget acc' amt cmt st fr iv un


-- | Find all the assertions that failed, with the actual balance map
failedAssertions :: Journal -> (ABalance, [Assertion])
failedAssertions (Journal {jAccounts = accs, jTxns = txns, jAssertions = as}) =
    let names = map aName accs
        balance = aBalanceMapTxn StmtDate (Just names) txns
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