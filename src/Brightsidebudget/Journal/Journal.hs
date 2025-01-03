{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Brightsidebudget.Journal.Journal
    ( 
        jBalanceMap,
        loadJournal,
        validateJournal,
        loadAndValidateJournal,
        loadValidateAndCheckJournal,
        saveJournal,
        failedAssertions,
        actualAssertionAmount,
        lastAssertionDate,
        fixStmtDate,
        addTxns,
        updateTxns,
        nextTxnId
    )
where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (unless, foldM)
import Data.List (sort, groupBy, sortBy)
import Data.Ord (comparing)
import Data.Time.Calendar (addDays, Day)
import qualified Data.HashMap.Strict as HM
import Control.Monad.Except (ExceptT, liftEither, throwError)
import Brightsidebudget.Journal.Data (Journal(..), Account(..), JLoadConfig(..), JSaveConfig(..), WhichDate(..),
                             ABalance, Assertion(..), Amount, AssertionType(..), QName, Txn(..), Posting(..),
                             BudgetTarget(..))
import Brightsidebudget.Journal.Account (loadAccounts, validateAccounts, saveAccounts, toShortNames, qnameToText)
import Brightsidebudget.Journal.Txn (loadTxns, validateTxns, saveTxnsMultipleFiles, updatePostings)
import Brightsidebudget.Journal.Assertion (loadAssertions, validateAssertions, saveAssertions, showDate)
import Brightsidebudget.Journal.Budget (loadBudgetTargets, validateBudgetTargets, saveBudgetTargetsMultipleFiles)
import Brightsidebudget.Journal.ABalance (aBalanceMapTxn, aBalance)
import Brightsidebudget.Journal.Amount (amountToDouble)

-- | Load a journal from a configuration. This function should be followed by
--   validateJournal, since other functions assume the QName to be full qnames.
loadJournal :: JLoadConfig -> ExceptT Text IO Journal
loadJournal (JLoadConfig {jlAccounts = accs, jlAssertions = as, jlTxns = txns, jlTargets = targets}) = do
    accs' <- loadAccounts accs
    txns' <- loadTxns txns
    as' <-  maybe (pure []) loadAssertions as
    targets' <-  loadBudgetTargets targets
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
            unless (null ts) (saveBudgetTargetsMultipleFiles (jsTargets journalConfig) ts)

-- | Return the balance map
jBalanceMap :: Journal -> WhichDate -> ABalance
jBalanceMap j wd = aBalanceMapTxn wd (Just (map aName $ jAccounts j)) (jTxns j)

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

lastAssertionDate :: Journal -> QName -> Maybe Day
lastAssertionDate j acc =
    let as = (reverse . sort)
           $ map maxDate
           $ filter (\a -> baAccount a == acc)
           $ jAssertions j
    in case as of
        [] -> Nothing
        (x:_) -> Just x 

    where maxDate :: Assertion -> Day
          maxDate (Assertion {baType = at}) =
              case at of
                  BalanceAssertion d -> d
                  FlowAssertion _ d -> d

nextTxnId :: Journal -> Int
nextTxnId j | jTxns j == [] = 1
nextTxnId j = maximum (map txnId (jTxns j)) + 1

-- | Add transactions to the journal, and return the new transactions and the updated journal
--   The added transaction are renumbered starting from the maximum txn id + 1 and validated
addTxns :: Journal -> [Txn] -> Either Text ([Txn], Journal)
addTxns j txns =
    let maxTxnId = nextTxnId j
        txns2 = zipWith (\i t -> t {txnId = i}) [maxTxnId + 1..] txns
        fullQn = fmap aName (jAccounts j)
    in do
        txns3 <- validateTxns fullQn txns2
        pure (txns3, j{ jTxns = jTxns j ++ txns3 })

-- | Replace the txns in the Journal by those in the list, matching on txnId
updateTxns :: Journal -> [Txn] -> Either Text Journal
updateTxns j ls =
    let txnsMap = HM.fromList [(txnId t, t) | t <- jTxns j]
        fullQn = fmap aName (jAccounts j)
        foo m t =
            case HM.lookup (txnId t) m of
                Nothing -> Left $ "Txn id" <> (T.pack $ show $ txnId t) <> " not found"
                Just _ -> pure $ HM.adjust (const t) (txnId t) m
    in do
        ls' <- validateTxns fullQn ls
        txnsMap' <- foldM foo txnsMap ls'
        let txns = map snd $ HM.toList txnsMap'
        pure j{jTxns = txns}

-- | Tries to satisfy the balance assertion by shifting some statement date
-- after the balance assertion date
fixStmtDate :: Journal -> Integer -> ABalance -> Assertion -> Maybe Journal
fixStmtDate j maxDays balanceMap ba =
    let actual = actualAssertionAmount balanceMap ba
        target = actual - baAmount ba
        end = case baType ba of
                BalanceAssertion d -> d
                FlowAssertion _ d -> d
        start = addDays (-maxDays) end
        subset :: Maybe [(Txn, Posting)]
        subset = findSubset j (baAccount ba) target start end StmtDate
        regroup :: [(Txn, Posting)] -> [(Txn, [Posting])]
        regroup xs = 
            let 
                xs1 :: [(Txn, Posting)]
                xs1 = sortBy (comparing (txnId . fst)) xs
                xs2 :: [[(Txn, Posting)]]
                xs2 = groupBy (\a b -> txnId (fst a) == txnId (fst b)) xs1
                foo :: [(Txn, Posting)] -> (Txn, [Posting])
                foo [] = error "Impossible"
                foo ls@((t,_) : _) = (t, map snd ls)
            in map foo xs2
        newPs :: (Txn, [Posting]) -> (Txn, [(Posting, Posting)])
        newPs (t, xs) = (t, map (\p -> (p, p{pStmtDate = Just $ addDays 1 end})) xs)
        upTxns ::  (Txn, [(Posting, Posting)]) -> Txn
        upTxns (t, xs) = updatePostings t xs
        newTxns :: [(Txn, Posting)] -> [Txn]
        newTxns xs = map (upTxns . newPs) $ regroup xs
        updateJ :: [(Txn, Posting)] -> Maybe Journal
        updateJ xs = either (const Nothing) pure $ updateTxns j (newTxns xs)
    in subset >>= updateJ

findSubset :: Journal -> QName -> Integer -> Day -> Day -> WhichDate -> Maybe [(Txn, Posting)]
findSubset j acc target start end whichDate =
    let ps :: [(Txn, Posting)]
        ps = reverse
           $ sortBy (comparing getDate)
           $ filter (\x -> getDate x >= start && getDate x <= end)
           $ filter (\(_, p) -> pAccount p == acc)
           $ concatMap (\t -> map (t,) (txnPostings t)) (jTxns j)
    in subsetsSum target ps
    where getDate :: (Txn, Posting) -> Day
          getDate (t, p) = case whichDate of
                                TxnDate -> txnDate t
                                StmtDate -> maybe (txnDate t) id $ pStmtDate p

subsetsSum :: Amount -> [(Txn, Posting)] -> Maybe [(Txn, Posting)]
subsetsSum target xs =
    let initialMap = HM.singleton 0 []
    in go initialMap xs
  where
    go :: HM.HashMap Amount [(Txn, Posting)]  -> [(Txn, Posting)] -> Maybe [(Txn, Posting)]
    go currentMap [] = HM.lookup target currentMap
    go currentMap (x:rest) =
      -- For each sum s we already have, attempt to form the new sum s + x
      -- unless it's already in our map (to keep the map from exploding in size).
      let xAmount = pAmount $ snd x
          newPairs = [ (s + xAmount, x : subset) | (s, subset) <- HM.toList currentMap, not (HM.member (s + xAmount) currentMap)]
          -- Insert all new sums into the map.
          newMap = foldl (\acc (sumVal, sub) -> HM.insert sumVal sub acc) currentMap newPairs
      in case HM.lookup target newMap of
           Just found -> Just found   -- We found a subset summing to target
           Nothing    -> go newMap rest