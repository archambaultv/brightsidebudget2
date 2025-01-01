{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Journal.Budget
    (
      fromCsvBudgetTarget,
      toCsvBudgetTarget,
      validateBudgetTarget,
      validateBudgetTargets,
      loadBudgetTargets,
      saveBudgetTargets,
      targetToTxn
    )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Csv (decodeByName, encodeDefaultOrderedByName)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Control.Monad (when)
import Data.Time.Calendar (addDays, addGregorianMonthsClip, addGregorianYearsClip)
import Control.Monad.Except (ExceptT, throwError, liftEither)
import Brightsidebudget.Utils (loadFile)
import Brightsidebudget.Journal.Data (BudgetTarget(..), CsvBudgetTarget(..), BudgetFrequency(..), QName,
                                        Txn(..), Posting(..))
import Brightsidebudget.Journal.Account (textToQname, validateQname, shortNameOf, qnameToText)
import Brightsidebudget.Journal.Amount (doubleToAmount, amountToDouble)
import Brightsidebudget.Journal.Calendar (dateAsDay, dayAsDate)

fromCsvBudgetTarget :: CsvBudgetTarget -> Either Text BudgetTarget
fromCsvBudgetTarget (CsvBudgetTarget {csvbtAccount = acc, csvbtAmount = amt, csvbtComment = cmt,
                                      csvbtStart = start, csvbtFrequency = freq, csvbtInterval = interval,
                                      csvbtUntil = until_}) = do
    start' <- dateAsDay start
    until' <- if T.null $ T.strip until_
                then pure Nothing
                else Just <$> dateAsDay until_
    when (start' > maybe start' id until') (Left "Start date is after end date")
    freq' <- case T.strip freq of
        "" -> Right BNone
        "Hebdomadaire" -> Right BWeekly
        "Mensuel" -> Right BMonthly
        "Annuel" -> Right BYearly
        _ -> Left $ "Unknown frequency: " <> freq
    let interval' = maybe 1 id interval
    pure $ BudgetTarget (textToQname acc) (doubleToAmount amt) cmt start' freq' interval' until'

toCsvBudgetTarget :: BudgetTarget -> CsvBudgetTarget
toCsvBudgetTarget (BudgetTarget {btAccount = acc, btAmount = amt, btComment = cmt, btStart = start, btFrequency = freq, btInterval = interval, btUntil = until_}) =
    let freqText = case freq of
            BNone -> ""
            BWeekly -> "Hebdomadaire"
            BMonthly -> "Mensuel"
            BYearly -> "Annuel"
        until' = maybe "" dayAsDate until_
        interval' = if freq == BNone then Nothing else Just interval
    in CsvBudgetTarget (qnameToText acc) (amountToDouble amt) cmt (dayAsDate start) freqText interval' until'

validateBudgetTarget :: [QName] -> BudgetTarget -> Either Text BudgetTarget
validateBudgetTarget knownQn (BudgetTarget {btAccount = acc, btAmount = amt, btComment = cmt, btStart = start, btFrequency = freq, btInterval = interval, btUntil = until_}) = do
    validateQname acc
    fullQn <- shortNameOf acc knownQn
    case until_ of
        Just duntil_ -> when (start > duntil_) (Left "Start date is after end date")
        Nothing -> pure ()
    pure $ BudgetTarget fullQn amt cmt start freq interval until_

validateBudgetTargets :: [QName] -> [BudgetTarget] -> Either Text [BudgetTarget]
validateBudgetTargets knownQn = traverse (validateBudgetTarget knownQn)

loadCsvBudgetTargets :: FilePath -> ExceptT Text IO [CsvBudgetTarget]
loadCsvBudgetTargets filePath = do
    csvData <- loadFile filePath
    case decodeByName csvData of
        Left err -> throwError $ T.pack err
        Right (_, v) -> pure $ V.toList v

loadBudgetTargets :: FilePath -> ExceptT Text IO [BudgetTarget]
loadBudgetTargets filePath = do
    csvBudgetTargets <- loadCsvBudgetTargets filePath
    liftEither $ traverse fromCsvBudgetTarget csvBudgetTargets

saveBudgetTargets :: FilePath -> [BudgetTarget] -> IO ()
saveBudgetTargets filePath budgetTargets = do
    let csvBudgetTargets = map toCsvBudgetTarget budgetTargets
    BL.writeFile filePath $ encodeDefaultOrderedByName csvBudgetTargets

-- | Convert a budget target to a list of transactions, potentially infinite
--   The other account is the account to which the amount is transferred
--   TxnId is always 0
targetToTxn :: QName -> BudgetTarget -> [Txn]
targetToTxn otherAcc t =
    let start = btStart t
        freq = btFrequency t
        interval = btInterval t
        until_ = btUntil t
        acc = btAccount t
        amount = btAmount t
        comment = btComment t
        ps1 = Posting acc amount comment "" Nothing
        ps2 = Posting otherAcc (negate amount) comment "" Nothing
        txn = Txn 0 start [ps1, ps2]
        next = case freq of
            BNone -> Nothing
            BWeekly -> Just $ addDays (7 * interval) start
            BMonthly -> Just $ addGregorianMonthsClip interval start
            BYearly -> Just $ addGregorianYearsClip interval start
    in case next of
        Nothing -> [txn]
        Just next' -> case until_ of
                        Nothing -> txn : targetToTxn otherAcc t{btStart = next'}
                        Just until' -> if next' > until' 
                                        then [txn]
                                        else txn : targetToTxn otherAcc t{btStart = next'}