{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Budget
    (
      fromCsvBudgetTarget,
      toCsvBudgetTarget,
      validateBudgetTarget,
      loadBudgetTargets
    )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Csv (decodeByName)
import qualified Data.Vector as V
import Control.Monad (when)
import Control.Monad.Except (ExceptT, throwError, liftEither)
import Brightsidebudget.Utils (loadFile)
import Brightsidebudget.Data (BudgetTarget(..), CsvBudgetTarget(..), BudgetFrequency(..), QName)
import Brightsidebudget.Account (textToQname, validateQname, shortNameOf, qnameToText)
import Brightsidebudget.Amount (doubleToAmount, amountToDouble)
import Brightsidebudget.Calendar (dateAsDay, dayAsDate)

fromCsvBudgetTarget :: CsvBudgetTarget -> Either Text BudgetTarget
fromCsvBudgetTarget (CsvBudgetTarget {csvbtAccount = acc, csvbtAmount = amt, csvbtComment = cmt, csvbtStart = start, csvbtFrequency = freq, csvbtInterval = interval, csvbtUntil = until_}) = do
    start' <- dateAsDay start
    until' <- dateAsDay until_
    freq' <- case freq of
        "Hebdomadaire" -> Right BWeekly
        "Mensuel" -> Right BMonthly
        "Annuel" -> Right BYearly
        _ -> Left $ "Unknown frequency: " <> freq
    pure $ BudgetTarget (textToQname acc) (doubleToAmount amt) cmt start' freq' interval until'

toCsvBudgetTarget :: BudgetTarget -> CsvBudgetTarget
toCsvBudgetTarget (BudgetTarget {btAccount = acc, btAmount = amt, btComment = cmt, btStart = start, btFrequency = freq, btInterval = interval, btUntil = until_}) =
    let freqText = case freq of
            BWeekly -> "Hebdomadaire"
            BMonthly -> "Mensuel"
            BYearly -> "Annuel"
    in CsvBudgetTarget (qnameToText acc) (amountToDouble amt) cmt (dayAsDate start) freqText interval (dayAsDate until_)

validateBudgetTarget :: [QName] -> BudgetTarget -> Either Text BudgetTarget
validateBudgetTarget knownQn (BudgetTarget {btAccount = acc, btAmount = amt, btComment = cmt, btStart = start, btFrequency = freq, btInterval = interval, btUntil = until_}) = do
    validateQname acc
    fullQn <- shortNameOf acc knownQn
    when (start > until_) (Left "Start date is after end date")
    pure $ BudgetTarget fullQn amt cmt start freq interval until_

loadCsvBudgetTargets :: FilePath -> ExceptT Text IO [CsvBudgetTarget]
loadCsvBudgetTargets filePath = do
    csvData <- loadFile filePath
    case decodeByName csvData of
        Left err -> throwError $ T.pack err
        Right (_, v) -> pure $ V.toList v

loadBudgetTargets :: [QName] -> FilePath -> ExceptT Text IO [BudgetTarget]
loadBudgetTargets knownQn filePath = do
    csvBudgetTargets <- loadCsvBudgetTargets filePath
    targets <- liftEither $ traverse fromCsvBudgetTarget csvBudgetTargets
    liftEither $ traverse (validateBudgetTarget knownQn) targets