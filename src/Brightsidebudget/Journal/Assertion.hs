{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Journal.Assertion
    ( fromCsvAssertion,
      toCsvAssertion,
      validateAssertion,
      validateAssertions,
      loadAssertions,
      saveAssertions,
      showDate,
    )
where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (unless)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Csv (decodeByName, encodeDefaultOrderedByNameWith)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Control.Monad.Except (ExceptT, throwError, liftEither)
import Brightsidebudget.Utils (loadFile, csvEncodeOptions)
import Brightsidebudget.Journal.Data (Assertion(..), AssertionType(..), QName, CsvAssertion(..))
import Brightsidebudget.Journal.Account (textToQname, validateQname, shortNameOf, qnameToText)
import Brightsidebudget.Journal.Amount (doubleToAmount, amountToDouble)
import Brightsidebudget.Journal.Calendar (dateAsDay, dayAsDate)

fromCsvAssertion :: CsvAssertion -> Either Text Assertion
fromCsvAssertion (CsvAssertion {csvbaDate1 = d1, csvbaAccount = acc, csvbaAmount = amt, csvbaDate2 = d2,
                                csvbaComment = comment}) = do
    d1' <- dateAsDay d1
    if T.null $ T.strip d2
    then pure $ Assertion (BalanceAssertion d1') (textToQname acc) (doubleToAmount amt) comment
    else do 
        d2' <- dateAsDay d2
        pure $ Assertion (FlowAssertion d1' d2') (textToQname acc) (doubleToAmount amt) comment

toCsvAssertion :: Assertion -> CsvAssertion
toCsvAssertion (Assertion {baType = at, baAccount = acc, baAmount = amt, baComment = comment}) =
    let (d1, d2) = case at of
            BalanceAssertion d -> (dayAsDate d, T.empty)
            FlowAssertion d1' d2' -> (dayAsDate d1', dayAsDate d2')
    in CsvAssertion d1 (qnameToText acc) (amountToDouble amt) comment d2

validateAssertion :: [QName] -> Assertion -> Either Text Assertion
validateAssertion knownQn (Assertion {baType = at, baAccount = acc, baAmount = amt, baComment = comment}) = do
    validateQname acc
    fullQn <- shortNameOf acc knownQn
    pure $ Assertion at fullQn amt comment

validateAssertions :: [QName] -> [Assertion] -> Either Text [Assertion]
validateAssertions knownQn assertions = do
    -- Check for duplicate assertions
    let ids = map (\a -> (qnameToText $ baAccount a, baType a)) assertions
    let ids_set = M.fromListWith (+) $ zip ids (repeat (1 :: Int))
    let dups = filter ((> 1) . snd) $ M.toList ids_set
    unless (null dups) (Left $ T.pack $ "duplicate balance assertions: " ++ show dups)
    -- validate each assertion and update the QName
    traverse (validateAssertion knownQn) assertions

loadCsvAssertions :: FilePath -> ExceptT Text IO [CsvAssertion]
loadCsvAssertions filePath = do
    csvData <- loadFile filePath
    case decodeByName csvData of
        Left err -> throwError $ T.pack err
        Right (_, v) -> pure $ V.toList v

loadAssertions :: FilePath -> ExceptT Text IO [Assertion]
loadAssertions filePath = do
    csvAssertions <- loadCsvAssertions filePath
    liftEither $ traverse fromCsvAssertion csvAssertions

saveAssertions :: FilePath -> [Assertion] -> IO ()
saveAssertions filePath assertions = do
    let ordFoo a = (getDate a, baAccount a)
    let csvAssertions = map toCsvAssertion $ sortBy (comparing ordFoo) assertions
    BL.writeFile filePath $ encodeDefaultOrderedByNameWith csvEncodeOptions csvAssertions

    where getDate (Assertion {baType = at}) = 
            case at of
                BalanceAssertion d -> d
                FlowAssertion _ d2 -> d2

showDate :: AssertionType -> Text
showDate (BalanceAssertion d) = dayAsDate d
showDate (FlowAssertion d1 d2) = T.concat [dayAsDate d1, " - ", dayAsDate d2]