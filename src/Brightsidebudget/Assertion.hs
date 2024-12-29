module Brightsidebudget.Assertion
    ( fromCsvAssertion,
      toCsvAssertion,
      validateAssertion,
      loadAssertions
    )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Csv (decodeByName)
import qualified Data.Vector as V
import Control.Monad.Except (ExceptT, throwError, liftEither)
import Brightsidebudget.Utils (loadFile)
import Brightsidebudget.Data (Assertion(..), AssertionType(..), QName, CsvAssertion(..))
import Brightsidebudget.Account (textToQname, validateQname, shortNameOf, qnameToText)
import Brightsidebudget.Amount (doubleToAmount, amountToDouble)
import Brightsidebudget.Calendar (dateAsDay, dayAsDate)

fromCsvAssertion :: CsvAssertion -> Either Text Assertion
fromCsvAssertion (CsvAssertion {csvbaDate1 = d1, csvbaAccount = acc, csvbaAmount = amt, csvbaDate2 = d2}) = do
    d1' <- dateAsDay d1
    d2' <- traverse dateAsDay d2
    case d2' of
        Nothing -> pure $ Assertion (BalanceAssertion d1') (textToQname acc) (doubleToAmount amt)
        Just d2'' -> pure $ Assertion (FlowAssertion d1' d2'') (textToQname acc) (doubleToAmount amt)

toCsvAssertion :: Assertion -> CsvAssertion
toCsvAssertion (Assertion {baType = at, baAccount = acc, baAmount = amt}) =
    let (d1, d2) = case at of
            BalanceAssertion d -> (dayAsDate d, Nothing)
            FlowAssertion d1' d2' -> (dayAsDate d1', Just $ dayAsDate d2')
    in CsvAssertion d1 (qnameToText acc) (amountToDouble amt) d2

validateAssertion :: [QName] -> Assertion -> Either Text Assertion
validateAssertion knownQn (Assertion {baType = at, baAccount = acc, baAmount = amt}) = do
    validateQname acc
    fullQn <- shortNameOf acc knownQn
    pure $ Assertion at fullQn amt

loadCsvAssertions :: FilePath -> ExceptT Text IO [CsvAssertion]
loadCsvAssertions filePath = do
    csvData <- loadFile filePath
    case decodeByName csvData of
        Left err -> throwError $ T.pack err
        Right (_, v) -> pure $ V.toList v

loadAssertions :: [QName] -> FilePath -> ExceptT Text IO [Assertion]
loadAssertions knownQn filePath = do
    csvAssertions <- loadCsvAssertions filePath
    txns <- liftEither $ traverse fromCsvAssertion csvAssertions
    liftEither $ traverse (validateAssertion knownQn) txns