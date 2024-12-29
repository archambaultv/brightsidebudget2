module Brightsidebudget.Assertion
    ( csvAssertionToAssertion,
      assertionToCsvAssertion
    , validateAssertion
    )
where

import Data.Text (Text)
import Brightsidebudget.Data (Assertion(..), AssertionType(..), QName, CsvAssertion(..))
import Brightsidebudget.Account (textToQname, validateQname, shortNameOf, qnameToText)
import Brightsidebudget.Amount (doubleToAmount, amountToDouble)
import Brightsidebudget.Calendar (dateAsDay, dayAsDate)

csvAssertionToAssertion :: CsvAssertion -> Either Text Assertion
csvAssertionToAssertion (CsvAssertion {csvbaDate1 = d1, csvbaAccount = acc, csvbaAmount = amt, csvbaDate2 = d2}) = do
    d1' <- dateAsDay d1
    d2' <- sequence $ dateAsDay <$> d2
    case d2' of
        Nothing -> pure $ Assertion (BalanceAssertion d1') (textToQname acc) (doubleToAmount amt)
        Just d2'' -> pure $ Assertion (FlowAssertion d1' d2'') (textToQname acc) (doubleToAmount amt)

assertionToCsvAssertion :: Assertion -> CsvAssertion
assertionToCsvAssertion (Assertion {baType = at, baAccount = acc, baAmount = amt}) =
    let (d1, d2) = case at of
            BalanceAssertion d -> (dayAsDate d, Nothing)
            FlowAssertion d1' d2' -> (dayAsDate d1', Just $ dayAsDate d2')
    in CsvAssertion d1 (qnameToText acc) (amountToDouble amt) d2

validateAssertion :: [QName] -> Assertion -> Either Text Assertion
validateAssertion knownQn (Assertion {baType = at, baAccount = acc, baAmount = amt}) = do
    validateQname acc
    fullQn <- shortNameOf acc knownQn
    pure $ Assertion at fullQn amt