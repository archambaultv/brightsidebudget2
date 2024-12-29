module Brightsidebudget.Assertion
    ( csvAssertionToAssertion
    , validateAssertion
    )
where

import Data.Text (Text)
import Brightsidebudget.Data (Assertion(..), AssertionType(..), QName, CsvAssertion(..))
import Brightsidebudget.Account (textToQname, validateQname, shortNameOf)
import Brightsidebudget.Amount (doubleToAmount)

csvAssertionToAssertion :: CsvAssertion -> Assertion
csvAssertionToAssertion (CsvAssertion {csvbaDate1 = d1, csvbaAccount = acc, csvbaAmount = amt, csvbaDate2 = d2}) =
    case d2 of
        Nothing -> Assertion (BalanceAssertion d1) (textToQname acc) (doubleToAmount amt)
        Just d2' -> Assertion (FlowAssertion d1 d2') (textToQname acc) (doubleToAmount amt)

validateAssertion :: [QName] -> Assertion -> Either Text Assertion
validateAssertion knownQn (Assertion {baType = at, baAccount = acc, baAmount = amt}) = do
    validateQname acc
    fullQn <- shortNameOf acc knownQn
    pure $ Assertion at fullQn amt