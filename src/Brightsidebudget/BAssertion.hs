module Brightsidebudget.BAssertion
    ( csvAssertionToAssertion
    , validateAssertion
    )
where

import Data.Text (Text)
import Brightsidebudget.Data (BAssertion(..), AssertionType(..), QName, CsvBAssertion(..))
import Brightsidebudget.Account (textToQname, validateQname, shortNameOf)
import Brightsidebudget.Amount (doubleToAmount)

csvAssertionToAssertion :: CsvBAssertion -> BAssertion
csvAssertionToAssertion (CsvBAssertion {csvbaDate1 = d1, csvbaAccount = acc, csvbaAmount = amt, csvbaDate2 = d2}) =
    case d2 of
        Nothing -> BAssertion (BalanceAssertion d1) (textToQname acc) (doubleToAmount amt)
        Just d2' -> BAssertion (FlowAssertion d1 d2') (textToQname acc) (doubleToAmount amt)

validateAssertion :: [QName] -> BAssertion -> Either Text BAssertion
validateAssertion knownQn (BAssertion {baType = at, baAccount = acc, baAmount = amt}) = do
    validateQname acc
    fullQn <- shortNameOf acc knownQn
    pure $ BAssertion at fullQn amt