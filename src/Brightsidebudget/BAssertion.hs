module Brightsidebudget.BAssertion
    ( csvAssertionsToAssertions
    , validateAssertion
    )
where

import Data.Text (Text)
import qualified Data.Text as T
import Brightsidebudget.Data (BAssertion(..), AssertionType(..), QName, CsvBAssertion(..))

csvAssertionToAssertion :: [CsvBAssertion] -> [BAssertion]
csvAssertionToAssertion