module Brightsidebudget.Report.Data (
    PostingLine(..)
)

where

import qualified Data.HashMap.Strict as HM
import Brightsidebudget.Journal (Txn(..), Posting(..), QName)

data PostingLine = PostingLine {
    plTxn :: Txn,
    plPosting :: Posting,
    plShortNames :: HM.HashMap QName QName
}