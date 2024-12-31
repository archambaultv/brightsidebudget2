module Brightsidebudget.Report.Data (
    PostingLine(..)
)

where

import Brightsidebudget.Journal (Txn(..), Posting(..), QName)

data PostingLine = PostingLine {
    plTxn :: Txn,
    plPosting :: Posting,
    plShortName :: QName
}