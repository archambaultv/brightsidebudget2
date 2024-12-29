{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Txn
(
    csvTxnsToTxns,
    validateTxn
)
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Brightsidebudget.Data (Txn(..), Posting(..), CsvTxn(..), QName)
import Brightsidebudget.Account (validateQname, textToQname, shortNameOf)
import Brightsidebudget.Amount (doubleToAmount)

csvTxnsToTxns :: [CsvTxn] -> Either Text [Txn]
csvTxnsToTxns csvTnxs =
    let byId = groupBy (\a b -> csvtId a == csvtId b)
             $ sortBy (comparing csvtId) csvTnxs
    in sequence $ map csvTxnToTxn' byId

csvTxnToTxn' :: [CsvTxn] -> Either Text Txn
csvTxnToTxn' [] = Left $ T.pack "empty posting list"
csvTxnToTxn' csvTnxs@(CsvTxn {csvtId = ident, csvtDate = date}:_) =
    let dates = map csvtDate csvTnxs
    in if any (/= date) dates
       then Left $ T.pack $ "mismatched dates for txn " ++ show ident
       else Right $ Txn {
            txnId = ident,
            txnDate = date,
            txnPostings = map csvPostingToPosting csvTnxs
        }

csvPostingToPosting :: CsvTxn -> Posting
csvPostingToPosting (CsvTxn {csvtAccount = acct, csvtAmount = amt, csvtComment = cmt, csvtStmtDesc = sdesc, csvtStmtDate = sdate}) =
    Posting {
        pAccount = textToQname acct,
        pAmount = doubleToAmount amt,
        pComment = cmt,
        pStmtDesc = sdesc,
        pStmtDate = sdate
    }

validateTxn :: [QName] -> Txn -> Either Text Txn
validateTxn _ (Txn _ _ []) = Left "txn has no postings"
validateTxn knownQn (Txn {txnId = ident, txnDate = date, txnPostings = postings}) =
    let txnSum = if (sum $ map pAmount postings) /= 0
                 then Left $ T.pack $ "txn " ++ show ident ++ " does not balance"
                 else Right ()
    in do
        txnSum
        sequence_ $ map (validateQname . pAccount) postings
        fullQn <- sequence $ map (flip shortNameOf knownQn . pAccount) postings
        let ps = zipWith (\p qn -> p {pAccount = qn}) postings fullQn
        pure $ Txn ident date ps