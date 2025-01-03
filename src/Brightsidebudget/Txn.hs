{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Txn
(
    fromCsvTxns,
    validateTxn,
    loadTxns
)
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.Foldable (traverse_)
import qualified Data.Vector as V
import Data.Csv (decodeByName)
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, throwError, liftEither)
import Brightsidebudget.Data (Txn(..), Posting(..), CsvTxn(..), QName)
import Brightsidebudget.Account (validateQname, textToQname, shortNameOf)
import Brightsidebudget.Amount (doubleToAmount)
import Brightsidebudget.Calendar (dateAsDay)
import Brightsidebudget.Utils (loadFile)

fromCsvTxns :: [CsvTxn] -> Either Text [Txn]
fromCsvTxns csvTnxs =
    let byId = groupBy (\a b -> csvtId a == csvtId b)
             $ sortBy (comparing csvtId) csvTnxs
    in traverse fromCsvTxn' byId

fromCsvTxn' :: [CsvTxn] -> Either Text Txn
fromCsvTxn' [] = Left $ T.pack "empty posting list"
fromCsvTxn' csvTnxs@(CsvTxn {csvtId = ident, csvtDate = date}:_) =
    let dates = map csvtDate csvTnxs        
    in do
        unless (all (== date) dates) (Left $ T.pack $ "mismatched dates for txn " ++ show ident)
        d <- dateAsDay date
        ps <- traverse fromCsvPosting csvTnxs
        pure $ Txn {txnId = ident, txnDate = d, txnPostings = ps}

fromCsvPosting :: CsvTxn -> Either Text Posting
fromCsvPosting (CsvTxn {csvtAccount = acct, csvtAmount = amt, csvtComment = cmt, csvtStmtDesc = sdesc, csvtStmtDate = sdate}) = do
    sd <- if T.null $ T.strip sdate
          then pure Nothing
          else Just <$> dateAsDay sdate
    pure $ Posting {
        pAccount = textToQname acct,
        pAmount = doubleToAmount amt,
        pComment = cmt,
        pStmtDesc = sdesc,
        pStmtDate = sd
    }

validateTxn :: [QName] -> Txn -> Either Text Txn
validateTxn _ (Txn _ _ []) = Left "txn has no postings"
validateTxn knownQn (Txn {txnId = ident, txnDate = date, txnPostings = postings}) =
    let txnSum = sum $ map pAmount postings
    in do 
        when (txnSum /= 0) (Left $ T.pack $ "txn " ++ show ident ++ " does not balance")
        traverse_ (validateQname . pAccount) postings
        fullQn <- traverse (flip shortNameOf knownQn . pAccount) postings
        let ps = zipWith (\p qn -> p {pAccount = qn}) postings fullQn
        pure $ Txn ident date ps

loadCsvTxns :: FilePath -> ExceptT Text IO [CsvTxn]
loadCsvTxns fp = do
    csvTxns <- loadFile fp
    case decodeByName csvTxns of
        Left err -> throwError $ T.pack err
        Right (_, v) -> pure $ V.toList v

loadAllCsvTxns :: [FilePath] -> ExceptT Text IO [CsvTxn]
loadAllCsvTxns fps = do
    csvTxns <- traverse loadCsvTxns fps
    pure $ concat csvTxns

loadTxns :: [QName] -> [FilePath] -> ExceptT Text IO [Txn]
loadTxns knownQn fps = do
    csvTxns <- loadAllCsvTxns fps
    txns <- liftEither $ fromCsvTxns csvTxns
    liftEither $ traverse (validateTxn knownQn) txns