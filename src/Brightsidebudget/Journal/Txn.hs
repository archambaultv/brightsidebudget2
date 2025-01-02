{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Journal.Txn
(
    fromCsvTxns,
    toCsvTxns,
    validateTxn,
    validateTxns,
    loadTxns,
    saveTxns,
    saveTxnsMultipleFiles
)
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (groupBy, sortBy, intercalate)
import Data.Ord (comparing)
import Data.Foldable (traverse_)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.Csv (decodeByName, encodeDefaultOrderedByName)
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, throwError, liftEither)
import Brightsidebudget.Journal.Data (Txn(..), Posting(..), CsvTxn(..), QName)
import Brightsidebudget.Journal.Account (validateQname, textToQname, shortNameOf, qnameToText)
import Brightsidebudget.Journal.Amount (doubleToAmount, amountToDouble)
import Brightsidebudget.Journal.Calendar (dateAsDay, dayAsDate)
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

toCsvTxns :: Txn -> [CsvTxn]
toCsvTxns (Txn {txnId = ident, txnDate = date, txnPostings = postings}) = map toCsvTxn postings
    where toCsvTxn (Posting {pAccount = acct, pAmount = amt, pComment = cmt,
                             pStmtDesc = sdesc, pStmtDate = sdate}) =
            let sd = maybe "" dayAsDate sdate
            in CsvTxn {csvtId = ident, csvtDate = dayAsDate date,
                       csvtAccount = qnameToText acct, csvtAmount = amountToDouble amt,
                       csvtComment = cmt, csvtStmtDesc = sdesc, csvtStmtDate = sd}

validateTxn :: [QName] -> Txn -> Either Text Txn
validateTxn _ (Txn _ _ []) = Left "txn has no postings"
validateTxn knownQn (Txn {txnId = ident, txnDate = date, txnPostings = postings}) =
    let txnSum = sum $ map pAmount postings
    in do 
        when (txnSum /= 0) (Left $ errMss txnSum)
        traverse_ (validateQname . pAccount) postings
        fullQn <- traverse (flip shortNameOf knownQn . pAccount) postings
        let ps = zipWith (\p qn -> p {pAccount = qn}) postings fullQn
        pure $ Txn ident date ps

    where errMss :: Integer -> Text
          errMss txnSum = 
            let ps = intercalate "\n" $ map (\p -> "  " ++ show p) postings
            in T.pack $ "txn " ++ show ident ++ " does not balance. Sum is " ++ show txnSum ++ "\n" ++ ps

validateTxns :: [QName] -> [Txn] -> Either Text [Txn]
validateTxns knownQn txns = do
    -- check for duplicate txn ids
    let ids = map txnId txns
    let ids_set = HM.fromListWith (+) $ zip ids (repeat (1 :: Int))
    let dups = filter ((> 1) . snd) $ HM.toList ids_set
    unless (null dups) (Left $ T.pack $ "duplicate txn ids: " ++ show dups)
    -- validate each txn and update the QName
    traverse (validateTxn knownQn) txns

loadCsvTxns :: FilePath -> ExceptT Text IO [CsvTxn]
loadCsvTxns fp = do
    csvTxns <- loadFile fp
    case decodeByName csvTxns of
        Left err -> throwError $ T.pack err
        Right (_, v) -> pure $ V.toList v

loadTxns :: [FilePath] -> ExceptT Text IO [Txn]
loadTxns fps = do
    csvTxns <- traverse loadCsvTxns fps
    liftEither $ concat <$> traverse fromCsvTxns csvTxns

saveTxns :: FilePath -> [Txn] -> IO ()
saveTxns filePath txns = do
    let csvTxns = concatMap toCsvTxns txns
    BL.writeFile filePath $ encodeDefaultOrderedByName csvTxns

saveTxnsMultipleFiles :: (Txn -> FilePath) -> [Txn] -> IO ()
saveTxnsMultipleFiles txnFile txns = do
    let files = map txnFile txns
    let filesTable = HM.fromListWith (++) $ zip files (map (:[]) txns)
    mapM_ (\(file, xs) -> saveTxns file (sortBy (comparing txnId) xs)) (HM.toList filesTable)