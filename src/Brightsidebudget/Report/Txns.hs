{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Report.Txns
    ( 
        toPostingLines,
        postingLineMaxAccDepth,
        postingLineHeader,
        postingLineToText
    )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.HashMap.Strict (HashMap)
import Data.Time.Calendar (showGregorian)
import qualified Data.HashMap.Strict as HM
import Brightsidebudget.Journal (Journal(..), dayAsDate, Txn(..), Posting(..), qnameToText,
    amountToDouble, QName, Account(..), toShortNames)
import Brightsidebudget.Report.Data (PostingLine(..))

toPostingLines :: (QName -> Int) -> Journal -> [PostingLine]
toPostingLines qnameLength j =
    let txns = jTxns j
        names = map aName $ jAccounts j
        shortQnames = toShortNames qnameLength names 
        shortMap = HM.fromList $ zip names shortQnames
    in concatMap (fromTxn shortMap) txns

    where 
        fromTxn :: HashMap QName QName -> Txn -> [PostingLine]
        fromTxn m txn = map (\p -> PostingLine txn p m) (txnPostings txn)

postingLineMaxAccDepth :: [PostingLine] -> Int
postingLineMaxAccDepth = maximum . map (length . pAccount . plPosting)

postingLineHeader :: Int -> [Text]
postingLineHeader maxDepth =
    let comptes = map (\i -> T.pack $ "Compte " ++ show i) [1..maxDepth]
    in 
     ["No txn", "Date", "Compte", "Compte nom court", "Montant", "Commentaire",
      "Description du relevé", "Date du relevé"] 
     ++ comptes
     ++ ["Année", "Mois", "Années-mois", "Txn comptes"]
 
postingLineToText :: Int -> PostingLine -> [Text]
postingLineToText maxDepth pl =
    let namesMap = plShortNames pl
        txn = plTxn pl
        posting = plPosting pl
        acc = pAccount posting
        shortAcc = qnameToText $ namesMap HM.! acc
        amount = T.pack $ show $ amountToDouble $ pAmount posting
        comment = pComment posting
        statementDesc = pStmtDesc posting
        statementDate = maybe "" (T.pack . show . dayAsDate) $ pStmtDate posting
        date = dayAsDate $ txnDate txn
        txnNo = T.pack $ show $ txnId txn
        accParts = NE.take maxDepth acc ++ replicate (maxDepth - length acc) ""
        dateStr = showGregorian $ txnDate txn
        year = T.pack $ take 4 dateStr
        month = T.pack $ take 2 $ drop 5 dateStr
        yearMonth = year <> "-" <> month
        txnAccounts = T.intercalate " | " $ nub $ map (qnameToText . (namesMap HM.!) . pAccount) (txnPostings txn)
    in [ txnNo, date, qnameToText acc, shortAcc, amount, comment, statementDesc, statementDate ]
       ++ accParts
       ++ [ year, month, yearMonth, txnAccounts ]