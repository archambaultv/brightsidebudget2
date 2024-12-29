{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Data
    ( 
    QName,
    Amount,
    Account(..),
    CsvAccount(..),
    Txn(..),
    CsvTxn(..),
    Posting(..),
    AssertionType(..),
    Assertion(..),
    CsvAssertion(..),
    BudgetTarget(..),
    BudgetFrequency(..),
    ) where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Csv (FromNamedRecord(..), ToNamedRecord(..), DefaultOrdered(..), namedRecord, (.=), header, (.:))

type QName = [Text]

type Amount = Integer

data Account = Account {
    aName :: QName,
    aNumber :: Int
} deriving (Show)

data CsvAccount = CsvAccount {
    csvaName :: Text,
    csvaNumber :: Int
} deriving (Show)

instance FromNamedRecord CsvAccount where
    parseNamedRecord m = CsvAccount <$> m .: "Compte" <*> m .: "Numéro"
instance ToNamedRecord CsvAccount where
    toNamedRecord (CsvAccount name number) = namedRecord [
        "Compte" .= name, "Numéro" .= number]
instance DefaultOrdered CsvAccount where
    headerOrder _ = header ["Compte", "Numéro"]

data Txn = Txn {
    txnId :: Int,
    txnDate :: Day,
    txnPostings :: [Posting]
} deriving (Show)

data Posting = Posting {
    pAccount :: QName,
    pAmount :: Amount,
    pComment :: Text,
    pStmtDesc :: Text,
    pStmtDate :: Day
} deriving (Show)

data CsvTxn = CsvTxn {
    csvtId :: Int,
    csvtDate :: Text,
    csvtAccount :: Text,
    csvtAmount :: Double,
    csvtComment :: Text,
    csvtStmtDesc :: Text,
    csvtStmtDate :: Text
} deriving (Show)

instance FromNamedRecord CsvTxn where
    parseNamedRecord m = CsvTxn <$> m .: "No txn" <*> m .: "Date" <*> m .: "Compte" 
                       <*> m .: "Montant" <*> m .: "Commentaire" <*> m .: "Description du relevé" <*> m .: "Date du relevé"
instance ToNamedRecord CsvTxn where
    toNamedRecord (CsvTxn ident date acct amt cmt sdesc sdate) = namedRecord [
        "No txn" .= ident, "Date" .= date, "Compte" .= acct, "Montant" .= amt,
        "Commentaire" .= cmt, "Description du relevé" .= sdesc, "Date du relevé" .= sdate]
instance DefaultOrdered CsvTxn where
    headerOrder _ = header ["No txn", "Date", "Compte", "Montant", "Commentaire", "Description du relevé", "Date du relevé"]


data AssertionType = BalanceAssertion Day
                   | FlowAssertion Day Day
    deriving (Show, Eq)

data Assertion = Assertion {
    baType :: AssertionType,
    baAccount :: QName,
    baAmount :: Amount
} deriving (Show)

data CsvAssertion = CsvAssertion {
    csvbaDate1 :: Day,
    csvbaAccount :: Text,
    csvbaAmount :: Double,
    csvbaDate2 :: Maybe Day
} deriving (Show)

data BudgetTarget = BudgetTarget {
    btStart :: Day,
    btAccount :: QName,
    btAmount :: Amount,
    btFrequency :: BudgetFrequency,
    btInterval :: Int,
    btUntil :: Day
} deriving (Show)

data BudgetFrequency = BWeekly | BMonthly | BYearly deriving (Show)

data Journal = Journal {
    jAccounts :: [QName],
    jTxns :: [Txn],
    jAssertions :: [Assertion],
    jTargets :: [BudgetTarget]
} deriving (Show)
