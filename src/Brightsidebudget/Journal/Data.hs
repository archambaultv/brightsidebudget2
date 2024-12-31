{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Journal.Data
    ( 
    Amount,
    QName,
    Account(..),
    CsvAccount(..),
    Txn(..),
    Posting(..),
    CsvTxn(..),
    ABalance,
    AFlow,
    WhichDate(..),
    AssertionType(..),
    Assertion(..),
    CsvAssertion(..),
    BudgetTarget(..),
    BudgetFrequency(..),
    CsvBudgetTarget(..),
    Journal(..),
    JLoadConfig(..),
    JSaveConfig(..)
    ) where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.ByteString.UTF8 (ByteString, fromString)
import Data.Csv (FromNamedRecord(..), ToNamedRecord(..), DefaultOrdered(..), namedRecord, (.=), header, (.:))

type Amount = Integer

type QName = [Text]

data Account = Account {
    aName :: QName,
    aNumber :: Int
} deriving (Show, Eq)

data CsvAccount = CsvAccount {
    csvaName :: Text,
    csvaNumber :: Int
} deriving (Show, Eq)

fieldCompte, fieldNumero :: ByteString
fieldCompte = fromString "Compte"
fieldNumero = fromString "Numéro"  -- wihtout fromString, the UTF8 conversion is not done properly

instance FromNamedRecord CsvAccount where
    parseNamedRecord m = CsvAccount <$> m .: fieldCompte <*> m .: fieldNumero
instance ToNamedRecord CsvAccount where
    toNamedRecord (CsvAccount name number) = namedRecord [
        fieldCompte .= name, fieldNumero .= number]
instance DefaultOrdered CsvAccount where
    headerOrder _ = header [fieldCompte, fieldNumero]

type ABalance = HM.HashMap QName (M.Map Day Amount)
type AFlow = HM.HashMap QName (M.Map Day Amount)

data WhichDate = StmtDate | TxnDate deriving (Show, Eq)

data Txn = Txn {
    txnId :: Int,
    txnDate :: Day,
    txnPostings :: [Posting]
} deriving (Show, Eq)

data Posting = Posting {
    pAccount :: QName,
    pAmount :: Amount,
    pComment :: Text,
    pStmtDesc :: Text,
    pStmtDate :: Maybe Day
} deriving (Show, Eq)

data CsvTxn = CsvTxn {
    csvtId :: Int,
    csvtDate :: Text,
    csvtAccount :: Text,
    csvtAmount :: Double,
    csvtComment :: Text,
    csvtStmtDesc :: Text,
    csvtStmtDate :: Text
} deriving (Show, Eq)

stmt_desc, stmt_date :: ByteString
stmt_desc = fromString "Description du relevé"
stmt_date = fromString "Date du relevé"

instance FromNamedRecord CsvTxn where
    parseNamedRecord m = CsvTxn <$> m .: "No txn" <*> m .: "Date" <*> m .: "Compte" 
                       <*> m .: "Montant" <*> m .: "Commentaire" <*> m .: stmt_desc <*> m .: stmt_date
instance ToNamedRecord CsvTxn where
    toNamedRecord (CsvTxn ident date acct amt cmt sdesc sdate) = namedRecord [
        "No txn" .= ident, "Date" .= date, "Compte" .= acct, "Montant" .= amt,
        "Commentaire" .= cmt, stmt_desc .= sdesc, stmt_date .= sdate]
instance DefaultOrdered CsvTxn where
    headerOrder _ = header ["No txn", "Date", "Compte", "Montant", "Commentaire", stmt_desc, stmt_date]


data AssertionType = BalanceAssertion Day
                   | FlowAssertion Day Day
    deriving (Show, Eq, Ord)

data Assertion = Assertion {
    baType :: AssertionType,
    baAccount :: QName,
    baAmount :: Amount
} deriving (Show, Eq)

data CsvAssertion = CsvAssertion {
    csvbaDate1 :: Text,
    csvbaAccount :: Text,
    csvbaAmount :: Double,
    csvbaDate2 :: Text
} deriving (Show, Eq)

instance FromNamedRecord CsvAssertion where
    parseNamedRecord m = CsvAssertion <$> m .: "Date" <*> m .: "Compte" <*> m .: "Montant" <*> m .: "Date de fin pour flux"
instance ToNamedRecord CsvAssertion where
    toNamedRecord (CsvAssertion date acct amt mdate) = namedRecord [
        "Date" .= date, "Compte" .= acct, "Montant" .= amt, "Date de fin pour flux" .= mdate]
instance DefaultOrdered CsvAssertion where
    headerOrder _ = header ["Date", "Compte", "Montant", "Date de fin pour flux"]

data BudgetTarget = BudgetTarget {
    btAccount :: QName,
    btAmount :: Amount,
    btComment :: Text,
    btStart :: Day,
    btFrequency :: BudgetFrequency,
    btInterval :: Int,
    btUntil :: Maybe Day
} deriving (Show, Eq)

data CsvBudgetTarget = CsvBudgetTarget {
    csvbtAccount :: Text,
    csvbtAmount :: Double,
    csvbtComment :: Text,
    csvbtStart :: Text,
    csvbtFrequency :: Text,
    csvbtInterval :: Int,
    csvbtUntil :: Text
} deriving (Show, Eq)

start_date, frequency :: ByteString
start_date = fromString "Date de début"
frequency = fromString "Fréquence"

instance FromNamedRecord CsvBudgetTarget where
    parseNamedRecord m = CsvBudgetTarget <$> m .: "Compte" <*> m .: "Montant" <*> m .: "Commentaire" 
                                         <*> m .: start_date <*> m .: frequency <*> m .: "Intervalle"
                                         <*> m .: "Date de fin"
instance ToNamedRecord CsvBudgetTarget where
    toNamedRecord (CsvBudgetTarget acct amt cmt start freq interval until_) = namedRecord [
        "Compte" .= acct, "Montant" .= amt, "Commentaire" .= cmt, start_date .= start,
        frequency .= freq, "Intervalle" .= interval, "Date de fin" .= until_]
instance DefaultOrdered CsvBudgetTarget where
    headerOrder _ = header ["Compte", "Montant", "Commentaire", start_date, frequency, "Intervalle", "Date de fin"]


data BudgetFrequency = BWeekly | BMonthly | BYearly deriving (Show, Eq)

data Journal = Journal {
    jAccounts :: [Account],
    jTxns :: [Txn],
    jAssertions :: [Assertion],
    jTargets :: [BudgetTarget]
} deriving (Show, Eq)

data JLoadConfig = JLoadConfig {
    jlAccounts :: FilePath,
    jlTxns :: [FilePath],
    jlAssertions :: Maybe FilePath,
    jlTargets :: Maybe FilePath
} deriving (Show, Eq)

data JSaveConfig = JSaveConfig {
    jsAccounts :: FilePath,
    jsTxns :: Txn -> FilePath,
    jsAssertions :: FilePath,
    jsTargets :: FilePath
}