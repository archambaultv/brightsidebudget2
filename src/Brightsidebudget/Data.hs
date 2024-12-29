module Brightsidebudget.Data
    ( 
    QName,
    Amount,
    Txn(..),
    Posting(..),
    AssertionType(..),
    BAssertion(..),
    BTarget(..),
    BFrequency(..),
    CsvAccountHeader(..),
    CsvTxn(..),
    CsvTxnHeader(..),
    CsvAssertionHeader(..),
    CsvBTargetHeader(..)
    ) where

import Data.Text (Text)
import Data.Time.Calendar (Day)

type QName = [Text]

type Amount = Integer

data Txn = Txn {
    txnId :: !Int,
    txnDate :: !Day,
    txnPostings :: ![Posting]
} deriving (Show)

data Posting = Posting {
    pAccount :: !QName,
    pAmount :: !Amount,
    pComment :: !Text,
    pStmtDesc :: !Text,
    pStmtDate :: !Day
} deriving (Show)

data CsvTxn = CsvTxn {
    csvtId :: !Int,
    csvtDate :: !Day,
    csvtAccount :: !Text,
    csvtAmount :: !Double,
    csvtComment :: !Text,
    csvtStmtDesc :: !Text,
    csvtStmtDate :: !Day
} deriving (Show)

data AssertionType = BalanceAssertion Day
                   | FlowAssertion Day Day
    deriving (Show, Eq)

data BAssertion = BAssertion {
    baType :: AssertionType,
    baAccount :: !QName,
    baAmount :: !Amount
} deriving (Show)

data CsvBAssertion = CsvBAssertion {
    csvDate1 :: !Day,
    csvbaAccount :: !Text,
    csvbaAmount :: !Double,
    csvDate2 :: !Day
} deriving (Show)

data BTarget = BTarget {
    btStart :: !Day,
    btAccount :: !QName,
    btAmount :: !Amount,
    btFrequency :: !BFrequency,
    btInterval :: !Int,
    btUntil :: !Day
} deriving (Show)

data BFrequency = BWeekly | BMonthly | BYearly deriving (Show)

data Journal = Journal {
    jAccounts :: ![QName],
    jTxns :: ![Txn],
    jAssertions :: ![BAssertion],
    jTargets :: ![BTarget]
} deriving (Show)

data CsvAccountHeader = CsvAccountHeader {
    acchAccount :: !Text
} deriving (Show)

data CsvTxnHeader = CsvTxnHeader {
    thId :: !Text,
    thDate :: !Text,
    thAccount:: !Text,
    thAmount :: !Text,
    thComment :: !Text,
    thStmtDesc :: !Text,
    thStmtDate :: !Text
} deriving (Show)

data CsvAssertionHeader = CsvAssertionHeader {
    ahDate :: !Text,
    ahAccount :: !Text,
    ahAmount :: !Text,
    ahDate2 :: !Text
} deriving (Show)

data CsvBTargetHeader = CsvBTargetHeader {
    cbthStart :: !Text,
    cbthAccount :: !Text,
    cbthAmount :: !Text,
    cbthFrequency :: !Text,
    cbthInterval :: !Text,
    cbthUntil :: !Text
} deriving (Show)