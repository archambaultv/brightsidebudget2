{-# LANGUAGE OverloadedStrings #-}

module Journal.BankImport (
    bankImportTests
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad.IO.Class (liftIO)
import Journal.Journal (myRunExceptT)
import Brightsidebudget.Journal (BankCsv(..), Posting(..))
import qualified Brightsidebudget.Journal as J

bankImportTests :: TestTree
bankImportTests = testGroup "BankImport" [importBankPosting]

bankConfig :: BankCsv
bankConfig = BankCsv {
    bcsvFile = "test/fixtures/bank_data.csv",
    bcsvQname = "Assets" :| ["Compte courant"],
    bcsvDateCol = "Date",
    bcsvAmountCol = Right ("Debit", "Credit"),
    bcsvStmtDescCols = ["Description", "Category"],
    bcsvStmtDateCol = Nothing,
    bcsvDelimiter = ',',
    bcsvRemoveDelimiterFrom = ["An unquoted, comma"],
    bcsvSkipFirstRow = False,
    bcsvEncoding = "ISO-8859-1"
}


importBankPosting :: TestTree
importBankPosting = testCase "importBankPosting" $ myRunExceptT $ do
    bankPS <- J.importBankPosting bankConfig
    liftIO $ assertEqual "Nb of bank postings" 5 (length bankPS)
    let (date1, ps1) = bankPS !! 0
    liftIO $ assertEqual "Bank1 date" (read "2021-01-06") date1
    liftIO $ assertEqual "Bank1 comment" "Super marché âü | Food" (pStmtDesc ps1)