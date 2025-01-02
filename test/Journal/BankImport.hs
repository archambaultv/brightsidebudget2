{-# LANGUAGE OverloadedStrings #-}

module Journal.BankImport (
    bankImportTests
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad.IO.Class (liftIO)
import Data.Time.Calendar (Day)
import Journal.Journal (myRunExceptT, config)
import Brightsidebudget.Journal (BankCsv(..), Posting(..), Txn(..))
import qualified Brightsidebudget.Journal as J

bankImportTests :: TestTree
bankImportTests = testGroup "BankImport" [importBankPosting, createNewTxns]

bankConfig :: BankCsv
bankConfig = BankCsv {
    bcsvFile = "test/fixtures/bank_data.csv",
    bcsvQname = "Actifs" :| ["Compte courant"],
    bcsvDateCol = "Date",
    bcsvAmountCol = Right ("Credit", "Debit"),
    bcsvStmtDescCols = ["Description", "Category"],
    bcsvStmtDateCol = Nothing,
    bcsvDelimiter = ',',
    bcsvRemoveDelimiterFrom = ["An unquoted, comma"],
    bcsvSkipFirstRow = False,
    bcsvEncoding = "ISO-8859-1"
}

classifier :: (Day, Posting) -> Maybe Txn
classifier (d, p) =
    let p2 = p { pAmount = negate (pAmount p),
                 pAccount = "Dépenses" :| ["Autres"],
                 pStmtDate = Nothing}
    in Just $ J.Txn 0 d [p, p2]

importBankPosting :: TestTree
importBankPosting = testCase "importBankPosting" $ myRunExceptT $ do
    bankPS <- J.importBankPosting bankConfig
    liftIO $ assertEqual "Nb of bank postings" 5 (length bankPS)
    let (date1, ps1) = bankPS !! 0
    liftIO $ assertEqual "Bank1 date" (read "2021-01-06") date1
    liftIO $ assertEqual "Bank1 comment" "Super marché âü | Food" (pStmtDesc ps1)

createNewTxns :: TestTree
createNewTxns = testCase "createNewTxns" $ myRunExceptT $ do
    bankPS <- J.importBankPosting bankConfig
    j <- J.loadAndValidateJournal config
    let txns = J.createNewTxns j classifier bankPS (read "2020-12-31")
    liftIO $ assertEqual "Nb of txns" 4 (length txns)