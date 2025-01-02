import Test.Tasty
import Journal.Journal (journalTests)
import Journal.Account (accountTests)
import Journal.BankImport (bankImportTests)
import Report.Txns (txnsTests)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "All tests" [accountTests, journalTests, bankImportTests, txnsTests]