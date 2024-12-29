import Test.Tasty
import Journal (journalTests)
import Account (accountTests)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "All tests" [accountTests, journalTests]