import Test.Tasty
import Journal (journalTests)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "All tests" [journalTests]