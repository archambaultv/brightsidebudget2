module Report.Txns (
    txnsTests
)
where


import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString.Lazy as BL
import Data.Csv (encode)
import Control.Monad.IO.Class (liftIO)
import Brightsidebudget.Report (PostingLine, toPostingLines, postingLineHeader, postingLineToText, postingLineMaxAccDepth)
import Brightsidebudget.Journal (loadAndValidateJournal)
import Journal.Journal (myRunExceptT, config)

txnsTests :: TestTree
txnsTests = testGroup "Report Txns" [exportTxns]

exportTxns :: TestTree
exportTxns = testCase "loadJournal" $ myRunExceptT $ do
    journal <- loadAndValidateJournal config
    let pl = toPostingLines (\_ -> 1) journal :: [PostingLine]
    let maxDepth = postingLineMaxAccDepth pl
    let header = postingLineHeader maxDepth
    let xs = header : map (postingLineToText maxDepth) pl
    liftIO $ BL.writeFile "test/output/report_txns.csv" $ encode xs