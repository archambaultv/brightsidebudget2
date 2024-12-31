module Brightsidebudget.ABalance
    ( 
        dailyFlowMap,
        dailyFlow,
        aBalanceMap,
        aBalanceMapTxn,
        aBalance
    )
where

import Brightsidebudget.Data (Posting(..), Txn(..), ABalance, AFlow, Amount, QName, WhichDate(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Time.Calendar (Day)

-- | Compute the flow of money in and out of accounts for each day
dailyFlowMap :: WhichDate -> [Txn] -> AFlow
dailyFlowMap whichDate txns = foldl updateBalance HM.empty txns
  where
    updateBalance :: ABalance -> Txn -> AFlow
    updateBalance ab txn = foldl (updatePosting (txnDate txn)) ab (txnPostings txn)

    updatePosting :: Day -> AFlow -> Posting -> AFlow
    updatePosting tDate ab posting = 
        let date = case whichDate of
                    StmtDate -> maybe tDate id (pStmtDate posting)
                    TxnDate -> tDate
        in HM.insertWith (M.unionWith (+)) (pAccount posting) (M.singleton date (pAmount posting)) ab

-- | Compute the flow of money in and out of an account for a given day
dailyFlow :: AFlow -> QName -> Day -> Amount
dailyFlow af qname date = M.findWithDefault 0 date $ HM.findWithDefault M.empty qname af

-- | Compute the balance map from the flow map
aBalanceMap :: AFlow -> ABalance
aBalanceMap af = HM.map (snd . M.mapAccum cumul 0) af
    where
        cumul :: Amount -> Amount -> (Amount, Amount)
        cumul prev amount = (prev + amount, prev + amount)

-- | Compute the balance of an account at a given date
aBalance :: ABalance -> QName -> Day -> Amount
aBalance ab qname date = 
    let m = HM.findWithDefault M.empty qname ab
    in case M.splitLookup date m of
        (_, Just amount, _) -> amount
        (m2, Nothing, _) | M.null m2 -> 0
        (m2, Nothing, _) -> snd $ M.findMax m2

aBalanceMapTxn :: WhichDate -> [Txn] -> ABalance
aBalanceMapTxn wdate txns = aBalanceMap $ dailyFlowMap wdate txns