module Brightsidebudget.Journal.Amount
    ( 
        doubleToAmount,
        amountToDouble
    )
where

import Brightsidebudget.Journal.Data (Amount)

doubleToAmount :: Double -> Amount
doubleToAmount = round . (* 100)

amountToDouble :: Amount -> Double
amountToDouble = (/ 100) . fromIntegral