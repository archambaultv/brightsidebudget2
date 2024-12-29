module Brightsidebudget.Amount
    ( 
        doubleToAmount,
        amountToDouble
    )
where

import Brightsidebudget.Data (Amount)

doubleToAmount :: Double -> Amount
doubleToAmount = round . (* 100)

amountToDouble :: Amount -> Double
amountToDouble = (/ 100) . fromIntegral