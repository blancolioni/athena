module Escape where

data Vector = Vector (Float, Float, Float)

scanSignals :: IO [Signal]

getSignals :: [Signal -> Bool] -> IO [Signal]
getSignals = scanSignals >>= return . filter 
main :: IO ()
main = do
    signals <- getSignals [isHostile, isActive, isMilitary]
