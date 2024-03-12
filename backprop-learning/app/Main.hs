{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Lens (makeLenses, (^.))
import GHC.Generics (Generic)
import Lib
import Numeric.Backprop
import Numeric.LinearAlgebra.Static.Backprop (L, R, dot, (#>))

main :: IO ()
main = do
  someFunc

funcOnList (sequenceVar -> [x, y, z]) = sqrt (x / y) * z

data Net = N
  { _nWeights1 :: L 20 100,
    _nBias1 :: R 20,
    _nWeights2 :: L 5 20,
    _nBias2 :: R 5
  }
  deriving (Show, Generic)

instance Backprop Net

makeLenses ''Net

-- run net x =
--   let l1 = logistic (net ^. nWeights1 #> x + net ^. nBias1)
--       l2 = logistic (net ^. nWeights2 #> l1 + net ^. nBias2)
--    in l2

runNet :: (Reifies s W) => BVar s Net -> BVar s (R 100) -> BVar s (R 5)
runNet net x = z
  where
    -- run first layer
    y = logistic $ (net ^^. nWeights1) #> x + (net ^^. nBias1)
    -- run second layer
    z = logistic $ (net ^^. nWeights2) #> y + (net ^^. nBias2)

logistic :: (Floating a) => a -> a
logistic x = 1 / (1 + exp (-x))

squaredError target output = error `dot` error
  where
    error = target - output

netError target input net = squaredError (auto target) (runNet net (auto input))