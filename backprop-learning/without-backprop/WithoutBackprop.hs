{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Lens hiding ((<.>))
import GHC.Generics (Generic)
import Numeric.LinearAlgebra.Static
import Numeric.OneLiner

data Net = N
  { _weights1 :: L 250 784,
    _bias1 :: R 250,
    _weights2 :: L 10 250,
    _bias2 :: R 10
  }
  deriving (Generic)

makeLenses ''Net

instance Num Net where
  (+) = gPlus
  (-) = gMinus
  (*) = gTimes
  negate = gNegate
  abs = gAbs
  signum = gSignum
  fromInteger = gFromInteger

instance Fractional Net where
  (/) = gDivide
  recip = gRecip
  fromRational = gFromRational

runNet :: Net -> R 784 -> R 10
runNet net x = z
  where
    y = logistic $ (net ^. weights1) #> x + (net ^. bias1)
    z = logistic $ (net ^. weights2) #> y + (net ^. bias2)

logistic :: (Floating a) => a -> a
logistic x = 1 / (1 + exp (-x))

softMax :: R 10 -> R 10
softMax x = expx / konst (norm_1 expx)
  where
    expx = exp x

crossEntropy :: R 10 -> R 10 -> Double
crossEntropy target output = -(log output <.> target)

netErr :: R 784 -> R 10 -> Net -> Double
netErr x target net = crossEntropy target (runNet net x)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"