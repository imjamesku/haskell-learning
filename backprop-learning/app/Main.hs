{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Lens (makeLenses)
import GHC.Generics (Generic)
import Lib
import Numeric.Backprop (Backprop, sequenceVar)
import Numeric.LinearAlgebra.Static (L, R)

main :: IO ()
main = someFunc

data Net = N
  { _nWeights1 :: L 20 100,
    _nBias1 :: R 20,
    _nWeights2 :: L 5 20,
    _nBias2 :: R 5
  }
  deriving (Show, Generic)

instance Backprop Net

makeLenses ''Net