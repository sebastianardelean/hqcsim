module Quantum.QDataTypes
  (
    State
  , Gate
  ) where

import qualified Numeric.LinearAlgebra as LA


type State = LA.Vector (LA.Complex Double)

type Gate = LA.Matrix (LA.Complex Double)

