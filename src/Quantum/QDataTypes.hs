{-|
 -Module      : QDataTypes
 -Description : Definitions of datatypes for quantum state and quantum gates.
 -Copyright   : (c) Mihai Sebastian Ardelean, 2024
 -License     : BSD3
 -Maintainer  : ardeleanasm@gmail.com
 -Portability : POSIX
 -}
module Quantum.QDataTypes
  (
    State
  , Gate
  ) where

import qualified Numeric.LinearAlgebra as LA

-- | 
-- The `State` type is an alias for a vector of complex numbers.
--
-- In the context of quantum computing, a `State` represents a quantum state
-- as a column vector where each element is a complex number. The length of
-- the vector is typically \(2^n\) for a system of `n` qubits.
--
-- For example, a `State` could be:
-- 
-- @
--  [1 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0]  -- Represents |0⟩
-- @
type State = LA.Vector (LA.Complex Double)

-- |
-- The `Gate` type is an alias for a matrix of complex numbers.
--
-- In quantum computing, a `Gate` is a unitary matrix that represents a quantum operation
-- applied to qubits. The matrix elements are complex numbers.
--
-- The `Gate` type is used to describe quantum gates in algorithms. For example:
--
-- @
-- -- Represents a 2x2 Hadamard Gate
-- hGate :: Gate
-- hGate = (2LA.><2)
--   [1/sqrt 2,1/sqrt 2,1/sqrt 2,(-1)/sqrt 2] :: Gate
--
-- @
type Gate = LA.Matrix (LA.Complex Double)

