{-|
 -Module      : Gates
 -Description : Basic Quantum Gates
 -Copyright   : (c) Mihai Sebastian Ardelean, 2024
 -License     : BSD3
 -Maintainer  : ardeleanasm@gmail.com
 -Portability : POSIX
 -}
module Quantum.Gates
  (
    iGate
  , swapGate
  , hGate
  , xGate
  , yGate
  , zGate
  , cNotGate
  , Gate
  )where


import qualified Numeric.LinearAlgebra as LA

import Quantum.QDataTypes

{-|
 -  iGate function represent an Identity Matrix
 
 >>>iGate
 (2><2)
 [ 1.0 :+ 0.0, 0.0 :+ 0.0
 , 0.0 :+ 0.0, 1.0 :+ 0.0 ]
 -}
iGate :: Gate
iGate = LA.ident 2 :: Gate

{-|
 -  swapGate function represent a Swap Gate
 
 >>>swapGate
 (4><4)
 [ 1.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0
 , 0.0 :+ 0.0, 0.0 :+ 0.0, 1.0 :+ 0.0, 0.0 :+ 0.0
 , 0.0 :+ 0.0, 1.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0
 , 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 1.0 :+ 0.0 ]
 -}
swapGate :: Gate
swapGate = (4LA.><4)
    [ 1, 0, 0, 0 
    , 0, 0, 1, 0 
    , 0, 1, 0, 0 
    , 0, 0, 0, 1]::Gate

{-|
 -  hGate function represent a Hadamard Gate
  
  >>>hGate
 (2><2)
 [ 0.7071067811865475 :+ 0.0,    0.7071067811865475 :+ 0.0
 , 0.7071067811865475 :+ 0.0, (-0.7071067811865475) :+ 0.0 ]
 -}
hGate :: Gate
hGate = (2LA.><2) [1/sqrt 2,1/sqrt 2,1/sqrt 2,(-1)/sqrt 2] :: Gate

{-|
 -  yGate function represent a Pauli Y-Gate
 
 >>>yGate
(2><2)
 [ 0.0 :+ 0.0, 0.0 :+ (-1.0)
 , 0.0 :+ 1.0,    0.0 :+ 0.0 ]
 -}
yGate :: Gate
yGate = (2LA.><2) [0.0,0.0LA.:+(-1.0),0.0LA.:+1.0,0.0] :: Gate

{-|
 -  zGate function represent a Pauli Z-Gate
 
 >>>zGate
 (2><2)
 [ 1.0 :+ 0.0,       0.0 :+ 0.0
 , 0.0 :+ 0.0, (-1.0) :+ (-0.0) ]
 -}
zGate :: Gate
zGate = (2LA.><2) [1,0,0,-1] :: Gate

{-|
 -  xGate function represent a Pauli X-Gate
 
 >>>xGate
 (2><2)
 [ 0.0 :+ 0.0, 1.0 :+ 0.0
 , 1.0 :+ 0.0, 0.0 :+ 0.0 ]
 -}
xGate :: Gate
xGate = (2LA.><2) [0,1,1,0] :: Gate

{-|
 -  cNotGate function represent a Controlled-Not Gate
 
 >>>cNotGate
 (4><4)
 [ 1.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0
 , 0.0 :+ 0.0, 1.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0
 , 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 1.0 :+ 0.0
 , 0.0 :+ 0.0, 0.0 :+ 0.0, 1.0 :+ 0.0, 0.0 :+ 0.0 ]
 -}
cNotGate :: Gate
cNotGate = (4LA.><4)
  [1,0,0,0,
   0,1,0,0,
   0,0,0,1
  ,0,0,1,0] :: Gate
