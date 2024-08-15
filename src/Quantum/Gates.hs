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

iGate :: Gate
iGate = LA.ident 2 :: Gate

swapGate :: Gate
swapGate = (4LA.><4)
    [ 1, 0, 0, 0 
    , 0, 0, 1, 0 
    , 0, 1, 0, 0 
    , 0, 0, 0, 1]::Gate


hGate :: Gate
hGate = (2LA.><2) [1/sqrt 2,1/sqrt 2,1/sqrt 2,(-1)/sqrt 2] :: Gate

yGate :: Gate
yGate = (2LA.><2) [0.0,0.0LA.:+(-1.0),0.0LA.:+1.0,0.0] :: Gate

zGate :: Gate
zGate = (2LA.><2) [1,0,0,-1] :: Gate

xGate :: Gate
xGate = (2LA.><2) [0,1,1,0] :: Gate

cNotGate :: Gate
cNotGate = (4LA.><4)
  [1,0,0,0,
   0,1,0,0,
   0,0,0,1
  ,0,0,1,0] :: Gate
