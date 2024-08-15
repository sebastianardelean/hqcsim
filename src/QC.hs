{-|
 -Module      : QC
 -Description : Quantum Computing
 -Copyright   : (c) Mihai Sebastian Ardelean, 2024
 -License     : BSD3
 -Maintainer  : ardeleanasm@gmail.com
 -Portability : POSIX
 
 This module is used to import needed modules for Quantum Computing.
 
 -}
module QC(
  module Quantum.QDataTypes
  , module Quantum.Gates
  , module Quantum.QProgram
         ) where

import Quantum.QDataTypes
import Quantum.Gates
import Quantum.QProgram
