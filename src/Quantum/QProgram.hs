{-|
 -Module      : QProgram
 -Description : Definitions of qprogram datatypes and of simulation function.
 -Copyright   : (c) Mihai Sebastian Ardelean, 2024
 -License     : BSD3
 -Maintainer  : ardeleanasm@gmail.com
 -Portability : POSIX
 -}
module Quantum.QProgram
  (
    runQProg
  , makeQuantumState
  , Machine(..)
  , QInstruction(..)
  , QProgram(..)
  ) where

import Quantum.QDataTypes
import Quantum.Gates

import Data.List (nub, foldl')

import System.Random (randomRIO)

import qualified Numeric.LinearAlgebra as LA 

import Control.Monad (replicateM)

{-|
A `Machine` is defined by the quantum state and the measurement register.

It has two fields:
* `qstate` of type `State`
* `measurementRegister` of type `Int`
-}
data Machine = Machine {
    qstate :: State            -- ^ Quantum state.
  , measurementRegister :: Int -- ^ Measurement register.
  } deriving (Eq, Show)


{-|
A `QInstruction` is defined by the unitary transformation and by the
qubits' index on which the transformation is applied.

It has two fields:
* `gateMatrix` of type `Gate` is the unitary matrix that defines the quantum gate.
* `affectedQubits` of type `[Int]`
-}
data QInstruction = QInstruction {
    gateMatrix ::Gate       -- ^ Quantum gate matrix.
  , affectedQubits :: [Int] -- ^ List of qubits' index that are affected by the quantum gate.
    } deriving (Eq,Show)

{-|
A `QProgram` is defined by the list quantum instructions:
* `instructions` is of type `[QInstruction]`
-}
data QProgram = QProgram {
  instructions :: [QInstruction] -- ^ List of program instructions.
                   } deriving (Eq, Show)

{-|
 - makeQuantumState function initializes a quantum state of `n` qubits
-}
makeQuantumState :: Int -> State
makeQuantumState n = LA.fromList $ 1 : replicate (2 ^ n - 1) 0


dimensionQubits :: Int -> Int
dimensionQubits size = floor $ logBase 2 (fromIntegral size)

apply :: Gate -> State -> State
apply = (LA.#>)

compose :: Gate -> Gate -> Gate
compose = (LA.<>)

kroneckerMul :: Gate -> Gate -> Gate
kroneckerMul a b = LA.kronecker a b

kroneckerExp :: Gate -> Int -> Gate
kroneckerExp gate n
  | n < 1 = (1LA.><1) [1]::Gate
  | n == 1 = gate
  | otherwise = kroneckerMul (kroneckerExp gate (n - 1)) gate


lift :: Gate -> Int -> Int -> Gate
lift gate i n = liftResult
  where
    left = kroneckerExp iGate (n - i - (dimensionQubits $ LA.rows gate))
    right = kroneckerExp iGate i
    liftResult = kroneckerMul left $ kroneckerMul gate right

perm2trans :: [Int] -> [(Int, Int)]
perm2trans permutation = nub (concatMap processIndex [0..length permutation - 1])
  where
    -- Process each index to determine necessary swaps
    updateSrc :: Int -> Int -> [Int] -> Int
    updateSrc src dest lst
      |src >= dest = src
      |otherwise = updateSrc (lst !! src) dest lst
    processIndex dest
      | src < dest = [(src, dest)]
      | src > dest = [(dest,src)]
      | otherwise = []
      where
        originalSrc = permutation !! dest
        src = updateSrc originalSrc dest permutation

    
trans2adj :: [(Int, Int)] -> [Int]
trans2adj transpositions = concatMap expandConsecutive transpositions
  where 
    expandConsecutive :: (Int, Int) -> [Int]
    expandConsecutive (x, y)
      | y - x == 1 = [x]
      | otherwise  = trans ++ reverse (init trans)
      where
        trans = [x..y-1]

apply1Q :: State -> Gate -> Int -> State
apply1Q s u qubit = q1State
  where
    liftedU = lift u qubit (dimensionQubits $ LA.size s)
    q1State = apply liftedU s



applyNQ :: State -> Gate -> [Int] -> State
applyNQ s u qubits = qubitsNState
  where
    swap :: Int -> Int -> Gate
    swap i n = lift swapGate i n

    trans2op :: [Int] -> Int -> Gate
    trans2op [] n = LA.ident (2^n)
    trans2op (t:ts) n = foldl' compose (swap t n) (map (`swap` n) ts)

    n = dimensionQubits $ LA.size s
    u01 = lift u 0 n
    fromSpace = reverse qubits ++ [i | i <- [0..n-1], i `notElem` qubits]
    trans = perm2trans fromSpace
    adj = trans2adj trans
    toFrom = trans2op adj n
    fromTo = trans2op (reverse adj) n
    upq = compose toFrom (compose u01 fromTo)
    qubitsNState = apply upq s

applyGate :: State -> Gate -> [Int] -> State
applyGate s u qubits
  | qubitsLength == 1 = apply1Q s u (qubits !! 0)
  | otherwise         = applyNQ s u qubits
  where
    qubitsLength = length qubits




sample :: State -> IO Int
sample s = do
  r <- randomRIO (0.0, 1.0)
  return $ sampleIndex (LA.toList s) r 0.0
  where
    sampleIndex :: [LA.Complex Double] -> Double -> Double -> Int
    sampleIndex [] _ _ = error "Invalid state vector"
    sampleIndex (c:cs) r accProb =
      let prob = accProb + (LA.magnitude c) ** 2
      in if r < prob
         then 0
         else 1 + sampleIndex cs r prob

collapse :: State -> Int -> State
collapse st i = LA.fromList collapsedState
  where
    stateLength = LA.size st
    collapsedState = replicate i 0 ++ [1] ++ replicate (stateLength - i - 1) 0    

observe :: Machine -> IO Machine
observe machine = do
  let state = qstate machine
  i <- sample state
  let newState = collapse state i
  return machine { qstate = newState, measurementRegister = i}


evolveState :: QInstruction -> Machine ->Machine
evolveState (QInstruction gateMatrix affectedQubits) m = newMachine
  where
    newState = applyGate (qstate m) gateMatrix affectedQubits
    newMachine = Machine newState (measurementRegister m)

{-|
 - runQProg function executes the program's instruction.
-}
runQProg :: QProgram -> Machine -> IO Machine
runQProg qprog machine = runInstruction (instructions qprog) machine
  where
    runInstruction :: [QInstruction] -> Machine -> IO Machine
    runInstruction [] m = observe m
    runInstruction (x:xs) m = runInstruction xs (evolveState x m)

