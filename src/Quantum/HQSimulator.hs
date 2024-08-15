module Quantum.HQSimulator
  (
    HQSimulator(..)
  ) where


import Quantum.QProgram

class HQSimulator a b where
  runSimulation :: a -> b -> IO b


instance HQSimulator QProgram Machine
  runSimulation
