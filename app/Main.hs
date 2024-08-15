module Main where

import Control.Monad (replicateM)
import QC


main :: IO ()
main = do
  let machine = Machine (makeQuantumState 10) 0
  let qprog = QProgram [
        (QInstruction hGate [0])
        , (QInstruction hGate [1])
        , (QInstruction hGate [2])
        , (QInstruction hGate [3])
        , (QInstruction hGate [4])
        , (QInstruction hGate [5])
                                        
        ]
  machines <- replicateM 10 (runQProg qprog machine)
  --mapM_ print machines
  let measurementRegisters = map measurementRegister machines
  print measurementRegisters
 -- mapM_ (\a -> putStrLn $ show (measurementRegister a)) machines
  --putStrLn $ "Measured state: " ++ show (measurementRegister result)


