module Course.IR where

import Data.Int (Int64)
import Course.Syntax (Name, Op)

type Slot = Int
type Label = Int

data Instruction
  = Constant Slot Int64
  | Copy Slot Slot
  | Operation Slot Op Slot Slot
  | Call Slot Name [Slot]
  | Branch Slot Label Label
  | Jump Label
  | Mark Label
  | Return Slot
  | TailCall [Slot]
  deriving (Eq, Show)

data FunctionIR = FunctionIR
  { irName :: Name
  , irParameters :: [Slot]
  , irSlotCount :: Int
  , irInstructions :: [Instruction]
  } deriving (Eq, Show)
