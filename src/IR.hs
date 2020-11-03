module IR where

import Syntax

-- discriminate on type
data Label a = Label String deriving (Show, Eq)

type BlockID inst ret = Label (BasicBlock inst ret)

-- basic block
data BasicBlock inst ret = BasicBlock {
    bbins   :: [inst]
  , bbret   :: ret
  , bblabel :: BlockID inst ret
  , bbsucc  :: BlockID inst ret
  , bbpred  :: BlockID inst ret
}

data Inst = Alloc
          | Add Int Int
          | Mul Int Int
          | Load Int
          | Store Int Int
