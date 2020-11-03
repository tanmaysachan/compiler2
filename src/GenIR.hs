module GenIR where

import IR
import Syntax
import qualified Data.Map.StringMap as SM

data Program inst ret = Program {
    bbentry :: BlockID inst ret
  , bbmap   :: SM.StringMap (BlockID inst ret)
} deriving (Show)

type ParseTree = [Expr]


buildBlocks :: Program -> ParseTree -> Program
buildBlocks prog (expr:tree) = do
                    let bbmap_ = break expr
