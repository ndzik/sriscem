module ASM where

import           Data.Array                     ( Array )
import           Data.Vector.Unboxed            ( Vector )
import           Data.Word

type ROM = Array Int Instruction
type Program = [Instruction]
type Stack = Vector Value
type Flag = Bool
type Value = Word64
type Register = (RegType, Value)
data RegType = PC | SP | RA | RB | RC | RD deriving (Show, Eq)
data OPRAND = Reg RegType | Val Value deriving (Show, Eq)

data Instruction = ADD RegType OPRAND
                 | SUB RegType OPRAND
                 | MOV RegType OPRAND
                 | JMP OPRAND
                 | JNZ OPRAND
                 | PSH OPRAND
                 | POP RegType
                 | FIN
                 deriving (Show, Eq)
