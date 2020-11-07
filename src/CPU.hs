module CPU where

-- CPU implements the basic CPU type and a description of a coresponding DSL
-- for assembly programs.
import           Data.Array                    as A
import           Data.Vector.Unboxed           as V
import           Data.Word
import           Data.STRef
import           Data.Vector.Unboxed.Mutable   as M

data CPU = CPU { pc :: Register
               , sp :: Register
               , ra :: Register
               , rb :: Register
               , rc :: Register
               , rd :: Register
               , z :: Flag
               , rom :: ROM
               , stack :: Stack
               } deriving Show

data SCPU s = SCPU { mpc :: STRef s Register
                   , msp :: STRef s Register
                   , mra :: STRef s Register
                   , mrb :: STRef s Register
                   , mrc :: STRef s Register
                   , mrd :: STRef s Register
                   , mz :: STRef s Flag
                   , mrom :: ROM
                   , mstack :: STVector s Value
                   }

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

mkCPU :: Program -> CPU
mkCPU p = CPU { pc    = (PC, 0)
              , sp    = (SP, 0)
              , ra    = (RA, 0)
              , rb    = (RB, 0)
              , rc    = (RC, 0)
              , rd    = (RD, 0)
              , z     = True
              , rom   = A.listArray (0, Prelude.length p - 1) p
              , stack = V.empty
              }
