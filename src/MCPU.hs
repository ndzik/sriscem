module MCPU where

import           Control.Monad.ST
import           Control.Monad
import           Data.STRef
import           Data.Array
import           Data.Vector.Unboxed.Mutable
import           Data.Vector.Unboxed
import           Data.Word

-- MCPU is a Monad CPU. It allows for inplace updates in a functional style.
--
-- We want to be able to chain computations in the following manner:
-- `writeReg RA 42 >>= add RA 2 >>= incPC >>= setFlags >>= sub RA RB`
-- Each bind operation, has the corresponding CPU instance implicitly
-- available.

newtype SRISC s a = SRISC {
                          runCPU :: CPU s -> ST s a
                          }


-- Reg returns the register of supplied CPU.
type Reg s = CPU s -> STRef s Value
type ROM = Array Int Instruction
type Program = [Instruction]
data RegType = PC | SP | RA | RB | RC | RD deriving (Show, Eq)
type Flag = Bool
type Value = Word64
data Oprand = OV Value | OR RegType deriving (Show, Eq)
data Instruction = ADD RegType Oprand
                 | SUB RegType Oprand
                 | MOV RegType Oprand
                 | JMP Oprand
                 | JNZ Oprand
                 | PSH Oprand
                 | POP RegType
                 | FIN
                 deriving (Show, Eq)

data CPU s = CPU { pc :: STRef s Value
                 , sp :: STRef s Value
                 , ra :: STRef s Value
                 , rb :: STRef s Value
                 , rc :: STRef s Value
                 , rd :: STRef s Value
                 , z :: STRef s Flag
                 , rom :: ROM
                 , stack :: STVector s Value
                 }

data FrozenCPU = FrozenCPU { _pc :: Value
                           , _sp :: Value
                           , _ra :: Value
                           , _rb :: Value
                           , _rc :: Value
                           , _rd :: Value
                           , _z :: Flag
                           , _rom :: ROM
                           , _stack :: Vector Value
                           } deriving Show

instance Functor (SRISC s) where
  fmap f (SRISC g) = SRISC $ g >=> return . f

instance Applicative (SRISC s) where
  pure a = SRISC $ \_ -> return a
  (SRISC f) <*> (SRISC a) = SRISC $ \cpu -> do
    v <- a cpu
    g <- f cpu
    return $ g v

instance Monad (SRISC s) where
  ma >>= fb = SRISC $ \cpu -> do
    v <- runCPU ma cpu
    runCPU (fb v) cpu

-- newCPU creates a reference to a mutable cpu type contained in a
-- `StateTransformer`. The stack size of this CPU is fixed to 256-Word64
-- so 2048 bytes.
newCPU :: Program -> ST s (CPU s)
newCPU p = do
  pcv <- newSTRef 0
  spv <- newSTRef 0
  rav <- newSTRef 0
  rbv <- newSTRef 0
  rcv <- newSTRef 0
  rdv <- newSTRef 0
  zv  <- newSTRef True
  sv  <- new 256
  return CPU { pc    = pcv
             , sp    = spv
             , ra    = rav
             , rb    = rbv
             , rc    = rcv
             , rd    = rdv
             , z     = zv
             , rom   = listArray (0, Prelude.length p - 1) p
             , stack = sv
             }

newSRISC :: CPU s -> SRISC s (CPU s)
newSRISC cpu = SRISC $ \_ -> return cpu
