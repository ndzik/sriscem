module SCPU where

import           Control.Monad.ST
import           Data.STRef
import           Data.Array                     ( Array
                                                , listArray
                                                )
import           Data.Vector.Unboxed.Mutable
import           Data.Vector.Unboxed
import           ASM

-- Mutable `SCPU` allowing for inplace memory updates.
newtype CPU s = CPU (STRef s (CPU_ s))

data CPU_ s = CPU_ { pc :: STRef s Register
                   , sp :: STRef s Register
                   , ra :: STRef s Register
                   , rb :: STRef s Register
                   , rc :: STRef s Register
                   , rd :: STRef s Register
                   , z :: STRef s Flag
                   , rom :: ROM
                   , stack :: STVector s Value
                   }

data C = C { _pc :: Register
           , _sp :: Register
           , _ra :: Register
           , _rb :: Register
           , _rc :: Register
           , _rd :: Register
           , _z :: Flag
           , _rom :: ROM
           , _stack :: Vector Value
           } deriving Show

freezeCPU :: CPU s -> ST s C
freezeCPU p_cpu = do
  cpu <- readRef p_cpu
  p   <- readSTRef $ pc cpu
  s   <- readSTRef $ sp cpu
  a   <- readSTRef $ ra cpu
  b   <- readSTRef $ rb cpu
  c   <- readSTRef $ rc cpu
  d   <- readSTRef $ rd cpu
  zz  <- readSTRef $ z cpu
  st  <- freeze $ stack cpu
  return C { _pc    = p
           , _sp    = s
           , _ra    = a
           , _rb    = b
           , _rc    = c
           , _rd    = d
           , _z     = zz
           , _rom   = rom cpu
           , _stack = st
           }

-- newCPU creates a reference to a mutable cpu type contained in a
-- `StateTransformer`. The stack size of this CPU is fixed to 256-Word64
-- so 2048 bytes.
newCPU :: Program -> ST s (CPU s)
newCPU p = do
  pcv <- newSTRef (PC, 0)
  spv <- newSTRef (SP, 0)
  rav <- newSTRef (RA, 0)
  rbv <- newSTRef (RB, 0)
  rcv <- newSTRef (RC, 0)
  rdv <- newSTRef (RD, 0)
  zv  <- newSTRef True
  sv  <- new 256
  cpu <- newSTRef CPU_ { pc    = pcv
                       , sp    = spv
                       , ra    = rav
                       , rb    = rbv
                       , rc    = rcv
                       , rd    = rdv
                       , z     = zv
                       , rom   = listArray (0, Prelude.length p - 1) p
                       , stack = sv
                       }
  return $ CPU cpu

readRef :: CPU s -> ST s (CPU_ s)
readRef (CPU scpu) = readSTRef scpu

--writeRef :: CPU s -> CPU_ s -> ST s ()
--writeRef (CPU stcpu) = writeSTRef stcpu

--newRef :: CPU_ s -> ST s (CPU s)
--newRef scpu = CPU <$> newSTRef scpu
