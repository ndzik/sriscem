module CPU where

-- CPU implements the basic CPU type and a description of a coresponding DSL
-- for assembly programs.
import           Data.Array                    as A
import           Data.Vector.Unboxed           as V
import           ASM

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
