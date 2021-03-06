{-# LANGUAGE NoImplicitPrelude #-}
module Sriscem
  ( runProg
  , step
  , mkCPU
  )
where

import           Prelude                        ( Int
                                                , Eq
                                                , Bool(..)
                                                , IO
                                                , undefined
                                                , error
                                                , (+)
                                                , (-)
                                                , (.)
                                                , ($)
                                                , (==)
                                                , const
                                                , id
                                                , flip
                                                , snd
                                                , fst
                                                , length
                                                , print
                                                , return
                                                , fromIntegral
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Array                    as A
import           Data.Vector.Unboxed           as V
import           CPU
import           ASM

runProg :: Program -> IO Value
runProg p =
  let cpu    = go $ mkCPU p
      (_, v) = ra cpu
  in  return v
 where
  go :: CPU -> CPU
  go cpu =
    let opcode = rom cpu A.! fromIntegral (snd . pc $ cpu)
    in  if opcode == FIN then cpu else go $ step opcode cpu

step :: Instruction -> CPU -> CPU
step (ADD r oprand) cpu =
  nextPC $ setFlag r $ updReg cpu $ add (viewReg r cpu) oprand cpu
step (SUB r oprand) cpu =
  nextPC $ setFlag r $ updReg cpu $ sub (viewReg r cpu) oprand cpu
step (MOV r oprand) cpu = nextPC $ updReg cpu $ mov (viewReg r cpu) oprand cpu
step (PSH oprand) cpu =
  nextPC . nextSP $ cpu { stack = psh (stack cpu) oprand cpu }
step (POP r) cpu = nextPC . prevSP $ (pop r cpu) { stack = V.tail (stack cpu) }
step (JMP oprand) cpu = updReg cpu $ mov (pc cpu) oprand cpu
step (JNZ oprand) cpu =
  let zr = z cpu in if zr then nextPC cpu else step (JMP oprand) cpu

nextPC :: CPU -> CPU
nextPC cpu = cpu { pc = add (pc cpu) (Val 1) cpu }

prevSP :: CPU -> CPU
prevSP cpu = cpu { sp = sub (sp cpu) (Val 1) cpu }

nextSP :: CPU -> CPU
nextSP cpu = cpu { sp = add (sp cpu) (Val 1) cpu }

setFlag :: RegType -> CPU -> CPU
setFlag rt cpu =
  let updZ :: CPU -> Register -> CPU
      updZ cpu (_, v) = if v == 0 then cpu { z = True } else cpu { z = False }
  in  case rt of
        RA -> updZ cpu $ ra cpu
        RB -> updZ cpu $ rb cpu
        RC -> updZ cpu $ rc cpu
        RD -> updZ cpu $ rd cpu

pop :: RegType -> CPU -> CPU
pop rt cpu =
  let v = Val $ V.head . stack $ cpu
  in  case rt of
        RA -> updReg cpu $ mov (ra cpu) v cpu
        RB -> updReg cpu $ mov (rb cpu) v cpu
        RC -> updReg cpu $ mov (rc cpu) v cpu
        RD -> updReg cpu $ mov (rd cpu) v cpu
        PC -> updReg cpu $ mov (pc cpu) v cpu
        SP -> updReg cpu $ mov (sp cpu) v cpu

psh :: Stack -> OPRAND -> CPU -> Stack
psh s (Reg rt) cpu = V.cons (snd $ viewReg rt cpu) s
psh s (Val v ) _   = V.cons v s

updReg :: CPU -> Register -> CPU
updReg cpu r@(rt, _) = case rt of
  RA -> cpu { ra = r }
  RB -> cpu { rb = r }
  RC -> cpu { rc = r }
  RD -> cpu { rd = r }
  PC -> cpu { pc = r }
  SP -> cpu { sp = r }

viewReg :: RegType -> CPU -> Register
viewReg rt cpu = case rt of
  RA -> ra cpu
  RB -> rb cpu
  RC -> rc cpu
  RD -> rd cpu
  PC -> pc cpu
  SP -> sp cpu

mov :: Register -> OPRAND -> CPU -> Register
mov = combine (const id)

add :: Register -> OPRAND -> CPU -> Register
add = combine (+)

sub :: Register -> OPRAND -> CPU -> Register
sub = combine (-)

combine :: (Value -> Value -> Value) -> Register -> OPRAND -> CPU -> Register
combine f r@(rt, v) oprand cpu = case oprand of
  (Reg rt') -> combine f r (Val $ snd $ viewReg rt' cpu) cpu
  (Val v' ) -> (rt, f v v')
