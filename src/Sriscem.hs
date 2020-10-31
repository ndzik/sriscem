{-# LANGUAGE NoImplicitPrelude #-}
module Sriscem where

-- Naive approach, does not easily allow conditinoal jumps.
import           Prelude                        ( Int
                                                , undefined
                                                , error
                                                , (+)
                                                , (-)
                                                , (.)
                                                , ($)
                                                , const
                                                , flip
                                                )
import           Data.Array                    as A
import           Data.Vector                   as V

data CPU = CPU { pc :: Register
               , sp :: Register
               , ra :: Register
               , rb :: Register
               , rc :: Register
               , rd :: Register
               , rom :: ROM
               , stack :: Stack
               }

type ROM = Array Int Instruction
type Stack = Vector Int
data Register = Register RegType Int
data RegType = PC | SP | RA | RB | RC | RD
data OPRAND = Reg Register | Val Int

data Instruction = ADD Register OPRAND
                 | SUB Register OPRAND
                 | MOV Register OPRAND
                 | JMP OPRAND
                 | JNZ OPRAND
                 | PSH OPRAND
                 | POP Register
                 | FIN

step :: Instruction -> CPU -> CPU
step (ADD r oprand) cpu =
  updReg (add (pc cpu) (Val 1)) (updReg (add r oprand) cpu)
step (SUB r oprand) cpu =
  updReg (add (pc cpu) (Val 1)) (updReg (sub r oprand) cpu)
step (MOV r oprand) cpu =
  updReg (add (pc cpu) (Val 1)) (updReg (mov r oprand) cpu)
step (JMP oprand) cpu = updReg (mov (pc cpu) oprand) cpu
step (PSH oprand) cpu =
  (updReg (add (sp cpu) (Val 1)) cpu) { stack = psh (stack cpu) oprand }
step (POP r) cpu =
  (updReg (add (sp cpu) (Val 1)) (pop r cpu)) { stack = V.tail (stack cpu) }

pop :: Register -> CPU -> CPU
pop r@(Register rt _) cpu = case rt of
  RA -> updReg (mov (ra cpu) (Val (V.head . stack $ cpu))) cpu
  RB -> updReg (mov (rb cpu) (Val (V.head . stack $ cpu))) cpu
  RC -> updReg (mov (rc cpu) (Val (V.head . stack $ cpu))) cpu
  RD -> updReg (mov (rd cpu) (Val (V.head . stack $ cpu))) cpu
  PC -> updReg (mov (pc cpu) (Val (V.head . stack $ cpu))) cpu
  SP -> updReg (mov (sp cpu) (Val (V.head . stack $ cpu))) cpu

psh :: Stack -> OPRAND -> Stack
psh s (Reg (Register _ v)) = V.cons v s
psh s (Val v             ) = V.cons v s

updReg :: Register -> CPU -> CPU
updReg r@(Register rt v) cpu = case rt of
  RA -> cpu { ra = r }
  RB -> cpu { rb = r }
  RC -> cpu { rc = r }
  RD -> cpu { rd = r }
  PC -> cpu { pc = r }
  SP -> cpu { sp = r }

mov :: Register -> OPRAND -> Register
mov = process (flip const)

add :: Register -> OPRAND -> Register
add = process (+)

sub :: Register -> OPRAND -> Register
sub = process (-)

process :: (Int -> Int -> Int) -> Register -> OPRAND -> Register
process f r@(Register rt v) oprand = case oprand of
  (Reg (Register rt v')) -> process f r (Val v')
  (Val v'              ) -> Register rt (f v v')
