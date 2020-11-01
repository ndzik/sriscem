# sriscem

**S**uper **R**educed **I**nstruction **S**et **EM**ulator is intended to be a
medium sized __kata__.

## Instructions

The instructions are limited to the following:
  * ADD REG       {REG|VAL}
  * SUB REG       {REG|VAL}
  * MOV REG       {REG|VAL}
  * JMP {REG|VAL}
  * JNZ {REG|VAL}
  * PSH {REG|VAL}
  * POP REG
  * FIN

The CPU should use four general purpose registers (**RA**, **RB**, **RC**, **RD**).

## Goal

One should be able to write a program in the given assembly language and have
it as an input to this **sriscem**. The result should be returned in **RA** when
the emulation is done.

## This Repository...

...holds my take on implementing the outlined **sriscem** in beautiful  Haskell.
