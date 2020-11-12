# sriscem

**S**uper **R**educed **I**nstruction **S**et **Interpreter** and is intended to be
a medium sized __kata__.

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

You can either think of real `opcode`s, which are parsed by your interpreter or
try an eDSL for the given ASM syntax.

## This Repository...

...holds my take on implementing the outlined **sriscem** in **Haskell**. It
contains multiple ways of realizing such a `CPU`, from obvious, verbose and pure
functional style, to `ST` & `State` Monads.
