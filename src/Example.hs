{-# LANGUAGE NoImplicitPrelude #-}
module Example where

import           Sriscem

-- prog01 describes `2^10 = 2048`.
prog01 :: Program
prog01 =
  [ MOV RA (Val 2)
  , MOV RB (Val 9)
  , ADD RA (Reg RA)
  , SUB RB (Val 1)
  , JNZ (Val 2)
  , FIN
  ]

-- prog02 describes `2*10 = 20`.
prog02 :: Program
prog02 =
  [ MOV RC (Val 2)
  , MOV RB (Val 10)
  , ADD RA (Reg RC)
  , SUB RB (Val 1)
  , JNZ (Val 2)
  , FIN
  ]
