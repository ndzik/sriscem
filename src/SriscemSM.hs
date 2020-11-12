module SriscemSM ( runProg ) where

import           Control.Monad.State
import           Data.Array                    as A
import           Data.Vector.Unboxed           as V
import           CPU
import           ASM

runProg :: Program -> IO Value
runProg p = return . snd . ra $ evalState runCPU (mkCPU p)

runCPU :: State CPU CPU
runCPU = do
  r <- step
  case r of
    Left  ()  -> get
    Right _ -> runCPU

step :: State CPU (Either () CPU)
step = do
  opcode <- fetch
  case opcode of
    FIN -> return $ Left ()
    _   -> Right <$> decodeExec opcode

fetch :: State CPU Instruction
fetch = gets fetchCurOp

fetchCurOp :: CPU -> Instruction
fetchCurOp cpu = rom cpu A.! fromIntegral (snd . pc $ cpu)

decodeExec :: Instruction -> State CPU CPU
decodeExec (ADD rt oprand) = add oprand rt >> incPC >> updateFlag rt
decodeExec (SUB rt oprand) = sub oprand rt >> incPC >> updateFlag rt
decodeExec (MOV rt oprand) = mov oprand rt >> incPC
decodeExec (PSH oprand   ) = psh oprand >> incPC >> incSP
decodeExec (POP rt       ) = pop rt >> incPC >> decSP
decodeExec (JMP oprand) = jmp oprand
decodeExec (JNZ oprand) = get >>= jnz oprand . z

incPC :: State CPU CPU
incPC = add (Val 1) PC

incSP :: State CPU CPU
incSP = add (Val 1) SP

decSP :: State CPU CPU
decSP = sub (Val 1) SP

add :: OPRAND -> RegType -> State CPU CPU
add oprand rt = fetchReg rt >>= combine (+) oprand >>= updateReg

sub :: OPRAND -> RegType -> State CPU CPU
sub oprand rt = fetchReg rt >>= combine (-) oprand >>= updateReg

updateFlag :: RegType -> State CPU CPU
updateFlag rt = viewReg rt >>= (\v -> get >>= (\cpu ->
  if v == 0
     then return cpu {z = True}
     else return cpu {z = False}
                                              )) >>= updateCPU
mov :: OPRAND -> RegType -> State CPU CPU
mov oprand rt = fetchReg rt >>= combine (const id) oprand >>= updateReg

psh :: OPRAND -> State CPU CPU
psh (Reg rt) = viewReg rt >>= psh . Val
psh (Val v ) = get >>= (\cpu -> updateCPU cpu { stack = V.cons v $ stack cpu })

pop :: RegType -> State CPU CPU
pop rt = do
  cpu <- get
  let v = V.head . stack $ cpu in
      updateReg (rt, v) >>= (\cpu' -> updateCPU cpu' { stack = V.tail . stack $ cpu' })

jnz :: OPRAND -> Bool -> State CPU CPU
jnz _ True = incPC
jnz oprand False = decodeExec (JMP oprand)

jmp :: OPRAND -> State CPU CPU
jmp oprand = mov oprand PC

combine :: (Value -> Value -> Value) -> OPRAND -> Register -> State CPU Register
combine f (Reg rt') r = viewReg rt' >>= (\v' -> combine f (Val v') r)
combine f (Val v') (rt, v) = return (rt, f v v')

updateReg :: Register -> State CPU CPU
updateReg r@(rt, _) =
  get
    >>= (\cpu -> case rt of
          RA -> updateCPU cpu { ra = r }
          RB -> updateCPU cpu { rb = r }
          RC -> updateCPU cpu { rc = r }
          RD -> updateCPU cpu { rd = r }
          PC -> updateCPU cpu { pc = r }
          SP -> updateCPU cpu { sp = r }
        )

updateCPU :: CPU -> State CPU CPU
updateCPU cpu = put cpu >> return cpu

fetchReg :: RegType -> State CPU Register
fetchReg = caseOverRegs id (RA, 0)

viewReg :: RegType -> State CPU Value
viewReg = caseOverRegs snd 0

-- caseOverRegs abstracts case matching over a cpu registers. The second
-- parameter is a "hack" to tell ghc what type to return.
caseOverRegs :: (Register -> a) -> a -> RegType -> State CPU a
caseOverRegs f _ rt =
  get
    >>= (\cpu -> case rt of
          RA -> return $ f . ra $ cpu
          RB -> return $ f . rb $ cpu
          RC -> return $ f . rc $ cpu
          RD -> return $ f . rd $ cpu
          PC -> return $ f . pc $ cpu
          SP -> return $ f . sp $ cpu
        )
