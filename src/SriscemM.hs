module SriscemM
  ( runProg
  )
where

-- SriscemM implements a sriscem using `Control.Monad.State` and
-- `Monad Transformers`.
-- In hindsight, it would've been WAY better to either make a monad out of
-- the `CPU` itself, or encapsulate it in `Control.Monad.State`. This takes off
-- the burden of passing all the `CPU` references around.
import           Control.Monad.State
import           Control.Monad.ST
import           Data.STRef
import           Data.Array                    as A
import           Data.Vector.Unboxed.Mutable   as M
import           SCPU
import           ASM

runProg :: Program -> IO Value
runProg p = do
  cpu  <- stToIO $ run $ newCPU p
  fcpu <- stToIO $ freezeCPU cpu
  return $ snd . _ra $ fcpu

run :: ST s (CPU s) -> ST s (CPU s)
run scpu = do
  p_cpu  <- scpu
  opcode <- fetchCurOp p_cpu
  if opcode == FIN then return p_cpu else run $ decodeExec opcode p_cpu


fetchCurOp :: CPU s -> ST s Instruction
fetchCurOp cpu = do
  p_cpu <- readRef cpu
  reg   <- readSTRef $ pc p_cpu
  return $ rom p_cpu A.! (fromIntegral . snd $ reg)

decodeExec :: Instruction -> CPU s -> ST s (CPU s)
decodeExec (ADD rt oprand) p_cpu =
  add oprand rt p_cpu >>= setFlags rt >>= incPC
decodeExec (SUB rt oprand) p_cpu =
  sub oprand rt p_cpu >>= setFlags rt >>= incPC
decodeExec (MOV rt oprand) p_cpu = mov oprand rt p_cpu >>= incPC
decodeExec (PSH oprand   ) p_cpu = psh p_cpu oprand >>= incPC
decodeExec (POP rt       ) p_cpu = pop p_cpu rt >>= incPC
decodeExec (JMP oprand   ) p_cpu = jmp p_cpu oprand
decodeExec (JNZ oprand   ) p_cpu = jnz p_cpu oprand

add :: OPRAND -> RegType -> CPU s -> ST s (CPU s)
add = combine (+)

sub :: OPRAND -> RegType -> CPU s -> ST s (CPU s)
sub = combine (flip (-))

mov :: OPRAND -> RegType -> CPU s -> ST s (CPU s)
mov = combine const

jmp :: CPU s -> OPRAND -> ST s (CPU s)
jmp p_cpu (Reg rt) = do
  v <- readRegVal p_cpu rt
  jmp p_cpu (Val v)
jmp p_cpu (Val v) = do
  p_pc <- readRegRef p_cpu PC
  writeSTRef p_pc (PC, v)
  return p_cpu

jnz :: CPU s -> OPRAND -> ST s (CPU s)
jnz p_cpu oprand = do
  cpu <- readRef p_cpu
  zb  <- readSTRef $ z cpu
  if zb then incPC p_cpu else jmp p_cpu oprand


setFlags :: RegType -> CPU s -> ST s (CPU s)
setFlags rt p_cpu = do
  res <- (== 0) <$> readRegVal p_cpu rt
  cpu <- readRef p_cpu
  writeSTRef (z cpu) res
  return p_cpu

incSP :: CPU s -> ST s (CPU s)
incSP = add (Val 1) SP

decSP :: CPU s -> ST s (CPU s)
decSP = sub (Val 1) SP

incPC :: CPU s -> ST s (CPU s)
incPC = add (Val 1) PC

-- The usage of `STVector` as a "stack" in the given way does not make much
-- sense so we will simply regard this impl as a proof of concept and educational.
psh :: CPU s -> OPRAND -> ST s (CPU s)
psh p_cpu (Reg rt) = do
  p_reg  <- readRegRef p_cpu rt
  (_, v) <- readSTRef p_reg
  psh p_cpu (Val v)
psh p_cpu (Val v) = do
  cpu <- readRef p_cpu
  sp' <- readSTRef $ sp cpu
  let pos = fromIntegral $ snd sp'
  write (stack cpu) pos v
  incSP p_cpu

pop :: CPU s -> RegType -> ST s (CPU s)
pop p_cpu rt = do
  pos <- readRegVal p_cpu SP
  cpu <- readRef p_cpu
  v   <- M.read (stack cpu) (fromIntegral pos - 1)
  mov (Val v) rt p_cpu
  decSP p_cpu

combine
  :: (Value -> Value -> Value) -> OPRAND -> RegType -> CPU s -> ST s (CPU s)
combine f (Reg rt') rt p_cpu = do
  v' <- readRegVal p_cpu rt'
  combine f (Val v') rt p_cpu
combine f (Val v) rt p_cpu = do
  p_reg   <- readRegRef p_cpu rt
  (_, v') <- readSTRef p_reg
  writeSTRef p_reg (rt, f v v')
  return p_cpu

readRegRef :: CPU s -> RegType -> ST s (STRef s Register)
readRegRef scpu rt = do
  cpu <- readRef scpu
  case rt of
    RA -> return $ ra cpu
    RB -> return $ rb cpu
    RC -> return $ rc cpu
    RD -> return $ rd cpu
    PC -> return $ pc cpu
    SP -> return $ sp cpu

readRegVal :: CPU s -> RegType -> ST s Value
readRegVal cpu rt = do
  r <- readSTRef <=< readRegRef cpu $ rt
  return $ snd r
