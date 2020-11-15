module SriscemMonad
  ( runProg
  )
where

import           MCPU
import           Data.STRef
import           Data.Array                    as A
import           Control.Monad.ST
import           Data.Vector.Unboxed           as V
import           Data.Vector.Unboxed.Mutable   as M

runProg :: Program -> IO Value
runProg p = do
  let v = runST $ do
        initCPU <- newCPU p
        doneCPU <- runCPU go initCPU
        freezeST doneCPU
  return $ _ra v
 where
  go :: SRISC s (CPU s)
  go =
    fetch
      >>= (\instr -> case instr of
            FIN -> stopCPU
            _   -> decodeExec instr >> go
          )

decodeExec :: Instruction -> SRISC s (CPU s)
decodeExec (ADD rt oprand) =
  readReg rt >>= (`add` oprand) >> setFlags rt >> incPC >> cont
decodeExec (SUB rt oprand) =
  readReg rt >>= (`sub` oprand) >> setFlags rt >> incPC >> cont
decodeExec (MOV rt oprand) = readReg rt >>= (`mov` oprand) >> incPC >> cont
decodeExec (PSH oprand   ) = psh oprand >> incSP >> incPC >> cont
decodeExec (POP rt       ) = pop rt >> decSP >> incPC >> cont
decodeExec (JMP oprand   ) = jmp oprand >> cont
decodeExec (JNZ oprand   ) = jnz oprand >> cont

cont :: SRISC s (CPU s)
cont = SRISC $ \cpu -> return cpu

stopCPU :: SRISC s (CPU s)
stopCPU = SRISC $ \cpu -> return cpu

fetch :: SRISC s Instruction
fetch = readReg PC >>= readRegVal >>= fetchAt

fetchAt :: Value -> SRISC s Instruction
fetchAt v = SRISC $ \cpu -> return $ rom cpu A.! fromIntegral v

readFlag :: SRISC s (CPU s -> STRef s Flag)
readFlag = pure z

readFlagRef :: SRISC s (STRef s Flag)
readFlagRef = SRISC $ \cpu -> return $ z cpu

readFlagVal :: (CPU s -> STRef s Flag) -> SRISC s Flag
readFlagVal pf = SRISC $ \cpu -> readSTRef $ pf cpu

writeFlag :: Flag -> STRef s Flag -> SRISC s ()
writeFlag f pf = SRISC $ \_ -> writeSTRef pf f

readStack :: SRISC s (CPU s -> STVector s Value)
readStack = pure stack

readStackVal :: (CPU s -> STVector s Value) -> SRISC s (STVector s Value)
readStackVal f = SRISC $ \cpu -> return $ f cpu

readReg :: RegType -> SRISC s (Reg s)
readReg PC = pure pc
readReg SP = pure sp
readReg RA = pure ra
readReg RB = pure rb
readReg RC = pure rc
readReg RD = pure rd

readRegVal :: Reg s -> SRISC s Value
readRegVal r = SRISC $ \cpu -> readSTRef $ r cpu

writeReg :: Reg s -> Value -> SRISC s ()
writeReg r v = SRISC $ \cpu -> writeSTRef (r cpu) v

setFlags :: RegType -> SRISC s ()
setFlags rt =
  readReg rt >>= readRegVal >>= (\v -> readFlagRef >>= writeFlag (v == 0))

jmp :: Oprand -> SRISC s ()
jmp oprand = readReg PC >>= (`mov` oprand)

jnz :: Oprand -> SRISC s ()
jnz oprand =
  readFlag >>= readFlagVal >>= (\f -> if f then incPC else jmp oprand)

add :: Reg s -> Oprand -> SRISC s ()
add = combine (+)

sub :: Reg s -> Oprand -> SRISC s ()
sub = combine (flip (-))

mov :: Reg s -> Oprand -> SRISC s ()
mov = combine const

incPC :: SRISC s ()
incPC = readReg PC >>= (\r -> add r (OV 1))

incSP :: SRISC s ()
incSP = readReg SP >>= (\r -> add r (OV 1))

decSP :: SRISC s ()
decSP = readReg SP >>= (\r -> sub r (OV 1))

combine :: (Value -> Value -> Value) -> Reg s -> Oprand -> SRISC s ()
combine f r (OV v ) = readRegVal r >>= writeReg r . f v
combine f r (OR rt) = readReg rt >>= readRegVal >>= combine f r . OV

psh :: Oprand -> SRISC s ()
psh (OV v) =
  readStack
    >>= readStackVal
    >>= (\s -> readReg SP >>= readRegVal >>= (\rv -> writeStack s rv v))
psh (OR r) = readReg r >>= readRegVal >>= psh . OV

pop :: RegType -> SRISC s ()
pop rt =
  readReg rt
    >>= (\r -> readStack >>= readStackVal >>= readStackTop >>= writeReg r)

readStackTop :: STVector s Value -> SRISC s Value
readStackTop vec = SRISC $ \cpu ->
  (fromIntegral <$> readSTRef (sp cpu)) >>= (\p -> M.read vec (p - 1))

writeStack :: STVector s Value -> Value -> Value -> SRISC s ()
writeStack vec p v = SRISC $ \_ -> write vec (fromIntegral p) v

freezeST :: CPU s -> ST s FrozenCPU
freezeST cpu = do
  pcv <- readSTRef $ pc cpu
  spv <- readSTRef $ sp cpu
  rav <- readSTRef $ ra cpu
  rbv <- readSTRef $ rb cpu
  rcv <- readSTRef $ rc cpu
  rdv <- readSTRef $ rd cpu
  zv  <- readSTRef $ z cpu
  sv  <- V.freeze $ stack cpu
  return FrozenCPU { _pc    = pcv
                   , _sp    = spv
                   , _ra    = rav
                   , _rb    = rbv
                   , _rc    = rcv
                   , _rd    = rdv
                   , _z     = zv
                   , _rom   = rom cpu
                   , _stack = sv
                   }
