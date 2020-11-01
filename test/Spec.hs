import           Sriscem
import           Data.Array                    as A
import           Test.Hspec

main :: IO ()
main = hspec $ describe "SRISCEM" $ do
  it "moves oprands into registers" $ do
    got <- run testMov
    got `shouldBe` 420
  it "adds registers and oprands" $ do
    got <- run testAddRegDiff
    got `shouldBe` 7
    got <- run testAddRegSame
    got `shouldBe` 6
    got <- run testAddVal
    got `shouldBe` 13
  it "subs registers and oprands" $ do
    got <- run testSubRegDiff
    got `shouldBe` 1
    got <- run testSubRegSame
    got `shouldBe` 0
    got <- run testSubVal
    got `shouldBe` 5
  it "pushes and pops oprands from/to stack" $ do
    got <- run testStack
    got `shouldBe` 69
  it "allows to jump" $ do
    got <- run testJump
    got `shouldBe` 2
    got <- run testJumpNZ
    got `shouldBe` 2
    got <- run testNoJumpNZ
    got `shouldBe` 1

run :: Program -> IO Int
run p =
  let cpu    = go $ mkCPU p
      (_, v) = ra cpu
  in  return v
 where
  go :: CPU -> CPU
  go cpu =
    let opcode = rom cpu A.! (snd . pc $ cpu)
    in  if opcode == FIN then cpu else go $ step opcode cpu

testMov :: Program
testMov = [MOV RA (Val 420), FIN]

testAddRegDiff :: Program
testAddRegDiff = [MOV RA (Val 3), MOV RB (Val 4), ADD RA (Reg RB), FIN]

testAddRegSame :: Program
testAddRegSame = [MOV RA (Val 3), ADD RA (Reg RA), FIN]

testAddVal :: Program
testAddVal = [MOV RA (Val 4), ADD RA (Val 9), FIN]

testSubRegDiff :: Program
testSubRegDiff = [MOV RA (Val 4), MOV RB (Val 3), SUB RA (Reg RB), FIN]

testSubRegSame :: Program
testSubRegSame = [MOV RA (Val 3), SUB RA (Reg RA), FIN]

testSubVal :: Program
testSubVal = [MOV RA (Val 9), SUB RA (Val 4), FIN]

testStack :: Program
testStack = [MOV RB (Val 69), PSH (Reg RB), POP RA, FIN]

testJump :: Program
testJump = [MOV RA (Val 1), JMP (Val 3), FIN, MOV RA (Val 2), FIN]

testJumpNZ :: Program
testJumpNZ = [ADD RA (Val 1), JNZ (Val 3), FIN, MOV RA (Val 2), FIN]

testNoJumpNZ :: Program
testNoJumpNZ = [MOV RA (Val 1), JNZ (Val 3), FIN, MOV RA (Val 2), FIN]
