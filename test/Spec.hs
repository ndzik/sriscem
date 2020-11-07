import           Sriscem
import           SriscemSM
import           CPU
import           Data.Array                    as A
import           Test.Hspec

main :: IO ()
main =
  mapM_ testCPU [(Sriscem.runProg, "Sriscem"), (SriscemSM.runProg, "SriscemSM")]

testCPU :: (Program -> IO Value, String) -> IO ()
testCPU (cpuType, name) = hspec $ describe name $ do
  it "moves oprands into registers" $ do
    got <- cpuType testMov
    got `shouldBe` 420
  it "adds registers and oprands" $ do
    got <- cpuType testAddRegDiff
    got `shouldBe` 7
    got <- cpuType testAddRegSame
    got `shouldBe` 6
    got <- cpuType testAddVal
    got `shouldBe` 13
  it "subs registers and oprands" $ do
    got <- cpuType testSubRegDiff
    got `shouldBe` 1
    got <- cpuType testSubRegSame
    got `shouldBe` 0
    got <- cpuType testSubVal
    got `shouldBe` 5
  it "pushes and pops oprands from/to stack" $ do
    got <- cpuType testStack
    got `shouldBe` 69
  it "allows to jump" $ do
    got <- cpuType testJump
    got `shouldBe` 2
    got <- cpuType testJumpNZ
    got `shouldBe` 2
    got <- cpuType testNoJumpNZ
    got `shouldBe` 1

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
