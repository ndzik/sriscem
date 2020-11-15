import           SriscemMonad
import           MCPU
import           Test.Hspec

main :: IO ()
main = mapM_ testCPU [(SriscemMonad.runProg, "SriscemMonad")]

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
  it "runs 2^10" $ do
    got <- cpuType prog01
    got `shouldBe` 1024
  it "runs 2*10" $ do
    got <- cpuType prog02
    got `shouldBe` 20

testMov :: Program
testMov = [MOV RA (OV 420), FIN]

testAddRegDiff :: Program
testAddRegDiff = [MOV RA (OV 3), MOV RB (OV 4), ADD RA (OR RB), FIN]

testAddRegSame :: Program
testAddRegSame = [MOV RA (OV 3), ADD RA (OR RA), FIN]

testAddVal :: Program
testAddVal = [MOV RA (OV 4), ADD RA (OV 9), FIN]

testSubRegDiff :: Program
testSubRegDiff = [MOV RA (OV 4), MOV RB (OV 3), SUB RA (OR RB), FIN]

testSubRegSame :: Program
testSubRegSame = [MOV RA (OV 3), SUB RA (OR RA), FIN]

testSubVal :: Program
testSubVal = [MOV RA (OV 9), SUB RA (OV 4), FIN]

testStack :: Program
testStack = [MOV RB (OV 69), PSH (OR RB), POP RA, FIN]

testJump :: Program
testJump = [MOV RA (OV 1), JMP (OV 3), FIN, MOV RA (OV 2), FIN]

testJumpNZ :: Program
testJumpNZ = [ADD RA (OV 1), JNZ (OV 3), FIN, MOV RA (OV 2), FIN]

testNoJumpNZ :: Program
testNoJumpNZ = [MOV RA (OV 1), JNZ (OV 3), FIN, MOV RA (OV 2), FIN]

-- prog01 describes `2^10 = 1024`.
prog01 :: Program
prog01 =
  [MOV RA (OV 2), MOV RB (OV 9), ADD RA (OR RA), SUB RB (OV 1), JNZ (OV 2), FIN]

-- prog02 describes `2*10 = 20`.
prog02 :: Program
prog02 =
  [ MOV RC (OV 2)
  , MOV RB (OV 10)
  , ADD RA (OR RC)
  , SUB RB (OV 1)
  , JNZ (OV 2)
  , FIN
  ]
