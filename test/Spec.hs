{-# Language OverloadedStrings #-}
import Test.Framework (defaultMain, testGroup, buildTest)
import Test.Framework.Providers.HUnit

import Test.HUnit

import Data.List

import Lib
import Text.Peggy
import System.IO


main = defaultMain $ hUnitTestToTests $ TestList [
  "Parse example-instruction-list.txt" ~: testExample
 ]


testExampleExemplar = [OpPage [Encoding (Just "shtort form") [Instruction "JE" [15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0] [PatternLiteral [15,14,13] [True,False,True],PatternLiteral [12] [True],PatternLiteral [11,10,9,8] [True,False,False,False],PatternVariable [7,6,5,4,3,2,1,0] "off8"],Instruction "JE" [15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0] [PatternLiteral [15,14,13] [True,False,True],PatternLiteral [12] [True],PatternLiteral [11,10,9,8] [True,False,False,False],PatternVariable [7,6,5,4,3,2,1,0] "off8"]],Encoding (Just "short form") [Instruction "JE" [15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0] [PatternLiteral [15,14,13] [True,False,True],PatternLiteral [12] [True],PatternLiteral [11,10,9,8] [True,False,False,False],PatternVariable [7,6,5,4,3,2,1,0] "off8"]],Encoding (Just "shtort form") [Instruction "JE" [15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0] [PatternLiteral [15,14,13] [True,False,True],PatternLiteral [12] [True],PatternLiteral [11,10,9,8] [True,False,False,False],PatternVariable [7,6,5,4,3,2,1,0] "off8"],Instruction "JE" [15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0] [PatternLiteral [15,14,13] [True,False,True],PatternLiteral [12] [True],PatternLiteral [11,10,9,8] [True,False,False,False],PatternVariable [7,6,5,4,3,2,1,0] "off8"]],Encoding (Just "long form") [Instruction "EXTI" [15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0] [PatternLiteral [15,14,13] [True,True,False],PatternVariable [12,11,10,9,8,7,6,5,4,3,2,1,0] "ext13"],Instruction "JE" [15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0] [PatternLiteral [15,14,13] [True,False,True],PatternLiteral [12] [True],PatternLiteral [11,10,9,8] [True,False,False,False],PatternVariable [7,6,5,4,3,2,1,0] "off8"]]] "To perform a right arithmetical shift of a 16-bit integer in a register by a constant bit amount." "The amount may be between 0 and 15, inclusive." "opA \8592 mem[W|Ra]\nopB \8592 mem[W|Rb]\nres \8592 opA[15]{imm3+1}|opA[16:15-imm3]\nmem[W|Rd] \8592 res\nZ \8592 res = 0\nS \8592 res[15]\nC \8592 UNDEFINED\nV \8592 UNDEFINED\n"]

testExample :: Test
testExample = TestCase (do
    foo <- readFile "test/example-instruction-list.txt"
    case (parseString instructionlist "..." foo) of
      Right result    -> assertEqual "Instruction List parses correctly" result testExampleExemplar
      Left parseError -> assertBool  "Instruction List parses correctly" False
    )
