{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable, LambdaCase #-}

module Lib
    ( someFunc
    ) where

import Text.Peggy
import Data.Text (Text, pack, unlines)
import Data.Maybe (fromMaybe)
import Data.Data
import Data.Typeable


someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Instructions

type Bits = Int
type Name = Text
type Mnemonic = Text

data Part' = PatternLiteral' Int [Bool] | PatternVariable' Int Name
data Part  = PatternLiteral Bits [Bool] | PatternVariable Bits Name

data Instruction = Instruction Mnemonic [Part]

data Encoding = Encoding (Maybe Name) [Instruction]

-- Rep functions

data Term = TermBits Bits | TermMemoryLocation

data Signature = Signature [Term] Term

data Rep = Rep Name Signature

instructifier :: [[Int]] -> Text -> [Part'] -> Instruction
instructifier lengths name segments = if (and $ map (== head lengths) (lengths))
                                      then let
                                        cumulativesegs = scanl (+) 0 (head lengths)
                                        cumulativestarts = scanl (+) 0 (map (\case
                                          PatternLiteral'  x _ -> x
                                          PatternVariable' x _ -> x) segments)
                                        parts = undefined in Instruction name parts
                                      else undefined -- Blow up here

[peggy|

nl :: () = [\r]? [\n] { () }

ws :: () = " " { () }

bracketed :: Text
  = "(" [^)\n\r]+ ")" { pack $1 }

instrheader :: [Int] = ws+ ("|" [^|\n\r]+)+ "|" { map length $2 }

bit :: Bool = "1" { True } / "0" { False }

instrpart :: Part' = (ws* bit+ ws* { PatternLiteral' ((length $1) + (length $2) + (length $3)) ($2) }) / (ws* [a-zA-Z] [a-zA-Z0-9]* ws* { PatternVariable' ((length $1) + (1) + (length $3) + (length $4)) (pack ($2:$3)) })

instr :: Instruction = (instrheader nl { $1 })+ ws+ [^ \n\r]+ ws ("|" instrpart { $1 })+ "|" nl { instructifier $1 (pack $3) $5 }

bracketedEncoding :: Encoding
  = "Encoding" ws+ bracketed ":" nl instr+  { Encoding (Just $2) $4 }

unbracketedEncoding :: Encoding
  = "Encoding:" ws* nl instr+ { Encoding Nothing $3 }

encoding :: [Encoding]
  = (bracketedEncoding+ / (unbracketedEncoding {[$1]}))

|]
