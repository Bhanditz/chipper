{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

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

data Part = PatternLiteral Bits [Bool] | PatternVariable Bits Name

data Instruction = Instruction Mnemonic [Part]

data Encoding = Encoding (Maybe Name) [Instruction]

-- Rep functions

data Term = TermBits Bits | TermMemoryLocation

data Signature = Signature [Term] Term

data Rep = Rep Name Signature

instructifier = undefined

[peggy|

nl :: () = [\r]? [\n] { () }

ws :: () = " " { () }

bracketed :: Text
  = "(" [^)\n\r]+ ")" { pack $1 }

instrheader :: [Int] = ws+ ("|" [^|\n\r]+)+ "|" { map length $2 }

instr :: Instruction = (instrheader nl)+ ws+ [^ \n\r]+ ws ("|" [^|\n\r]+)+ "|" nl { instructifier $1 $3 $5 }

bracketedEncoding :: Encoding
  = "Encoding" ws+ bracketed ":" nl instr+  { Encoding (Just $2) $4 }

unbracketedEncoding :: Encoding
  = "Encoding:" ws* nl instr+ { Encoding Nothing $3 }

encoding :: [Encoding]
  = (bracketedEncoding+ / (unbracketedEncoding {[$1]}))

|]
