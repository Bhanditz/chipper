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

data Encoding = Encoding [Instruction]

-- Rep functions

data Term = TermBits Bits | TermMemoryLocation

data Signature = Signature [Term] Term

data Rep = Rep Name Signature

[peggy|

nl :: () = [\r]? [\n] { () }

ws :: () = " " { () }

bracketed :: Text
  = "(" [^)\n\r]+ ")" { pack $1 }

bracketedEncoding :: Encoding
  = "Encoding" ws+ bracketed ":" nl { Encoding [] }

unbracketedEncoding :: Encoding
  = "Encoding:" ws* nl { Encoding [] }

encoding :: [Encoding]
  = (bracketedEncoding+ / (unbracketedEncoding {[$1]}))

|]
