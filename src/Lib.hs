{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable, LambdaCase #-}

module Lib
    ( someFunc, encoding, Encoding(..), oppage, OpPage(..)
    ) where

import Text.Peggy
import Data.Text (Text, pack, unlines)
import Data.Maybe (fromMaybe)
import Data.Data
import Data.Typeable
import Numeric (readHex)
import System.IO
import Data.Monoid (mconcat)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Instructions

type BitOrder = [Int]

type Bits = [Int] -- list of which bits make up a part - length bits gets you the number of bits
type Name = Text
type Mnemonic = Text

data Part' = PatternLiteral' Int [Bool] | PatternVariable' Int Name deriving (Show, Data, Eq, Typeable)
data Part  = PatternLiteral Bits [Bool] | PatternVariable Bits Name deriving (Show, Data, Eq, Typeable)

data Instruction = Instruction Mnemonic BitOrder [Part] deriving (Show, Data, Eq, Typeable)

data Encoding = Encoding (Maybe Name) [Instruction] deriving (Show, Data, Eq, Typeable)

type Purpose = Text
type Restrictions = Text
type Operation = Text

data OpPage = OpPage [Encoding] Purpose Restrictions Operation deriving (Show, Data, Eq, Typeable)

-- Rep functions

data Term = TermBits Bits | TermMemoryLocation

data Signature = Signature [Term] Term

data Rep = Rep Name Signature

instructifier :: [[Int]] -> BitOrder -> Text -> [Part'] -> Instruction
instructifier lengths bitorder name segments = if (and $ map (== head lengths) (lengths))
                                      then let
                                        cumulativeheads = tail (scanl (+) 0 (head lengths))
                                        headedges       = zip [0..] (zip (0:(map (\n -> n - 1) cumulativeheads)) cumulativeheads)
                                        cumulativepats = tail $ scanl (+) 0 (map (\case
                                          PatternLiteral'  x _ -> x
                                          PatternVariable' x _ -> x) segments)
                                        patedges = zip (0:(map (\n -> n - 1) cumulativepats)) cumulativepats
                                        partsbits = map (\(startpat, endpat) -> map (\y -> bitorder !! (fst y)) (filter (\(_, (startbit, endbit)) -> (startpat <= startbit) && (endpat >= endbit)) headedges)) patedges
                                        parts = map (\case
                                          (b, PatternLiteral'  _ x) -> PatternLiteral  b x
                                          (b, PatternVariable' _ x) -> PatternVariable b x) (zip partsbits segments) in Instruction name bitorder parts
                                      else undefined -- Blow up here

[peggy|

nl :: () = [\r]? [\n] { () }

ws :: () = [ ] { () }

bracketed :: Text
  = '(' [^)\n\r]+ ')' { pack $1 }

instrheader :: [Int] = ws+ ('|' [-]+)+ '|' { map (\x -> 1 + (length x)) $2 }

bit :: Bool = '1' { True } / '0' { False }

instrpart :: Part' = (ws* bit+ ws* { PatternLiteral' ((length $1) + (length $2) + (length $3) + 1) ($2) }) / (ws* [a-zA-Z] [a-zA-Z0-9]* ws* { PatternVariable' ((length $1) + (1) + (length $3) + (length $4) + 1) (pack ($2:$3)) })

bitsheader :: BitOrder = ws+ ('|' ws* [0-9a-fA-F]+ ws* { fst (head (readHex $2)) } )+ '|' { $2 }

instr :: [Instruction] = instrheader nl bitsheader nl instrheader nl (ws+ [^ \n\r]+ ws ('|' instrpart { $1 })+ '|' nl instrheader nl { ($2, $4) })+ { map (\x -> instructifier ($1:[$5]) $3 (pack (fst x)) (snd x)) $7 }

bracketedEncoding :: Encoding
  = 'Encoding' ws+ bracketed ':' nl instr nl { Encoding (Just $2) $4 }

unbracketedEncoding :: Encoding
  = 'Encoding:' ws* nl instr nl { Encoding Nothing $3 }

purpose :: Purpose = 'Purpose:' nl (ws ws [^\n\r]+ nl { pack $3 })+ nl { mconcat $2 }

restrictions :: Restrictions = 'Restrictions:' nl (ws ws [^\n\r]+ nl { pack $3 })+ nl { mconcat $2 }

operation :: Operation = 'Operation:' nl (ws ws [^\n\r]+ nl { pack $3 })+ nl { mconcat $2 }

encoding :: [Encoding]
  = (bracketedEncoding+ / (unbracketedEncoding {[$1]}))

oppage :: OpPage = encoding purpose restrictions operation nl { OpPage $1 $2 $3 $4 }

|]
