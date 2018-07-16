
-- Scanner -- Decaf scanner                                     -*- haskell -*-
-- Copyright (C) 2013  Benjamin Barenblat <bbaren@mit.edu>
--
-- This file is a part of decafc.
--
-- decafc is free software: you can redistribute it and/or modify it under the
-- terms of the MIT (X11) License as described in the LICENSE file.
--
-- decafc is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the X11 license for more details.
{
{-# OPTIONS_GHC -w #-}
module Scanner ( ScannedToken(..)
               , Token(..)
               , scan
               , formatTokenOrError
               ) where


import Data.Word (Word8)
import Text.Printf (printf)
import Data.Char (ord)

import qualified Data.Bits
}

----------------------------------- Tokens ------------------------------------


$digit = [0-9]
$white2 = $white # \f -- we want the scanner to error on '\f' (form feed) characters
$escapes = [b t n \' \\ \"]
$alpha = [a-zA-Z_]             

tokens :-
  -- space
  $white2+ ;
  -- comment1
  "//".*   ;
  -- comment2(errors)
  "/*"(.|\n)*"*/";

  -- Symbols
  \+= { \posn _ -> scannedToken posn $ Sym "+="}
  \-= { \posn _ -> scannedToken posn $ Sym "-="}
  \++ { \posn _ -> scannedToken posn $ Sym "++"}
  \-- { \posn _ -> scannedToken posn $ Sym "--"}  
  \+ { \posn _ -> scannedToken posn $ Sym "+"}
  \- { \posn _ -> scannedToken posn $ Sym "-"}
  \* { \posn _ -> scannedToken posn $ Sym "*"}
  \/ { \posn _ -> scannedToken posn $ Sym "/"}
  \% { \posn _ -> scannedToken posn $ Sym "%"}
  == { \posn _ -> scannedToken posn $ Sym "=="}
  != { \posn _ -> scannedToken posn $ Sym "!="}
  \> { \posn _ -> scannedToken posn $ Sym ">"}
  \< { \posn _ -> scannedToken posn $ Sym "<"}
  = { \posn _ -> scannedToken posn $ Sym "="}
  && { \posn _ -> scannedToken posn $ Sym "&&"}
  \|\| { \posn _ -> scannedToken posn $ Sym "||"}
  \? {\posn _ -> scannedToken posn $ Sym "?"}
  \! {\posn _ -> scannedToken posn $ Sym "!"}
  \; {\posn _ -> scannedToken posn $ Sym ";"}
  \, {\posn _ -> scannedToken posn $ Sym ","}
  
  
  -- TODO(Handora): *= and so on

  -- string literal
  \" ((\\ $escapes)|($printable # [\\ \"]))*  \"  { \posn s -> scannedToken posn $ StringLiteral $ escapeString $ init $ tail s }

  "'" ((\\ $escapes) | ($printable # [\\ \'])) "'" { \posn s -> scannedToken posn $ CharLiteral $ (escapeString $ init $ tail s) !! 0 }                               

  -- integer
  (\+|\-)? $digit+  { \posn s -> scannedToken posn $ IntLiteral $ str2Int s }

  -- hex integer
  0x ($digit | [a-fA-F])+  { \posn s -> scannedToken posn $ IntLiteral (read s) }
  

-- Keyword Token
  class    { \posn _ -> scannedToken posn $ Class }
  bool     { \posn _ -> scannedToken posn $ DataType "bool" }
  break     { \posn _ -> scannedToken posn $ Break }
  import     { \posn s -> scannedToken posn $ Import }
  continue     { \posn s -> scannedToken posn $ Continue }
  if     { \posn s -> scannedToken posn $ If }
  else     { \posn s -> scannedToken posn $ Else }
  true     { \posn s -> scannedToken posn $ BoolLiteral True }
  false     { \posn s -> scannedToken posn $ BoolLiteral False }
  for     { \posn s -> scannedToken posn $ For }
  while     { \posn s -> scannedToken posn $ While }
  int     { \posn s -> scannedToken posn $ DataType "int" }
  return     { \posn s -> scannedToken posn $ Return }
  len     { \posn s -> scannedToken posn $ Len }
  void     { \posn s -> scannedToken posn $ Void }

  -- identifier
  $alpha($alpha|$digit)*  { \posn s -> scannedToken posn $ Identifier s }


  \{       { \posn _ -> scannedToken posn LCurly }
  \}       { \posn _ -> scannedToken posn RCurly }
  \[       { \posn _ -> scannedToken posn LBracket }
  \]       { \posn _ -> scannedToken posn RBracket }
  \(       { \posn _ -> scannedToken posn LParenthes }
  \)       { \posn _ -> scannedToken posn RParenthes }

----------------------------- Representing tokens -----------------------------

{

-- | A token.
data Token = BoolLiteral Bool
           | StringLiteral String
           | IntLiteral Int
           | CharLiteral Char
           | Identifier String
           | Sym String
           | LCurly
           | RCurly
           | DataType String
           | Break
           | Import
           | Continue
           | Else
           | For
           | While
           | If
           | Return
           | Len
           | Void
           | Class
           | LParenthes
           | RParenthes
           | LBracket
           | RBracket
           deriving (Eq)

instance Show Token where
  show (Identifier s) = "IDENTIFIER " ++ s
  show LCurly = "{"
  show RCurly = "}"
  show (BoolLiteral b) = "BOOLLITERAL " ++ show b
  show (DataType s) = "DATATYPE " ++ show s
  show Break = "break"
  show Import = "import"
  show Continue = "continue"
  show Else = "else"
  show For = "for"
  show While = "while"
  show If = "if"
  show Return = "return"
  show Len = "len"
  show Void = "void"
  show Class = "class"
  show (StringLiteral s) = "STRINGLITERAL \"" ++ escapeString s ++ "\""
  show (CharLiteral c) = "CHARLITERAL '" ++ unescapeString [c] ++ "'"
  show (IntLiteral i) = "INTLITERAL " ++ show i
  show (Sym s) = s
  show LBracket = "["
  show RBracket = "]"
  show LParenthes = "("
  show RParenthes = ")"


-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type


type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,bs,s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (p,c,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c 
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))


-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

formatChar :: Char -> String
formatChar '\n' = "0xA"
formatChar '\f' = "0xC"
formatChar '\t' = "0x9"
formatChar c = "'" ++ [c] ++ "'"

data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

alexScanTokens :: String -> [Either String ScannedToken]
alexScanTokens str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError ((AlexPn pos line column), prevChar, currentBytes, inpString) ->
                  let badChar = head inpString in
                  let errorMessage = printf "line %d:%d: unexpected char: %s" line column $ formatChar badChar
                      inp' = ( alexMove (AlexPn pos line column) badChar
                             , prevChar
                             , currentBytes
                             , drop 1 inpString
                             ) in
                  Left errorMessage : go inp'
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> Right (act pos (take len str)) : go inp'

  
-- | A token with position information.
data ScannedToken = ScannedToken { line :: Int
                                 , column :: Int
                                 , extractRawToken :: Token
                                 } deriving (Eq)

{-| Smart constructor to create a 'ScannedToken' by extracting the line and
column numbers from an 'AlexPosn'. -}
scannedToken :: AlexPosn -> Token -> ScannedToken
scannedToken (AlexPn _ lineNo columnNo) tok = ScannedToken lineNo columnNo tok


---------------------------- Scanning entry point -----------------------------

-- fill out this function with extra cases if you use error tokens
-- and want them to be treated as errors instead of valid tokens
catchErrors :: Either String ScannedToken -> Either String ScannedToken
catchErrors e = e -- default case

scan :: String -> [Either String ScannedToken]
scan = map catchErrors . alexScanTokens

formatTokenOrError :: Either String ScannedToken -> Either String String
formatTokenOrError (Left err) = Left err
formatTokenOrError (Right tok) = Right $ unwords [ show $ line tok
                                                 , show $ extractRawToken tok
                                                 ]

escapeString :: String -> String
escapeString [] = []
escapeString ('\\':x:xs) = (convert x) : escapeString xs
escapeString (x:xs) = x : escapeString xs

convert :: Char -> Char
convert 't' = '\t'
convert 'n' = '\n'
convert 'b' = '\b'
convert x = x

unescapeString :: String -> String
unescapeString [] = []
unescapeString ('\n':xs) = '\\' : 'n' : unescapeString xs
unescapeString ('\t':xs) = '\\' : 't' : unescapeString xs
unescapeString ('\b':xs) = '\\' : 'b' : unescapeString xs
unescapeString (x:xs) = x : unescapeString xs

str2Int :: String -> Int
str2Int '+':s = read s
str2Int s = read s
}
