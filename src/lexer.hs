module Lexer (
  Token(..),
  Symbol_T(..),
  Operator_T(..),
  Keyword_T(..),
  lexi
) where

import Prelude hiding (EQ)
-- import LexerTest


-- Types

data Keyword_T
  = LET
  | IN
  | END
  | LETREC
  | AND
  | IF
  | THEN
  | ELSE
  | LAMBDA
  deriving (Show,Eq)


data Operator_T
  = EQ
  | LEQ
  | CAR
  | CDR
  | CONS
  | ATOM
  deriving (Show,Eq)


data Symbol_T
  = LPAREN
  | RPAREN
  | EQUALS
  | PLUS
  | MINUS
  | TIMES
  | DIVISION
  | VIRGOLA
  | DOLLAR
  deriving (Show,Eq)


data Token
  = Keyword Keyword_T
  | Operator Operator_T
  | Id String
  | Symbol Symbol_T
  | Number Integer
  | String String
  | Bool Bool
  | Nil
  deriving (Show,Eq)



-- Utility functions

isAlphaChar c = c `elem` (['a' .. 'z'] ++ ['A' .. 'Z'])


isDigitChar c = c `elem` ['0' .. '9']


isIdChar c = isAlphaChar c || isDigitChar c


isSeparator c = c `elem` "()=$,"


isSpace c = c `elem` [' ', '\n', '\f', '\r', '\t']


isSymbol c = c `elem` "()=+-*/%,"


extractWord :: String -> Token
extractWord w = case w of
    -- simple block
    "let"     -> Keyword LET
    "in"      -> Keyword IN
    "end"     -> Keyword END
    -- recursive block
    "letrec"  -> Keyword LETREC
    -- separator for multiple definitions
    "and"     -> Keyword AND
    -- lambda expression
    "lambda"  -> Keyword LAMBDA
    -- conditional operator
    "if"      -> Keyword IF
    "then"    -> Keyword THEN
    "else"    -> Keyword ELSE
    -- relational operators
    "eq"      -> Operator EQ
    "leq"     -> Operator LEQ
    -- structural operators
    "car"     -> Operator CAR
    "cdr"     -> Operator CDR
    "cons"    -> Operator CONS
    "atom"    -> Operator ATOM
    -- constants
    "true"    -> Bool True
    "false"   -> Bool False
    "nil"     -> Nil
    -- variables
    otherwise -> Id w


toSymbol :: Char -> Symbol_T
toSymbol c = case c of
    '(' -> LPAREN
    ')' -> RPAREN
    '+' -> PLUS
    '-' -> MINUS
    '*' -> TIMES
    '/' -> DIVISION
    '=' -> EQUALS
    ',' -> VIRGOLA

-- Functions that implement directly the automata states

n:: String -> Bool -> (Token, String)
n "" _ = error "Unexpected end of string"
n (h:t) sign
        | isDigitChar h =
            let
                buildNum input@(a:b) num =
                    if isDigitChar a
                        then buildNum b (num*10 + read [a]::Integer)
                        else (Number( (if sign then -1 else 1) * num), input)
                buildNum _ _ = error "Unexpected end of string"
            in
                buildNum t (read[h]::Integer)
        | otherwise     = error "Malformed input"


sc :: String -> String -> (Token, String)
sc "" _ = error "Unexpected end of string"
sc ('"':l) res = (String res, l)
sc (c:l)   res = sc l (res ++ [c])


s :: String -> String -> (Token, String)
s "" _ = error "Unexpected end of string"
s input@(c:l) res
    | isIdChar c = s l (res ++ [c])
    | otherwise = (extractWord(res), input)

i :: String -> [Token]
i "" = error "Unexpected end of string"
i "$" = [(Symbol DOLLAR)]
i input@(f:l)
  | isSpace  f = i l
  | isSymbol f =
                (Symbol (toSymbol f)):(i l)
  | otherwise =
      let
        match '\"' = sc l ""
        match '~'  = n l True
        match x
            | isDigitChar x = n input False
            | isAlphaChar x = s input ""
        (token, string) = match f
      in
        token:(i string)

-- Main function : lexical analyzer

lexi :: String -> [Token]
lexi = i
