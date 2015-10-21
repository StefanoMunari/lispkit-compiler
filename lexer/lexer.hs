module Lexer (
Token(..),
Symbol_T(..),
Operator_T(..),
Keyword_T(..),
lexi
) where

import Prelude hiding (EQ)
{-
import LexerUnitTest
import LexerIntegrationTest
-}
-- Tipi

data Keyword_T = LET | IN | END | LETREC | AND | IF | THEN | ELSE | LAMBDA
    deriving (Show,Eq)

data Operator_T = EQ | LEQ | CAR | CDR | CONS | ATOM
    deriving (Show,Eq)

data Symbol_T = LPAREN | RPAREN | EQUALS | PLUS | MINUS | TIMES | DIVISION |VIRGOLA| DOLLAR
    deriving (Show,Eq)

data Token = Keyword Keyword_T | Operator Operator_T | Id String |
    Symbol Symbol_T | Number Integer | String String | Bool Bool | Nil
    deriving (Show,Eq)



-- Funzioni di supporto

-- Testa se il carattere è un carattere valido per iniziare un identificatore, un operatore o una keyword
isAlphaChar c = c `elem` (['a' .. 'z'] ++ ['A' .. 'Z'])

-- Riconosce se c è un carattere numerico o no
isDigitChar c = c `elem` ['0' .. '9']

-- Testa se il carattere è un carattere valido per comporre un identificatore, un operatore o una keyword (ad eccezione del primo carattere)
isIdChar c = isAlphaChar c || isDigitChar c

-- Testa se il carattere è un separatore
isSeparator c = c `elem` "()=$,"

-- Testa se è uno spazio o accapo
isSpace c = c `elem` [' ', '\n', '\f', '\r', '\t']

isSymbol c = c `elem` "()=+-*/,"


{- data una stringa X la confronta con le parole chiavi e con gli operatori
   del Lisp Kit e se è una di queste cose, restituisce la corrispondente
   coppia token_lexema, altrimenti la considera un identificatore e
   restituisce la coppia (ID, STRINGA(X)) -}
extractWord :: String -> Token
extractWord w = case w of
    "let"     -> Keyword LET
    "in"      -> Keyword IN
    "end"     -> Keyword END
    "letrec"  -> Keyword LETREC
    "and"     -> Keyword AND
    "if"      -> Keyword IF
    "then"    -> Keyword THEN
    "else"    -> Keyword ELSE
    "lambda"  -> Keyword LAMBDA

    "eq"      -> Operator EQ
    "leq"     -> Operator LEQ
    "car"     -> Operator CAR
    "cdr"     -> Operator CDR
    "cons"    -> Operator CONS
    "atom"    -> Operator ATOM

    "true"    -> Bool True
    "false"   -> Bool False

    "nil"     -> Nil

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



{- Funzioni che implementano direttamente gli stati dell'automa. Osserva che
   non c'è ricorsione. Il passaggio dallo stato iniziale e principale I ad un
   altro stato è realizzato con un'invocazione. Poi si ritorna sempre a I e
   quindi basta il normale ritorno della funzione. -}

-- Stato N per riconoscere i numeri
{- n input numero segno
   input è la stringa in input
   numero è il numero elaborato finora
   segno è il segno del numero, true sse è negativo (rilevato da I) -}
n :: String -> Integer -> Bool -> (Token, String)
n "" _ _ = error "Unexpected end of string"
n input@(c:l) num sign
    | isDigitChar c =
        let d = read [c] :: Integer
        in n l (num*10 + d) sign
    | otherwise = (Number((if sign then -1 else 1) * num), input)

-- Stato SC per riconoscere le stringhe tra virgolette
{- sc input stringa
   stringa è la stringa elaborata finora -}
sc :: String -> String -> (Token, String)
sc "" _ = error "Unexpected end of string"
sc ('"':l) res = (String res, l) -- se è vuota ritorna la stringa elaborata finora
sc (c:l) res = sc l (res ++ [c]) -- se non è vuota appende il prossimo carattere alla stringa elaborata finora
                                 -- invocando ricorsivamente sc sull'input rimanente

-- Stato S per raccogliere le stringhe che possono corrispondere ad identificatori, operatori prefissi o keyword
{- s input stringa
   stringa è l'identificatore elaborato finora -}
s :: String -> String -> (Token, String)
s "" _ = error "Unexpected end of string"
s input@(c:l) res
    | isIdChar c = s l (res ++ [c]) -- se è un Id appende il carattere in fondo al risultato calcolato finora e invoca 's' sul resto della stringa da analizzare
    | otherwise = (extractWord(res), input) -- ritorna la tupla (Token, Stringa)


-- Stato principale I
{- i input
   input è il programma in forma di stringa
   Descrizione:
    stringa vuota => errore
    carattere $ => ritorno il risultato in forma di lista di token
    stringa non vuota =>
                        qualsiasi carattere di spazio => viene ignoranto invocando i sulla stringa restante
                        numero => stato n
                        stringa costante => stato sc
                        simboli => costruttore Symbol
                        identificatori,operatori,keyword => stato s
-}
i :: String -> [Token]
i "" = error "Unexpected end of string"
i "$" = [(Symbol DOLLAR)]
i input@(f:l)
  | isSpace f = i l
  | f == '~' =
     if isDigitChar (head(l))
       then
           let (tok, str) = n (tail(l))  (read [head(l)]::Integer) True
            in tok:(i str)
        else error "Unexpected token"
  | isDigitChar f =
                  let (tok, str) = n l (read [f]::Integer) False
                  in tok:(i str)
  | f == '\"' =
              let (tok, str) = sc l ""
              in tok:(i str)
  | isSymbol f =
                (Symbol (toSymbol f)):(i l)
  | isIdChar f =
                let (tok, str) = s input ""
                in tok:(i str)

-- Funzione principale per l'analisi lessicale
lexi :: String -> [Token]
lexi = i
