module Syntax (
  progdoll
) where

import Prelude hiding (EQ, exp)
import Lexer
-- import SyntaxTest

-- Datatype to handle exceptions in a declarative manner

type Exception = String
data Exc a = Raise Exception | Return a


instance Show a => Show (Exc a) where
 show (Raise e)  = "ERROR:" ++ e
 show (Return x) = "REACHED:" ++ (show x)


instance Functor Exc where
  fmap fun (Return x) = Return (fun x)
  fmap fun (Raise e)  = Raise e


instance Applicative Exc where
  pure               = Return
  (Raise e)    <*> _ = Raise e
  (Return fun) <*> q = fmap fun q


instance Monad Exc where
 return                 = Return
 (Raise e)  >>= _       = Raise e
 (Return x) >>= funMon  = funMon x


raise :: Exception -> Exc a
raise e = Raise e


-- Parsing of terminal symbols

rec_key:: [Token] -> Exc [Token]
rec_key (Keyword LET : b)    = Return b
rec_key (Keyword LETREC : b) = Return b
rec_key (a : _)              = Raise ("Found " ++ show(a) ++" instead of LET or LETREC")
rec_key  x                   = Raise ("Unknown error"  ++  show(x))


rec_in:: [Token] -> Exc [Token]
rec_in (Keyword IN : b) = Return b
rec_in (a : _)          = Raise ("Found " ++ show(a) ++ " instead of IN")


rec_end:: [Token] -> Exc [Token]
rec_end (Keyword END : b) = Return b
rec_end (a : _)           = Raise ("Found " ++ show(a) ++ " instead of END")


rec_then:: [Token] -> Exc [Token]
rec_then (Keyword THEN : b) = Return b
rec_then (a : _)            = Raise ("Found " ++ show(a) ++ " instead of THEN")


rec_else:: [Token] -> Exc [Token]
rec_else (Keyword ELSE : b) = Return b
rec_else (a : _)            = Raise ("Found " ++ show(a) ++ " instead of ELSE")


rec_lp:: [Token] -> Exc [Token]
rec_lp (Symbol LPAREN : b) = Return b
rec_lp (a : _)             = Raise ("Found "++ show(a) ++ " instead of (")


rec_rp:: [Token] -> Exc [Token]
rec_rp (Symbol RPAREN : b) = Return b
rec_rp (a : _)             = Raise ("Found "++ show(a) ++ " instead of )")


rec_virg:: [Token] -> Exc [Token]
rec_virg (Symbol VIRGOLA : b) = Return  b
rec_virg (a : _)              = Raise ("Found "++ show(a) ++ " instead of ,")


rec_equals:: [Token] -> Exc [Token]
rec_equals (Symbol EQUALS : b) = Return b
rec_equals (a : _)             = Raise ("Found "++ show(a) ++ " instead of =")


-- Parsing of non-terminal symbols

-- Prog ::= let Bind in Exp end | letrec Bind in Exp end
prog:: [Token] -> Exc [Token]
prog a = do
         w <- rec_key a
         x <- bind w
         y <- rec_in x
         z <- exp y
         k <- rec_end z
         let
             aux (Keyword LET : _)     = Return k
             aux (Keyword LETREC : _)  = Return k
             aux start                 = Raise  ("Found "++ show(start)
                                                ++ " instead of let o letrec")
          in
             aux a


-- Bind ::= var = Exp X
bind:: [Token] -> Exc [Token]
bind (Id a : b) =  do
                     x <- rec_equals b
                     y <- exp x
                     funx y
bind (a : _)    =  Raise ("Binder with "++ show(a) ++" on the left")


-- X ::= and Bind | epsilon
funx:: [Token] -> Exc [Token]
funx (Keyword AND : b)     = bind b
funx a@(Keyword IN : _)    = Return a
funx (a : _)               = Raise ("Found "++ show(a) ++" after binders")


-- Exp ::= Prog | lambda(Seq_Var) Exp | ExpA | OPP(Seq_Exp) |
--         if Exp then Exp else Exp
--
-- OPP ::= cons | car | cdr | eq | leq | atom
exp:: [Token] -> Exc [Token]
exp a@(Keyword LET : b)    = prog a
exp a@(Keyword LETREC : b) = prog a
exp (Keyword LAMBDA : b)   = do
                                x <- rec_lp b
                                y <- seq_var x
                                exp y
exp (Operator CONS : b)    = do
                                w <- rec_lp b
                                x <- exp w
                                y <- rec_virg x
                                z <- exp y
                                rec_rp z
exp (Operator LEQ : b)     = do
                                w <- rec_lp b
                                x <- exp w
                                y <- rec_virg x
                                z <- exp y
                                rec_rp z
exp (Operator EQ : b)      = do
                                w <- rec_lp b
                                x <- exp w
                                y <- rec_virg x
                                z <- exp y
                                rec_rp z
exp (Operator CAR : b)      = do
                                w <- rec_lp b
                                x <- exp w
                                rec_rp x
exp (Operator CDR : b)      = do
                                w <- rec_lp b
                                x <- exp w
                                rec_rp x
exp (Operator ATOM : b)     = exp b
exp (Keyword IF : b)        = do
                                w <- exp b
                                x <- rec_then w
                                y <- exp x
                                z <- rec_else y
                                exp z
exp x                       = expa x


-- ExpA ::= T E1
expa:: [Token] -> Exc [Token]
expa a = do
           x <- funt a
           fune1 x


-- E1 ::= OPA T E1 | epsilon
--
-- OPA ::= + | -
fune1:: [Token] -> Exc [Token]
fune1 (Symbol PLUS : a)    = do
                               x <- funt a
                               fune1 x
fune1 (Symbol MINUS : a)   = do
                               x <- funt a
                               fune1 x
fune1 x                    = Return x


-- T ::= F T1
funt:: [Token] -> Exc [Token]
funt a = do
           x <- funf a
           funt1 x


-- T1 ::= OPM F T1 | epsilon
--
-- OPM ::= * | /
funt1:: [Token] -> Exc [Token]
funt1 (Symbol TIMES : a)     = do
                                 x <- funf a
                                 funt1 x
funt1 (Symbol DIVISION : a)  = do
                                 x <- funf a
                                 funt1 x
funt1 x                      = Return x


-- F ::= var Y | exp_const | (ExpA)
funf:: [Token] -> Exc [Token]
funf (Id a : b)              = funy b
funf (Symbol LPAREN : b)     = do
                                 y <- expa b
                                 rec_rp y
funf (Number a : b)          = Return b
funf (Nil : b)               = Return b
funf (Bool a : b)            = Return b
funf (String a : b)          = Return b
funf (a : _)                 = Raise  ("Error in funf, found "++ show(a))


-- Y ::= (Seq_Exp) | epsilon
funy:: [Token] -> Exc [Token]
funy (Symbol LPAREN : b) = do
                             x <- seq_exp b
                             rec_rp x
funy x                   = Return x


-- Seq_Exp ::= Exp Sep_Exp | epsilon
seq_exp:: [Token] -> Exc [Token]
seq_exp a@(Symbol RPAREN : _)  = Return a
seq_exp a                      = do
                                   x <- exp a
                                   sep_exp x


-- Seq_Var ::= var Seq_var | epsilon
seq_var:: [Token] -> Exc [Token]
seq_var (Id a : b)              = seq_var b
seq_var (Symbol RPAREN : b)     = Return b
seq_var (a : _)                 = Raise ("Error in seq_var, found "++ show(a))


-- Sep_Exp ::=  , Exp Sep_Exp | epsilon
sep_exp:: [Token] -> Exc [Token]
sep_exp (Symbol VIRGOLA : b)   = seq_exp b
sep_exp a@(Symbol RPAREN : b)  = Return a
sep_exp (a : _)                = Raise ("Error in sep_exp, found "++ show(a))

-- Main function : verifies the correctness of the token list

progdoll::[Token] -> String
progdoll x= show (prog x)
