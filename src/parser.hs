module Parser (
  LKC(..),
  parse
) where

import Prelude hiding (EQ, exp)
import Lexer

-- Datatype LKC (Concrete Lispkit)

data LKC
    = ETY     -- epsilon production
    | VAR     String
    | NUM     Integer
    | STRI    String
    | BOO     Bool
    | NIL
    | ADD     LKC LKC
    | SUB     LKC LKC
    | MULT    LKC LKC
    | DIV     LKC LKC
    | EQC     LKC LKC
    | LEQC    LKC LKC
    | CARC    LKC
    | CDRC    LKC
    | CONSC   LKC LKC
    | ATOMC   LKC
    | IFC     LKC LKC LKC
    | LAMBDAC [LKC] LKC
    | CALL    LKC [LKC]
    | LETC    LKC [(LKC,LKC)]
    | LETRECC LKC [(LKC, LKC)]
    deriving(Show, Eq)

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
rec_in (Keyword IN : b)= Return b
rec_in (a : _)         = Raise ("Found " ++ show(a) ++ " instead of IN")


rec_end:: [Token] -> Exc [Token]
rec_end (Keyword END : b)= Return b
rec_end (a : _)          = Raise ("Found " ++ show(a) ++ " instead of END")


rec_then:: [Token] -> Exc [Token]
rec_then (Keyword THEN : b)= Return b
rec_then (a : _)           = Raise ("Found " ++ show(a) ++ " instead of THEN")


rec_else:: [Token] -> Exc [Token]
rec_else (Keyword ELSE : b)= Return b
rec_else (a : _)           = Raise ("Found " ++ show(a) ++ " instead of ELSE")


rec_lp:: [Token] -> Exc [Token]
rec_lp (Symbol LPAREN : b)= Return b
rec_lp (a : _)            = Raise ("Found "++ show(a) ++ " instead of (")


rec_rp:: [Token] -> Exc [Token]
rec_rp (Symbol RPAREN : b)= Return b
rec_rp (a : _)            = Raise ("Found "++ show(a) ++ " instead of )")


rec_virg:: [Token] -> Exc [Token]
rec_virg (Symbol VIRGOLA : b)= Return  b
rec_virg (a : _)             = Raise ("Found "++ show(a) ++ " instead of ,")


rec_equals:: [Token] -> Exc [Token]
rec_equals (Symbol EQUALS : b)= Return b
rec_equals (a : _)            = Raise ("Found "++ show(a) ++ " instead of =")


rec_dollar:: Exc ([Token], LKC) -> LKC
rec_dollar (Return ([Symbol  DOLLAR], abstract_tree)) = abstract_tree
rec_dollar (Return (_, _))                            = error "$ not found after END"
rec_dollar (Raise  exception)                         = error $ show(exception)


-- Parsing of non-terminal symbols

-- Prog ::= let Bind in Exp end | letrec Bind in Exp end
prog:: [Token] -> Exc ([Token], LKC)
prog a = do
         w            <- rec_key a
         (x, binders) <- bind w
         y            <- rec_in x
         (z, body)    <- exp y
         k            <- rec_end z
         let
             aux (Keyword LET : _)          = Return (k, LETC body binders)
             aux (Keyword LETREC : _)       = Return (k, LETRECC body binders)
             aux start                      = Raise  ("Found "++ show(start)
                                                ++ " instead of let o letrec")
          in
             aux a


-- Bind ::= var = Exp X
bind:: [Token] -> Exc ([Token], [(LKC,LKC)])
bind (Id a : b)            =  do
                              x            <- rec_equals b
                              (y, expr)    <- exp x
                              (z, binders) <- funx y
                              Return (z, (VAR a, expr) : binders)
bind (a : _)               =  Raise ("Binder with "++ show(a) ++" on the left")


-- X ::= and Bind | epsilon
funx:: [Token] -> Exc ([Token], [(LKC,LKC)])
funx (Keyword AND : b)     = bind b
funx a@(Keyword IN : _)    = Return (a, [])
funx (a : _)               = Raise ("Found "++ show(a) ++" after binders")


-- Exp ::= Prog | lambda(Seq_Var) Exp | ExpA | OPP(Seq_Exp) |
--         if Exp then Exp else Exp
--
-- OPP ::= cons | car | cdr | eq | leq | atom
exp:: [Token] -> Exc ([Token], LKC)
exp a@(Keyword LET : b)    = prog a
exp a@(Keyword LETREC : b) = prog a
exp (Keyword LAMBDA : b)   = do
                                x                 <- rec_lp b
                                (y, form_params)  <- seq_var x
                                (z, body)         <- exp y
                                Return (z, LAMBDAC form_params body)
exp (Operator CONS : b)    = do
                                w           <- rec_lp b
                                (x, car)    <- exp w
                                y           <- rec_virg x
                                (z, cdr)    <- exp y
                                k           <- rec_rp z
                                Return (k, CONSC car cdr)
exp (Operator LEQ : b)     = do
                                w        <- rec_lp b
                                (x, op0) <- exp w
                                y        <- rec_virg x
                                (z, op1) <- exp y
                                k        <- rec_rp z
                                Return (k, LEQC op0 op1)
exp (Operator EQ : b)      = do
                                w        <- rec_lp b
                                (x, op0) <- exp w
                                y        <- rec_virg x
                                (z, op1) <- exp y
                                k        <- rec_rp z
                                Return (k, EQC op0 op1)
exp (Operator CAR : b)      = do
                                w         <- rec_lp b
                                (x, list) <- exp w
                                y         <- rec_rp x
                                Return (y, CARC list)
exp (Operator CDR : b)      = do
                                w         <- rec_lp b
                                (x, list) <- exp w
                                y         <- rec_rp x
                                Return (y, CDRC list)
exp (Operator ATOM : b)     = do
                                (w, constant) <- exp b
                                Return (w, ATOMC constant)
exp (Keyword IF : b)        = do
                                (w, condition)   <- exp b
                                x                <- rec_then w
                                (y, consequent)  <- exp x
                                z                <- rec_else y
                                (k, alternative) <- exp z
                                Return (k, IFC condition consequent alternative)
exp x                       = expa x


-- ExpA ::= T E1
expa:: [Token] -> Exc ([Token], LKC)
expa a = do
           (x, operand)    <- funt a
           (y, expression) <- fune1 x operand
           if expression == ETY
            then Return (y, operand)
            else Return (y, expression)


-- E1 ::= OPA T E1 | epsilon
--
-- OPA ::= + | -
fune1:: [Token] -> LKC -> Exc ([Token], LKC)
fune1 (Symbol PLUS : a) op0  = do
                               (x, op1)         <- funt a
                               (y, expression)  <- fune1 x op1
                               if expression == ETY
                               then Return (y, ADD op0 op1)
                               else Return (y, ADD op0 expression)
fune1 (Symbol MINUS : a) op0  = do
                                (x, op1)        <- funt a
                                (y, expression) <- fune1 x op1
                                if expression == ETY
                                then Return (y, SUB op0 op1)
                                else Return (y, SUB op0 expression)
fune1 x _                     = Return (x, ETY)


-- T ::= F T1
funt:: [Token] -> Exc ([Token], LKC)
funt a = do
           (x, operand)     <- funf a
           (y, expression)  <- funt1 x operand
           if expression  == ETY
            then Return (y, operand)
            else Return (y, expression)


-- T1 ::= OPM F T1 | epsilon
--
-- OPM ::= * | /
funt1:: [Token] -> LKC -> Exc ([Token], LKC)
funt1 (Symbol TIMES : a) op0    = do
                                    (x, op1)        <- funf a
                                    (y, expression) <- funt1 x op1
                                    if expression == ETY
                                      then Return (y, MULT op0 op1)
                                      else Return (y, MULT op0 expression)
funt1 (Symbol DIVISION : a) op0 = do
                                    (x, op1)        <- funf a
                                    (y, expression) <- funt1 x op1
                                    if expression == ETY
                                      then Return (y, DIV op0 op1)
                                      else Return (y, DIV op0 expression)
funt1 x _                       = Return (x, ETY)


-- F ::= var Y | exp_const | (ExpA)
funf:: [Token] -> Exc ([Token], LKC)
funf (Id a : b)              = do
                               (x, val) <- funy b (VAR a)
                               Return (x, val)
funf (Symbol LPAREN : b)     = do
                                (y, val)  <- expa b
                                x         <- rec_rp y
                                Return (x, val)
funf (Number a : b)          = Return (b, NUM a)
funf (Nil : b)               = Return (b, NIL)
funf (Bool a : b)            = Return (b, BOO a)
funf (String a : b)          = Return (b, STRI a)
funf (a : _)                 = Raise  ("Error in funf, found "++ show(a))


-- Y ::= (Seq_Exp) | epsilon
funy:: [Token] -> LKC -> Exc ([Token], LKC)
funy (Symbol LPAREN : b) var     =  do
                                    (x, act_params) <- seq_exp b
                                    y               <- rec_rp x
                                    Return (y, CALL var act_params)
funy x var                       =  Return (x, var)


-- Seq_Exp ::= Exp Sep_Exp | epsilon
seq_exp:: [Token] -> Exc ([Token], [LKC])
seq_exp a@(Symbol RPAREN : _)  = Return (a, [])
seq_exp a                      = do
                                  (x, val)  <- exp a
                                  (y, exps) <- sep_exp x
                                  Return (y, val : exps )


-- Seq_Var ::= var Seq_var | epsilon
seq_var:: [Token] -> Exc ([Token], [LKC])
seq_var (Id a : b)              = do
                                  (x, vars) <- seq_var b
                                  Return (x, VAR a : vars)
seq_var (Symbol RPAREN : b)     = Return (b, [])
seq_var (a : _)                 = Raise ("Error in seq_var, found "++ show(a))


-- Sep_Exp ::=  , Exp Sep_Exp | epsilon
sep_exp:: [Token] -> Exc ([Token], [LKC])
sep_exp (Symbol VIRGOLA : b)   = seq_exp b
sep_exp a@(Symbol RPAREN : b)  = Return (a, [])
sep_exp (a : _)                = Raise ("Error in sep_exp, found "++ show(a))

-- Main function : predictive parser based on an LL(1) grammar

parse:: [Token] -> LKC
parse token_list = rec_dollar ( prog token_list )
