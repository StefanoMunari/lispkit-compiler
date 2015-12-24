-- ANALIZZATORE SINTATTICO - PARTE1 [parser predittivo]
-- @see syntax-1-document.pdf
-- @see syntax-2-document.pdf => part3: il parser predittivo
-- @description controlla che il programma sia sintatticamente corretto
module Syntax (
  progdoll
) where

import Lexer
import LexerTest
import SyntaxTest
import Prelude hiding (EQ,exp)

------------------------------------------------------------------------
-- Gestione delle eccezioni in modo dichiarativo

data Exc a = Raise Exception | Return a
type Exception = String

instance Show a => Show (Exc a) where
 show (Raise e)= "ERRORE:" ++ e
 show (Return x) = "RAGGIUNTO:" ++ (show x)

-- fmap :: Functor f => (a -> b) -> f a -> f b
instance Functor Exc where
  fmap fun (Return x) = Return (fun x)
  fmap fun (Raise e)  = Raise e

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
instance Applicative Exc where
  pure             = Return
  (Raise e) <*> _  = Raise e
  (Return fun) <*> q = fmap fun q

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
instance Monad Exc where
 return            = Return
 (Raise e) >>= _   = Raise e
 (Return x) >>= funMon  = funMon x
-- ex: (Return 3) >>= (\y->Return (3+y)) = RAGGIUNTO:6

raise :: Exception -> Exc a
raise e = Raise e

------------------------------------------------------------------------
-- Parsing di simboli terminali

-- riconosce : {let letrec}
{-
  if (simbolo corretto)
    then
      viene generato il token corrispondente e viene ritornata la lista restante
      incapsulata dentro la monade Exc
    else
      viene sollevata un'eccezione e termina la computazione
-}
rec_key:: [Token] -> Exc [Token]
rec_key (Keyword LET : b)    = Return b
rec_key (Keyword LETREC : b) = Return b
rec_key (a : _)              = Raise ("trovato " ++ show(a) ++", atteso LET o LETREC")
rec_key  x                   = Raise ("ERRORE STRANO"  ++  show(x))

-- riconosce : in
{-
  @sameas {let letrec}
-}
rec_in:: [Token] -> Exc[Token]
rec_in (Keyword IN : b)= Return b
rec_in (a : _)         = Raise ("trovato " ++ show(a) ++ ", atteso IN")

-- riconosce : end
{-
  @sameas {let letrec}
-}
rec_end:: [Token] -> Exc [Token]
rec_end (Keyword END : b)= Return b
rec_end (a : _)          = Raise ("trovato " ++ show(a) ++ ", atteso END")

-- riconosce : then
{-
  @sameas {let letrec}
-}
rec_then:: [Token] -> Exc [Token]
rec_then (Keyword THEN : b)= Return b
rec_then (a : _)           = Raise ("trovato " ++ show(a) ++ ", atteso THEN")

-- riconosce : else
{-
  @sameas {let letrec}
-}
rec_else:: [Token] -> Exc [Token]
rec_else (Keyword ELSE : b)= Return b
rec_else (a : _)           = Raise ("trovato " ++ show(a) ++ ", atteso ELSE")

-- riconosce : (
{-
  @sameas {let letrec}
-}
rec_lp:: [Token] -> Exc [Token]
rec_lp (Symbol LPAREN : b)= Return b
rec_lp (a : _)            = Raise ("trovato " ++ show(a) ++ ", atteso (")

-- riconosce : )
{-
  @sameas {let letrec}
-}
rec_rp:: [Token] -> Exc [Token]
rec_rp (Symbol RPAREN : b)= Return b
rec_rp (a : _)            = Raise ("trovato " ++ show(a) ++ ", attesa )")

-- riconosce : ,
{-
  @sameas {let letrec}
-}
rec_virg:: [Token] -> Exc [Token]
rec_virg (Symbol VIRGOLA : b)= Return  b
rec_virg (a : _)             = Raise ("trovato " ++ show(a) ++ ", attesa ,")


-- riconosce : =
{-
  @sameas {let letrec}
-}
rec_equals:: [Token] -> Exc [Token]
rec_equals (Symbol EQUALS : b)= Return b
rec_equals (a : _)            = Raise ("trovato " ++ show(a) ++ ", atteso =")

------------------------------------------------------------------------
-- Parsing di simboli non terminali

-- Funzione per testare PARTE1 [parser predittivo]: 
-- data una lista di token stampa il risultato del parsing
-- Risultato corretto => RAGGIUNTO: 
-- Risultato errato => ERRORE:
-- (Prog + $)
progdoll::[Token] -> String
progdoll x= show (prog x)

-- Prog::= let Bind in Exp end | letrec Bind in Exp end
prog:: [Token] -> Exc [Token]
prog a = do
         x<-rec_key a
         y<-bind x
         z<-rec_in y
         w<-exp z
         rec_end w

-- Bind::= var = Exp X
{-
 Id a : il primo parametro è un'identificatore
       =>
       viene parsato il resto (b) verificando che il prossimo simbolo sia "="
       verifica che il successore di "=" sia un'espressione

 (a:_) : se non è un'identificatore viene sollevata un'eccezione
-}
bind:: [Token] -> Exc [Token]
bind (Id a : b)            =  do
                               x<- rec_equals b
                               y<- exp x
                               funx y
bind (a : _)                  = Raise ("BINDER CON "++ show(a) ++" A SINISTRA")

-- X::= and Bind | epsilon
{-
 {and ...} => il successore deve essere un "Bind"
 {in ...} => ritorna l'intero input // in questo caso sto valutando FOLLOW(X)
                                    // perchè X contiene epsilon
-}
funx:: [Token] -> Exc [Token]
funx (Keyword AND : b)     = bind b
funx a@(Keyword IN : _)    = Return a
funx (a : _)               = Raise ("DOPO BINDERS; TROVATO"++show(a))

-- Exp ::= Prog | lambda(Seq_Var) Exp | ExpA | OPP(Seq_Exp) |
--         if Exp then Exp else Exp
-- NOTA: contiene OPP::= cons | car | cdr | eq | leq | atom
{-
  {let letrec ...} => prog dell'intero input // perchè prog verifica che
                                             // l'intera definizione
                                             // dell'espressione
                                             // sia corretta e non solo la testa
  {lambda ...} => @todo
                   verifica che il successore di seq_var sia un'espressione
  {cons ...} =>
                verifica che sia il successore sia "("
                verifica che il successore di "(" sia un'espressione
                verifica che il successore dell'espressione sia "," // cons
                                                // costruisce una lista sempre
                                                // con 2 elementi cons(car, cdr)
                verifica che il successore di "," sia un'espressione
                verifica che il successore dell'espressione sia ")"
  {leq ...} =>
              @sameas Exp::{cons ...}
  {eq ...} =>
              @sameas Exp::{cons ...}
  {car ...} =>
              verifica che il successore sia un'espressione
  {cdr ...} =>
              @sameas Exp::{car ...}
  {atom ...} =>
                @sameas Exp::{car ...}
  {if ...} => NOTA: if <-> else
              verifica che il successore sia un'espressione
              verifica che il successore dell'espressione sia "then"
              verifica che il successore di "then" sia un'espressione
              verifica che il successore dell'espressione sia "else"
              verifica che il successore di "else" sia un'espressione

-}
exp:: [Token] -> Exc [Token]
exp a@(Keyword LET : b)    = prog a
exp a@(Keyword LETREC : b) = prog a
exp (Keyword LAMBDA : b)   = do
                                x<-rec_lp b
                                y<-seq_var x
                                exp y
exp (Operator CONS : b)    = do
                                x<-rec_lp b
                                y<-exp x
                                z<-rec_virg y
                                w<-exp z
                                rec_rp w
exp (Operator LEQ : b)     = do
                                x<-rec_lp b
                                y<-exp x
                                z<-rec_virg y
                                w<- exp z
                                rec_rp w
exp (Operator EQ : b)      = do
                                x<-rec_lp b
                                y<-exp x
                                z<- rec_virg y
                                w<-exp z
                                rec_rp w
exp (Operator CAR : b)      = exp b
exp (Operator CDR : b)      = exp b
exp (Operator ATOM : b)     = exp b
exp (Keyword IF : b)        = do
                                x<- exp b
                                y<-rec_then x
                                z<-exp y
                                w<-rec_else z
                                exp w
exp x                       =  expa x


-- ExpA::= T E1
{- NOTA: @see GRAMMATICA G1
  l'input è T
  il successore è E1
-}
expa:: [Token] -> Exc [Token]
expa a = do
           x<- funt a
           fune1 x

-- E1::= OPA T E1 | epsilon
{- NOTA: @see GRAMMATICA G1, contiene OPA::= + | -
 {+ ..} =>
         verifica che il successore sia "T"
         verifica che il successore di "T" sia "E1"
 {"-" ..} =>
         @sameas E1::{+ ..}
 {.. } => epsilon
-}
fune1:: [Token] -> Exc [Token]
fune1 (Symbol PLUS : b)    = do
                             x<- funt b
                             fune1 x
fune1 (Symbol MINUS : b)   = do
                             x<-funt b
                             fune1 x
fune1 x                    = Return x

-- T::= F T1
{- NOTA: @see GRAMMATICA G1
  l'input è F
  il successore è T1
-}
funt:: [Token] -> Exc [Token]
funt a = do
           x<-funf a
           funt1 x

-- T1::= OPM F T1 | epsilon
{- NOTA: contiene OPM::= * | /
  {* ..} =>
          verifica che il successore sia "F"
          verifica che il successore di "F" sia "T1"
  {/ ..} =>
          @sameas T1::{* ..}
  {.. } => epsilon
-}
funt1:: [Token] -> Exc [Token]
funt1 (Symbol TIMES : b)    = do
                              x<-funf b
                              funt1 x
funt1 (Symbol DIVISION : b) = do
                              x<-funf b
                              funt1 x
funt1 x                     = Return x

-- F::= var Y | exp_const | (ExpA)
{-
    se è un espressione costante
            => successore
            ~> altrimenti verifica l'intero input attraverso
                una funzione ausiliaria:
                                {identificatore ..} =>
                                        verifica che il successore sia "Y"
                                {( ..} =>
                                        verifica che il successore sia "ExpA"
                                        verifica che il successore di "ExpA"
                                        sia ")"
                                {.. } => eccezione
-}
funf:: [Token] -> Exc [Token]
funf (a:b)                 = if (exp_const a) then Return b
                                              else fX (a:b)
fX ((Id _):b)              = fuy b
fX ((Symbol LPAREN):b)     = do
                              x<- expa b
                              rec_rp x
fX (a:_)                   = Raise ("ERRORE in fX, TROVATO"++ show(a))

-- Y :: = (Seq_Exp) | epsilon
{-
  Se l'input inizia con "("
                          => controlla che il successore sia "Seq_Exp"
                             controlla che il successore di "Seq_Exp" sia ")"
  altrimenti => epsilon
-}
fuy:: [Token] -> Exc [Token]
fuy ((Symbol LPAREN):b)      =  do
                                 x <- seq_exp b
                                 rec_rp x
fuy x                        = Return x

-- espressione costante (exp_const)
{-
  input is
    Number || Nil || Bool || String => True
    otherwise => False
-}
exp_const:: Token -> Bool
exp_const (Number _)  =  True
exp_const Nil         =  True
exp_const (Bool _)    =  True
exp_const (String _)  =  True
exp_const  _          = False

-- Seq_Exp::= Exp Sep_Exp |epsilon
{-
  {) ..} =>
    ritorna l'intero input, ha finito di calcolare la lista di parametri
    (quest'ultima è contenuta nella prima parte di a)
  a =>
      verifica che a contenga "Exp", se esiste un errore questo viene gestito
      da exp
      verifica che il successore di "Exp" sia un "Sep_Exp"
-}
seq_exp:: [Token] -> Exc[Token]
seq_exp a@(Symbol RPAREN : b)  = Return a
seq_exp a                      = do
                                  x <- exp a
                                  sep_exp x

-- Seq_Var ::= var Seq_var | epsilon
{-
  {Id ..} =>
      verifica che il successore di "Id" sia un "Seq_Var"
  {) ..} =>
      ritorna l'input restante, ha finito di calcolare la lista di parametri
      (contenuta nella prima parte di a)
  Nessuno dei precedenti => eccezione
-}
seq_var:: [Token]-> Exc[Token]
seq_var (Id a : b)             = seq_var b
seq_var (Symbol RPAREN : b)    = Return b
seq_var (a:_)                  = Raise ("ERRORE in seq_var, TROVATO "++ show(a))

-- Sep_Exp ::=  , Exp Sep_Exp | epsilon
{-
  {, ..} =>
      verifica che il successore di "," sia un "Seq_Exp" (cioè "Exp Sep_Exp")
  {) ..} =>
      ritorna l'intero input, ha finito di calcolare la lista di parametri
      (contenuta nella prima parte di a)
-}
sep_exp:: [Token]-> Exc[Token]
sep_exp (Symbol VIRGOLA : b) = seq_exp b
sep_exp a@(Symbol RPAREN : b)  = Return a
