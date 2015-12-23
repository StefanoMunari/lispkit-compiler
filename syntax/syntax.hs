module Syntax (
progdoll,
dd
) where

import Lexer
import Prelude hiding (EQ,exp)

------------------------------------------------------------------------
-- Gestione delle eccezioni in modo dichiarativo

data Exc a = Raise Exception | Return a
type Exception = String

instance Show a => Show (Exc a) where
 show (Raise e)= "ERRORE:" ++ e
 show (Return x) = "RAGGIUNTO:" ++ (show x)

instance Monad Exc where
 return x  = Return x
 (Raise e) >>= q   = Raise e
 (Return x) >>= q  = q x

raise :: Exception -> Exc a
raise e = Raise e

------------------------------------------------------------------------
-- Parsing di simboli terminali

-- {let  letrec}
rec_key::[Token]-> Exc [Token]
rec_key ((Keyword LET):b)    = Return b
rec_key ((Keyword LETREC):b) = Return b
rec_key (a:b)                = Raise ("trovato " ++ show(a) ++", atteso LET o LETREC")
rec_key  x                   = Raise ("ERRORE STRANO"  ++  show(x))

rec_in::[Token]->Exc[Token]
rec_in ((Keyword IN):b)= Return b
rec_in (a:b)           = Raise ("trovato " ++ show(a) ++ ", atteso IN")

rec_end::[Token]->Exc[Token]
rec_end ((Keyword END):b)= Return b
rec_end (a:b)            = Raise ("trovato " ++ show(a) ++ ", atteso END")

-- then
{-
  se è il simbolo corretto
    =>
      viene generato il token corrispondente e viene ritornata la lista restante
      incapsulata dentro la monade Exc
    ~>
      altrimenti viene sollevata un'eccezione e termina la computazione
-}
rec_then ((Keyword THEN):b)= Return b
rec_then (a:b)             = Raise ("trovato " ++ show(a) ++ ", atteso THEN")

-- else
{-
  @sameas then
-}
rec_else ((Keyword ELSE):b)= Return b
rec_else (a:b)             = Raise ("trovato " ++ show(a) ++ ", atteso ELSE")

-- (
{-
  @sameas then
-}
rec_lp ((Symbol LPAREN):b)= Return b
rec_lp (a:b)              = Raise ("trovato " ++ show(a) ++ ", atteso (")

-- )
{-
  @sameas then
-}
rec_rp ((Symbol RPAREN):b)= Return b
rec_rp (a:b)              = Raise ("trovato " ++ show(a) ++ ", attesa )")

-- ,
{-
  @sameas then
-}
rec_virg ((Symbol VIRGOLA):b)= Return  b
rec_virg (a:b)               = Raise ("trovato " ++ show(a) ++ ", attesa ,")


-- =
{-
  @sameas then
-}
rec_equals ((Symbol EQUALS):b)= Return b
rec_equals (a:b)              = Raise ("trovato " ++ show(a) ++ ", atteso =")

------------------------------------------------------------------------
-- Parsing di simboli non terminali

-- Funzione per testare parte2 : data una lista di token stampa il risultato
-- del parsing
-- (Prog + $) stampa il risultato di un Prog
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

-- Bin::= var = Exp X
{-
 Id a : il primo parametro è un'identificatore
       =>
       viene parsato il resto (b) verificando che il prossimo simbolo sia "="
       verifica che il successore di "=" sia un'espressione

 (a:_) : se non è un'identificatore viene sollevata un'eccezione
-}
bind ((Id a):b)            =  do
                               x<- rec_equals b
                               y<- exp x
                               funx y
bind (a:_)                  = Raise ("BINDER CON "++ show(a) ++" A SINISTRA")

-- X::= and Bind | epsilon
{-
 {and ...} => il successore deve essere un "Bin"
 {in ...} => ritorna l'intero input // in questo caso sto valutando FOLLOW(X)
                                    // perchè X contiene epsilon
-}
funx ((Keyword AND):b)     = bind b
funx a@((Keyword IN):b)    = Return a
funx (a:_)                 = Raise ("DOPO BINDERS; TROVATO"++show(a))

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
exp::[Token]->Exc[Token]
exp a@((Keyword LET):b)    = (prog a)
exp a@((Keyword LETREC):b) = (prog a)
exp ((Keyword LAMBDA):b)   = do
                                x<-seq_var b
                                exp x
exp ((Operator CONS):b)    = do
                                x<-rec_lp b
                                y<-exp x
                                z<-rec_virg y
                                w<-exp z
                                rec_rp w
exp ((Operator LEQ):b)     = do
                                x<-rec_lp b
                                y<-exp x
                                z<-rec_virg y
                                w<- exp z
                                rec_rp w
exp ((Operator EQ):b)      = do
                                x<-rec_lp b
                                y<-exp x
                                z<- rec_virg y
                                w<-exp z
                                rec_rp w
exp ((Operator CAR):b)      = exp b
exp ((Operator CDR):b)      = exp b
exp ((Operator ATOM):b)     = exp b
exp ((Keyword IF):b)        = do
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
fune1 ((Symbol PLUS):b)    = do
                             x<- funt b
                             fune1 x
fune1 ((Symbol MINUS):b)   = do
                             x<-funt b
                             fune1 x
fune1 x                    = Return x

-- T::= F T1
{- NOTA: @see GRAMMATICA G1
  l'input è F
  il successore è T1
-}
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
funt1 ((Symbol TIMES):b)   = do
                              x<-funf b
                              funt1 x
funt1 ((Symbol DIVISION):b)= do
                              x<-funf b
                              funt1 x
funt1 x                    = Return x

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
fuy ((Symbol LPAREN):b)      =  do
                                 x<-seq_exp b
                                 rec_rp x
fuy x                        = Return x

-- espressione costante (exp_const)
{-
  input is
    Number || Nil || Bool || String => True
    otherwise => False
-}
exp_const::Token -> Bool
exp_const (Number _)  =  True
exp_const Nil         =  True
exp_const (Bool _)    =  True
exp_const (String _)  =  True
exp_const  _          = False

-- Seq_Var ::= var Seq_var | epsilon
{-
  Nessuno dei precedenti => epsilon
-}
seq_var:: [Token]-> Exc[Token]
seq_var x = Return x  -- da completare ......................................
