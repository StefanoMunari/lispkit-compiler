-- ANALIZZATORE SINTATTICO - PARTE2 [parser predittivo]
-- @see syntax-2-document.pdf => Il linguaggio LKC
-- @description 
{-
  costruisce l'albero di derivazione 
  traducendolo il programma nel 
  linguaggio LKC utilizzando 
  la tecnica degli attributi semantici:
    ereditati: nel caso di espressioni aritmetiche;
    sintetizzati: per tutti gli altri casi.
-}
module Syntax (
  progdoll
) where

import Lexer
import LexerTest
import SyntaxTest
-- import ParserTest
import Prelude hiding (EQ,exp)

------------------------------------------------------------------------
-- Tipo LKC (Lispkit Concreto)

data LKC 
    = ETY --segnala epsilon productions
    | VAR     String 
    | NUM     Integer 
    | STRI    String 
    | BOO     Bool 
    | NIL 
    | ADD     LKC LKC 
    | SUB     LKC LKC 
    | MULT    LKC LKC 
    | REM     LKC LKC
    | DIV     LKC LKC 
    | EQC     LKC LKC
    | LEQC    LKC LKC
    | CARC    LKC 
    | CDRC    LKC 
    | CONSC   LKC LKC 
    | ATOMC   LKC 
    | IFC     LKC LKC LKC 
    | LAMBDAC [LKC] LKC 
    | CALL    LKC [LKC] -- funzione_da_invocare [parametri_attuali]
    | LETC    LKC [(LKC,LKC)] 
    | LETRECC LKC [(LKC, LKC)] 
    deriving(Show, Eq)

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

-- Prog::= let Bind in Exp end | letrec Bind in Exp end
prog:: [Token] -> Exc ([Token], LKC)
prog a = do
         x<-rec_key a
         y<-bind x
         z<-rec_in y
         w<-exp z
         rec_end w

-- Bind::= var = Exp X
{-
  z -> 
      lista di Token ancora da parsare
  (VAR a, expr) : binders ->  
                            ogni binder è rappresentato da una coppia 
                            (identificatore, espressione_associata)
                            a questo seguirà una lista 
                            di n binders t.c. 0 <= n < N
                            perchè bind è mutuamente ricorsiva con funx
  otherwise -> 
              qualsiasi altro elemento solleva un'eccezione
-}
bind:: [Token] -> Exc ([Token], [(LKC,LKC)])
bind (Id a : b)            =  do
                              x            <- rec_equals b -- scarto il token "=" perchè non serve 
                              (y, expr)    <- exp x        -- nell'albero di derivazione che sto costruendo
                              (z, binders) <- funx y
                              Return (z, (VAR a, expr) : binders)
bind (a : _)               =  Raise ("BINDER CON "++ show(a) ++" A SINISTRA")

-- X::= and Bind | epsilon
{-
  AND -> 
        genera un altro binder passando il successore b
  IN ->
        ritorna la lista dei prossimi token da analizzare
        e una lista vuota in quanto non ha nessun valore
        LKC da inserire
  otherwise -> 
              qualsiasi altro elemento solleva un'eccezione
  Nota:
    il tipo [(LKC,LKC)] è conforme al tipo della funzione bind
    perchè mutuamente ricorsiva
-}
funx:: [Token] -> Exc ([Token], [(LKC,LKC)])
funx (Keyword AND : b)     = bind b -- scarto il token "AND", non serve nell'albero
funx a@(Keyword IN : _)    = Return (a, []) -- binders terminati
                                            -- la keyword IN permette
                                            -- di segnalare la fine dei
                                            -- binders e l'inizio della parte
                                            -- dx del {let letrec},
                                            -- viene riconosciuta in
                                            -- prog subito dopo i binder
                                            -- attraverso rec_in
funx (a : _)               = Raise ("DOPO BINDERS; TROVATO"++show(a))

-- Exp ::= Prog | lambda(Seq_Var) Exp | ExpA | OPP(Seq_Exp) |
--         if Exp then Exp else Exp
-- NOTA: contiene OPP::= cons | car | cdr | eq | leq | atom
exp:: [Token] -> Exc ([Token], LKC)
exp a@(Keyword LET : b)    = prog a
exp a@(Keyword LETREC : b) = prog a
exp (Keyword LAMBDA : b)   = do
                                x                <- rec_lp b -- scarto il token "(", non serve nell'albero
                                (y, parameters)  <- seq_var x
                                (z, body)        <- exp y
                                Return (z, LAMBDAC parameters body)
exp (Operator CONS : b)    = do
                                w <- rec_lp b
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
                                (w, list) <- exp b
                                Return (w, CARC list)
exp (Operator CDR : b)      = do
                                (w, list) <- exp b
                                Return (w, CDRC list)
exp (Operator ATOM : b)     = do
                                (w, constant) <- exp b
                                Return (w, ATOMC constant)
exp (Keyword IF : b)        = do
                                (w, condition)   <- exp b
                                x <- rec_then w
                                (y, consequent)  <- exp x
                                z <- rec_else y
                                (k, alternative) <- exp z
                                Return (k, IFC condition consequent alternative)
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
    {identificatore ..} =>
            verifica che il successore sia "Y"
    {( ..} =>
            verifica che il successore sia "ExpA"
            verifica che il successore di "ExpA"
            sia ")"                     
    {Number || Nil || Bool || String ..} => 
                                    successore
    {.. } => eccezione
-}
funf:: [Token] -> Exc [Token]
funf (Id a : b)              = funy b 
funf (Symbol LPAREN : b)     = do
                              x<- expa b
                              rec_rp x
funf (Number a : b)          = Return b
funf (Nil : b)               = Return b
funf (Bool a : b)            = Return b
funf (String a : b)          = Return b
funf (a : _)                 = Raise ("ERRORE in funf, TROVATO"++ show(a))

-- Y :: = (Seq_Exp) | epsilon
{-
  Se l'input inizia con "("
                          => controlla che il successore sia "Seq_Exp"
                             controlla che il successore di "Seq_Exp" sia ")"
  altrimenti => epsilon
-}
funy:: [Token] -> Exc [Token]
funy (Symbol LPAREN : b)      =  do
                                 x <- seq_exp b
                                 rec_rp x
funy x                        = Return x

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
seq_exp:: [Token] -> Exc [Token]
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
seq_var:: [Token] -> Exc ([Token], [LKC])
seq_var (Id a : b)              = seq_var b
seq_var (Symbol RPAREN : b)     = Return b
seq_var (a: _)                  = Raise ("ERRORE in seq_var, TROVATO "++ show(a))

-- Sep_Exp ::=  , Exp Sep_Exp | epsilon
{-
  {, ..} =>
      verifica che il successore di "," sia un "Seq_Exp" (cioè "Exp Sep_Exp")
  {) ..} =>
      ritorna l'intero input, ha finito di calcolare la lista di parametri
      (contenuta nella prima parte di a)
-}
sep_exp:: [Token] -> Exc [Token]
sep_exp (Symbol VIRGOLA : b)   = seq_exp b
sep_exp a@(Symbol RPAREN : b)  = Return a
