-- Compilatore LKC
-- @see compiler-document.pdf => Compilazione da LKC → a SECD
module Compiler(
    Secdexpr(..),
    compile
)
where

import Lexer
import Parser
{-
import LexerTest
import SyntaxTest
import ParserTest
-}

data Secdexpr = Add                       -- +
              | Sub                       -- -
              | Mult                      -- *
              | Div                       -- /
              | Rem                       -- %
              | Eq                        -- ==
              | Leq                       -- <=
              | Car                       -- testa della lista
              | Cdr                       -- coda della lista
              | Cons                      -- costruttore di lista
              | Atom                      -- identifica argomenti "non lista"
              | Join                      -- ripristina il controllo salvato su Dump prima di eseguire Sel [Secdexpr] [Secdexpr]
              | Rtn                       -- return : esegue il rispristino della tripla (S E C) dal Dump
              | Stop                      -- terminazione : ultima istruzione del programma
              | Push                      -- carica un [OGA] in Enviroment
              | Ap                        -- applicazione di funzione : corrisponde a CALL
              | Rap                       -- Ap ricorsiva : sostituisce [OGA] con l'ambiente richiesto dai binder ricorsivi
              | Ld (Integer, Integer)     -- carica un valore da Enviroment a Stack
              | Ldc LKC                   -- carica un valore costante {NUM, BOO, STRI, NIL}
              | Sel [Secdexpr] [Secdexpr] -- condizionale {if then else}
              | Ldf [Secdexpr]            -- definizione di funzione
               deriving(Show, Eq)


-- funzioni per il calcolo dell'indirizzo di una variabile nell'ambiente
-- calcola l'indice di una variabile nella lista data
position:: String -> [LKC] -> Integer
position _ []           = error "position: non trova la variabile"
position x (VAR z : y)  = if z == x then 0 else 1 + (position x y)
position _ _            = error "position: trovata non VAR"

-- true <-> la variabile è definita
-- false altrimenti
member:: String -> [LKC] -> Bool
member _ []           = False
member x (VAR z : y)  = if x == z then True else member x y
member x _            = error ("member: trovata non VAR "++ x)

-- Integer è l'indice che conta i record di attivazione che stiamo percorrendo
-- ogni record di attivazione è un lista di nomi
-- e quindi l'ambiente statico è una lista di liste [[LKC]]
-- viene prodotto il conto dei record di attivazione(Integer)
-- e la posizione all'interno del record(Integer)
-- calcola l'indice di una variabile in Enviroment
location:: String -> Integer -> [[LKC]] -> (Integer, Integer)
location x _ []     = error ("location non trova VAR"++ x)
location x ct (n:m) = if (member x n) then (ct, position x n) else  location x (ct+1) m


sexpr_reverse::[a]->[a]
sexpr_reverse []= []
sexpr_reverse (a:b)= (sexpr_reverse b) ++ [a]


-- rimuove espressioni da una lista di binders
vars:: [(a, b)] -> [a]
vars []            = []
vars ((x, _) : r)  = x : vars r

-- rimuove variabili da una lista di binders
exprs:: [(a, b)] -> [b]
exprs []           = []
exprs((_, y) : r)  = y : exprs r

-- considera la lista di espressioni (primo parametro)
-- che corrispondono ai valori relativi
-- alle variabili compilate precedentemente (Let var = value in ... end $):
-- ne compila ogni elemento inserendolo in testa alla lista di istruzioni
-- compilate, in questo modo si mantiene la corrispondenza con le variabili/identificatori
-- precedentemente compilati.
-- arrivata in fondo alla lista:
--   inserisce la costante NIL in testa alle istruzioni compilate -> RA completato
--   quindi come risultato della compilazione so che dopo ogni NIL ci sarà la definizione di una chiusura
--           e         n           c      ->   c':c
complist:: [LKC] -> [[LKC]] -> [Secdexpr] -> [Secdexpr]
complist [] _ c      = Ldc NIL : c
complist (x : y) n c = complist y n (comp x n (Cons : c))

-- il codice viene prodotto da dx a sx, infatti :c
-- e := programma
-- n := ambiente statico
-- c := ciò che è stato compilato
--
-- IFC
-- x :=  test
-- y :=  ramo then
-- z :=  ramo else
-- in fondo mettiamo la Join
-- Sel controlla quello che c'è in cima alla pila
-- se True esegue then se False esegue else
--
-- LAMBDA e CALL assieme fanno una LET
-- LAMBDA := corpo di una funzione più variabili
-- CALL := lista di parametri attuali utilizzati per sostituire i formali e invocare la LAMBDA
--
-- LAMBDA
-- x := parametri formali
-- y := body
-- Ldf := creazione della chiusura della funzione
--
-- CALL
-- x := nome funzione da invocare
-- y := parametri attuali
-- STATO STACK
----------------
-- (body,E')
-- [parametri]
--  S
---------------
-- complist y n := carica i parametri attuali
-- comp x n := carica il corpo della funzione
-- =====================================
-- Compila un programma LKC in una sequenza di comandi SECD eseguibili dall'interprete
comp:: LKC -> [[LKC]] -> [Secdexpr] -> [Secdexpr]
comp e n c =  case e of
                        (VAR x)       -> (Ld (location x 0 n)):c
                        (NUM x)       -> (Ldc (NUM x)):c
                        (BOO x)       -> (Ldc (BOO x)):c
                        (STRI x)      -> (Ldc (STRI x)):c
                        NIL           -> (Ldc NIL):c
                        (ADD x y)     -> comp y n (comp x n (Add:c))
                        (SUB x y)     -> comp y n (comp x n (Sub:c))
                        (MULT x y)    -> comp y n (comp x n (Mult:c))
                        (DIV x y)     -> comp y n (comp x n (Div:c))
                        (REM x y)     -> comp y n (comp x n (Rem:c))
                        (EQC x y)     -> comp y n (comp x n (Eq:c))
                        (LEQC x y)    -> comp y n (comp x n (Leq:c))
                        (CARC x)      -> comp x n (Car:c)
                        (CDRC x)      -> comp x n (Cdr:c)
                        (CONSC x y)   -> comp y n (comp x n (Cons:c))
                        (ATOMC x)     -> comp x n (Atom:c)
                        (IFC x y z)   -> let
                                            thenp = comp y n [Join]
                                            elsep = comp z n [Join]
                                          in
                                            comp x n  ( Sel thenp elsep : c)
                        (LAMBDAC x y) -> (Ldf (comp y (x:n) [Rtn])):c
                        {-
                          x := body di LETC
                          y := [(id, value)] detti anche binders

                          -> separo gli identificatori (vars) dai valori (exprs)
                          -> compilo:
                                    - valori associati alle variabili (parte dx binders)
                                      (non sono i parametri attuali dell'invocazione:
                                      questi sono contenuti nella parte dopo "in" di "let"
                                      identificata da x, che infatti viene compilata subito dopo gli identificatori "varl")
                                    - ambiente statico attuale
                                    - istruzioni SECD -> compilate:
                                      * Ldf -> definizione di funzione (creo la chiusura):
                                          body di LET           -> programma da compilare
                                          identificatori        -> ambiente statico
                                          istruzione di ritorno -> compilata e quindi verrà messa in fondo alla chiusura da compilare,
                                                                   quando verrà eseguita la chiusura sarà l'ultima istruzione
                                      * Ap : esegue la funzione messa sopra
                                      * c  : ciò che è stato compilato finora
                        -}
                        -- LETC
                        -- verrà prodotta
                        -- > la lista dei parametri attuali (parte dx dei binders)
                        -- > un Ldf(body)
                        -- > :Ap
                        -- questo verrà prodotto utilizzando complist
                        -- l'ambiente statico è composto da (parti sx dei binders):n
                        -- l'ambiente statico è composto da SOLO n quando compilo la parte dx dei binders,
                        -- perchè è una LETC non ricorsiva, non posso usare i parametri che sto definendo.
                        -- in LETREC invece le cose cambiano, il body lo dovrò compilare nello stesso
                        -- ambiente statico di (parti sx dei binders) perchè posso utilizzare le definizioni ricorsive dentro la chiusura stessa
                        (LETC x y)    -> let
                                              varl  = vars  y -- produce una lista di variabili
                                              exprl = exprs y -- produce una lista di espressioni
                                         in --               (comp par_att par_form:n return)
                                              complist exprl n (Ldf(comp x (varl:n) [Rtn]) : Ap : c)
                        -- LETREC
                        -- verrà prodotta
                        -- > la lista dei parametri attuali (parte dx dei binders)
                        -- > un Ldf(body)
                        -- > :Rap
                        -- Ldf(body) := (parti sx binders):n
                        -- quindi nell'ambiente statico compilo con (parte dx dei binders):(parti sx binders):n
                        -- 1)
                        -- Rap eseguirà delle manipolazioni sulla lista di parametri(funzionali)
                        -- per rendere attuabile la ricorsione
                        -- 2) mette push in testa alla costruzione della parte dx dei binders
                        -- PUSH:
                        -- mette su l'ambiente dinamico un RA falso, un [OGA]
                        -- Ogni LETREC ha in testa un [OGA] che serve per mettere
                        -- a posto gli indirizzi del compilatore quando fa la chiamata ricorsiva
                        -- serve per fare tornare i conti dei RA
                        (LETRECC x y) -> let
                                              varl  = vars y
                                              exprl = exprs y
                                         in --[OGA]: aggiungo in testa all'ambiente statico la lista di parametri da sostituire ad [OGA]
                                              Push : complist exprl (varl:n) (Ldf(comp x (varl:n) [Rtn]) : Rap : c)
                        (CALL x y)    -> complist y n (comp x n (Ap:c))
                        _             -> [];


--esempi di prova

--c="letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X*FACT(  X - 1 ) and G = lambda ( H L ) if  eq ( nil, L ) then L else c--ons( H(car( L ) ), G ( H, cdr ( L ) )) in G ( FACT, cons(1 ,cons(2, cons(3, nil))) ) end $"



--d= "let x= 5 and y= 6 in x*3 + y * 2* x + x*y end $"

-- x := programma LKC da compilare
compile:: LKC -> [Secdexpr]
compile lkc = comp lkc [] []
