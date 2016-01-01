-- Compilatore LKC
-- @see compiler-document.pdf => Compilazione da LKC → a SECD
module Compiler(
    comp_one,
    Secdexpr(..)
)
where

import Parser

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
member _ _            = error ("member: trovata non VAR"++ x)

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


-- toglie espressioni da una lista di Binders
vars:: [(a, b)] -> [a]
vars []            = []
vars ((x, _) : r)  = x : vars r

-- toglie variabili da una lista di Binders
exprs:: [(a, b)] -> [b]
exprs []           = []
exprs((_, y) : r)  = y : exprs r

-- compila le istruzioni [LKC] per generare
-- una lista di parametri attuali [Secdexpr]
complist:: [LKC]-> [[LKC]] -> [Secdexpr] -> [Secdexpr]
complist [] _ c    = Ldc NIL : c
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
-- x è il corpo
-- y sono i parametri formali
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
-- 
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
-- ambiente statico di (parti sx dei binders)
-- 
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
-- serve per fare tornare i conti dei R
-- =====================================
-- Compila un programma LKC in una sequenza di comandi SECD eseguibili dall'interprete
comp:: LKC -> [[LKC]] -> [Secdexpr]->[Secdexpr]
comp e n c =  case e of 
                        (VAR x)       -> ((Ld (location x 0 n)):c)
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
                                            elsep = comp z n [Join])
                                         in comp x n  ( Sel thenp elsep : c)
                        (LAMBDAC x y) -> (Ldf (comp y (x:n) [Rtn])):c
                        (LETC x y)    ->  --DA FARE

                        (LETRECC x y) -> --DA FARE
                        (CALL x y)    -> complist y n (comp x n (Ap:c))
                        _             -> [];


--esempi di prova

--c="letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X*FACT(  X - 1 ) and G = lambda ( H L ) if  eq ( nil, L ) then L else c--ons( H(car( L ) ), G ( H, cdr ( L ) )) in G ( FACT, cons(1 ,cons(2, cons(3, nil))) ) end $"



--d= "let x= 5 and y= 6 in x*3 + y * 2* x + x*y end $"

comp_one x = comp x [] []
