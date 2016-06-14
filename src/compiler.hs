module Compiler(
    Secdexpr(..),
    compile
)
where

import Lexer
import Parser

-- Datatype Secdexpr (expressions for SECD machine)

data Secdexpr = Add 
              | Sub  
              | Mult 
              | Div  
              | Eq   
              | Leq 
              | Car                                      -- head of list
              | Cdr                                      -- tail of list
              | Cons                                   -- list constructor
              | Atom                                   -- not list arguments
              | Join                                     -- restores control from D saved before Sel
              | Rtn                                      -- restores the triple (S E C) from D
              | Stop                                    -- terminates the program
              | Push                                    -- push [OGA] placeholder in E
              | Ap                                        -- applies a function
              | Rap                                      -- recursive apply: replaces [OGA] with E required by recursive binders
              | Ld (Integer, Integer)             -- loads a value from E to S
              | Ldc LKC                               -- loads a constant value {NUM, BOO, STRI, NIL} from E to S
              | Sel [Secdexpr] [Secdexpr]   -- selection : {if then else}
              | Ldf [Secdexpr]                      -- function definition
               deriving(Show, Eq)

-- Utility functions for the compiler

position:: String -> [LKC] -> Integer
position _ []           = error "position: variable not found"
position x (VAR z : y)  = if z == x then 0 else 1 + (position x y)
position _ _            = error "position: found another element instead of VAR"


member:: String -> [LKC] -> Bool
member _ []           = False
member x (VAR z : y)  = if x == z then True else member x y
member x _            = error ("member: found another element instead of VAR "++ x)


location:: String -> Integer -> [[LKC]] -> (Integer, Integer)
location x _ []     = error ("location: VAR not found"++ x)
location x ct (n:m) = if (member x n) then (ct, position x n) else  location x (ct+1) m


sexpr_reverse::[a]->[a]
sexpr_reverse []= []
sexpr_reverse (a:b)= (sexpr_reverse b) ++ [a]


vars:: [(a, b)] -> [a]
vars []            = []
vars ((x, _) : r)  = x : vars r


exprs:: [(a, b)] -> [b]
exprs []           = []
exprs((_, y) : r)  = y : exprs r

-- compiles formal parameters of LET and LETREC instructions

complist:: [LKC] -> [[LKC]] -> [Secdexpr] -> [Secdexpr]
complist [] _ c      = Ldc NIL : c
complist (x : y) n c = complist y n (comp x n (Cons : c))


-- Compiler : compiles an LKC program into a list of SECD expressions

comp:: LKC -> [[LKC]] -> [Secdexpr] -> [Secdexpr]
comp lkc env secdx = case lkc of
                        (VAR x)        -> (Ld (location x 0 env)):secdx
                        (NUM x)       -> (Ldc (NUM x)):secdx
                        (BOO x)       -> (Ldc (BOO x)):secdx
                        (STRI x)       -> (Ldc (STRI x)):secdx
                        NIL               -> (Ldc NIL):secdx
                        (ADD x y)     -> comp y env (comp x env (Add:secdx))
                        (SUB x y)     -> comp y env (comp x env (Sub:secdx))
                        (MULT x y)   -> comp y env (comp x env (Mult:secdx))
                        (DIV x y)       -> comp y env (comp x env (Div:secdx))
                        (REM x y)     -> comp y env (comp x env (Rem:secdx))
                        (EQC x y)     -> comp y env (comp x env (Eq:secdx))
                        (LEQC x y)   -> comp y env (comp x env (Leq:secdx))
                        (CARC x)      -> comp x env (Car:secdx)
                        (CDRC x)      -> comp x env (Cdr:secdx)
                        (CONSC x y) -> comp y env (comp x env (Cons:secdx))
                        (ATOMC x)    -> comp x env (Atom:secdx)
                        (IFC x y z)   -> let
                                             thenp = comp y env [Join]
                                             elsep = comp z env [Join]
                                          in
                                             comp x env  ( Sel thenp elsep : secdx)
                        (LAMBDAC x y) -> (Ldf (comp y (x:n) [Rtn])):secdx
                        (LETC x y)    -> let
                                             varl  = vars  y
                                             exprl = exprs y
                                         in
                                             complist exprl env (Ldf(comp x (varl:env) [Rtn]) : Ap : secdx)
                        (LETRECC x y) -> let
                                             varl  = vars y
                                             exprl = exprs y
                                         in
                                             Push : complist exprl (varl:env) (Ldf(comp x (varl:env) [Rtn]) : Rap : secdx)
                        (CALL x y)    -> complist y env (comp x env (Ap:secdx))
                        _             -> [];


-- Main function : calls the compiler with the LKC program as argument

compile:: LKC -> [Secdexpr]
compile lkc = comp lkc [] []
