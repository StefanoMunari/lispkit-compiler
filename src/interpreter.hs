module Interpreter(
  Value(..),
  execute
)
where

import Lexer
import Parser
import Compiler

--------------------------------------------------------------------------------
-- Datatype Value (R-values of the LKC language)

data Value = V  LKC
            | OGA                                  -- placeholder for recursive enviroment
            | CLO [Secdexpr] [[Value]]  -- closure
            | VLISTA [Value]                 -- list of actual parameters
            deriving(Show,Eq)

-- Datatype Dump (holds temporary values of registers)

data Dump = CONTR  [Secdexpr]
          | TRIPLE [Value] [[Value]] [Secdexpr] 
          | DUMMY
          deriving(Show,Eq)


-- Utility functions for the interpreter

lazyE:: [Value] -> [Value] -> [Value]
lazyE [] _        = []
lazyE (a : b) c = lazyClo a c : lazyE b c

lazyClo:: Value -> [Value] -> Value
lazyClo (CLO a b) c   = CLO a (lazyE c c : tail b)
lazyClo (V x) _           = V x
lazyClo (VLISTA x) _ = VLISTA x 
lazyClo x _                 = error ("lazyClo: found invalid Value " ++ (show x))

index:: Integer -> [a] -> a
index 0 s   = head s
index n s   = index (n - 1) (tail s)

locate:: (Integer, Integer) -> [[Value]] -> Value
locate (a, b) e   = index b (index a e)

extract_int :: Value -> Integer
extract_int (V (NUM x)) = x
extract_int x           = error ("extract_int: found invalid argument " ++ (show x))

vhd :: Value -> Value
vhd (VLISTA (a:b))  = a
vhd (VLISTA [])       = error "vhd: empty list"
vhd _                       = error "vhd: VLISTA not found"

vtl :: Value -> Value
vtl (VLISTA (a:b))  = VLISTA b
vtl (VLISTA [])       = error "vtl: empty list"
vtl _                       = error "vtl: VLISTA not found"

vatom :: Value -> Value
vatom (V _)   = V (BOO True)
vatom _         = V (BOO False)

bool2s_espressione:: Bool -> LKC
bool2s_espressione b = if b then (BOO True) else (BOO False)

eqValue:: Value -> Value -> Bool
eqValue a@(V _) b           = eqV a b
eqValue a@(VLISTA _) b = eqVLISTA a b
eqValue a b                       = error ("eqValue: invalid equality, closures are not comparable "++ (show a) ++ (show b))

eqVLISTA:: Value -> Value -> Bool
eqVLISTA (VLISTA []) (VLISTA [])               = True
eqVLISTA (VLISTA (a : b)) (VLISTA (c : d)) = eqValue a c && eqVLISTA (VLISTA b) (VLISTA d)
eqVLISTA _ _                                               = False

eqV :: Value -> Value -> Bool
eqV (V a) (V b)  = a==b
eqV _ _              = False

------------------------------------------
-- SECD interpreter registers
------------------------------------------
-- s := stack
-- e := enviroment
-- c := control
-- d := dump
interpreter:: [Value] -> [[Value]] -> [Secdexpr] -> [Dump] -> Value
interpreter s e c d = case (head c) of
                                      Ld(b, n) -> let
                                                    x = (locate (b,n) e)
                                                  in
                                                    interpreter (x:s) e (tail c) d
                                      (Ldc k) -> case k of
                                                          NIL -> interpreter ((VLISTA []):s) e (tail c) d
                                                          _   -> interpreter ((V k):s) e (tail c) d
                                      Add -> let
                                                 operand1 = extract_int (head s)
                                                 operand2 = extract_int (head (tail s))
                                             in
                                                 interpreter ((V(NUM (operand1 + operand2))) : (tail (tail s))) e (tail c)  d

                                      Sub -> let
                                                 operand1 = extract_int (head s)
                                                 operand2 = extract_int (head (tail s))
                                             in
                                                 interpreter ((V(NUM (operand1 - operand2))) : (tail (tail s))) e (tail c)  d
                                      Mult -> let
                                                  operand1 = extract_int (head s)
                                                  operand2 = extract_int (head (tail s))
                                              in
                                                  interpreter ((V(NUM (operand1 * operand2))):(tail (tail s))) e (tail c)  d
                                      Div -> let
                                                 operand1 = extract_int (head s)
                                                 operand2 = extract_int (head (tail s))
                                             in
                                                 interpreter ((V(NUM (operand1 `div` operand2))):(tail (tail s))) e (tail c)  d
                                      Leq -> let
                                                 operand1 = extract_int (head s)
                                                 operand2 = extract_int (head (tail s))
                                             in
                                                 interpreter ((V(bool2s_espressione (operand1 <= operand2))):(tail (tail s))) e (tail c)  d
                                      Eq -> case s of
                                              (w1:w2:w3) -> interpreter ((V (bool2s_espressione (eqValue w1 w2))):w3) e (tail c) d
                                              _                 -> error "Eq: missing argument"
                                      Car -> interpreter ((vhd(head s) ):(tail s)) e (tail c) d
                                      Cdr -> interpreter ((vtl(head s) ):(tail s)) e (tail c) d
                                      Cons -> case head (tail s) of
                                                  (VLISTA x)   -> interpreter  (VLISTA ((head s):x):(tail (tail s))) e (tail c) d
                                                  x                   -> error ("Cons: the 2nd argument is not a list " ++ (show  x))
                                      Atom -> interpreter ((vatom (head s)):(tail s)) e (tail c) d

                                      Sel sl1 sl2 -> case head s of 
                                                          (V (BOO True))  -> interpreter (tail s) e sl1 ((CONTR (tail c)):d)
                                                          (V (BOO False)) -> interpreter (tail s) e sl2 ((CONTR (tail c)):d)
                                                          _                         -> error "Sel: missing BOO"

                                      Join -> case (head d) of
                                                  (CONTR c1) -> interpreter s e c1 (tail d)
                                                  _                   -> error "Join: CONTR is missing on Dump"
                                      Ldf sl -> interpreter ((CLO sl e):s) e (tail c) d
                                      Ap -> case (head s) of
                                                  (CLO c1 e1) -> case (head (tail s)) of
                                                                        VLISTA x -> interpreter [] (x:e1) c1 ((TRIPLE (tail(tail s)) e (tail c)):d)
                                                                        _             -> error "Ap: missing parameter list"
                                                  _  -> error "Ap: missing closure on Stack"

                                      Rtn ->  case (head d) of
                                                  (TRIPLE s1 e1 c1) -> interpreter ((head s):s1) e1 c1 (tail d)
                                                  _                             ->  error  "Rtn: missing TRIPLE on Dump"
                                      Rap -> case (head s) of 
                                                  (CLO c1 e1) ->  case e1 of 
                                                                        ([OGA]:re)   -> case (head (tail s)) of 
                                                                                        (VLISTA vl2) -> interpreter [] ((lazyE vl2 vl2):re) c1 ((TRIPLE (tail (tail s)) (tail e) (tail c)):d)
                                                                                        _                   -> error "Rap: missing parameter list after OGA"
                                                                        _            -> error "Rap: [OGA] not found in the recursive enviroment"
                                                  _          -> error "Rap: missing closure on Stack"
                                      Push -> interpreter s  ([OGA]:e)  (tail c)  d
                                      Stop -> (head s)

-- Main function: calls the interpreter with an SECD expression list as argument 

execute :: [Secdexpr] -> Value
execute secdexprl = interpreter [] [] (secdexprl ++ [Stop]) []
