module SyntaxTest (
tc0,
tc1,
tc2
) 
where

-- ===TEST SU PROGRAMMI BASE COMPLETI===

-- TC0
{-
	definisce una costante
-}
tc0 = "let k = (~233 + 10) $";

-- TC1
{-
  definizione ricorsiva di fattoriale applicato a 
  tutti gli elementi di una lista di interi
-}
tc1 = "letrec  FACT = lambda ( X )"
-- if  eq ( X, 0 ) then 1 else  X*FACT(  X- 1 ) and G = lambda ( H L ) if  eq ( L,  nil ) then L else cons( H(car( L ) ), G ( H, cdr ( L ) )) in G ( FACT, cons(1, cons(2, cons(3, nil))) ) end $"; 
-- TC2 : T 
{-
	testa liste e if then else
-}
tc2 = "let x=cons(\"ab\", cons(\"cd\", nil)) in if true then cons(\"01\", x) else nil end $";