-- LEXER: ==== TEST INTEGRAZIONE ====
module LexerIntegrationTest (
lti0,
lti1,
lti2
) where

-- TI0 : test su programma corretto per n : T
lti0 = "let k = (~233 + 10) $";

-- TI1 : T
lti1 = "letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X*FACT(  X- 1 )and G = lambda ( H L ) if  eq ( L,  nil ) then L else cons( H(car( L ) ), G ( H, cdr ( L ) )) in G ( FACT, cons(1, cons(2, cons(3, nil))) ) end $";

-- TI2 : T
lti2 = "let x=cons(\"ab\", cons(\"cd\", nil)) in if true then cons(\"01\", x) else nil end $";
