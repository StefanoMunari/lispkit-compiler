module SyntaxTest (
ts0,
ts1,
ts2,
ts3,
ts4,
ts5,
ts6,
ts7
) 
where

-- ===TEST SU PROGRAMMI BASE COMPLETI===

-- TS0 : F
{-
    definisce una costante
-}
ts0 = "let k = (~233 + 10) $";

-- TS1 : F
{-
  definizione ricorsiva di fattoriale INCOMPLETA
-}
ts1 = "letrec  FACT = lambda ( X ) $"

-- TS2 : T 
{-
    testa liste e if then else
-}
ts2 = "let x=cons(\"ab\", cons(\"cd\", nil)) " ++
      "in if true then cons(\"01\", x) else nil " ++
      "end $";

-- TS3 : T
{-
  definizione ricorsiva di fattoriale applicato a 
  tutti gli elementi di una lista di interi
-}
ts3 = "letrec FACT = lambda (X) "++
                        "if eq(X,0) then 1 else X*FACT(X-1) and "++
                        "G = lambda (H L) "++
                            "if eq(L,nil) "++
                                "then L "++
                                "else cons(H(car(L)),G(H,cdr(L))) "++
        "in G(FACT,cons(1,cons(2,cons(3,nil)))) "++
        "end $"; 

-- TS4 : T
{-
    esempio d'uso di letrec anche con binder non funzionali 
    (nota: le variabili a sinistra non devono apparire a destra)
-}
ts4 =   "let z = 2 "++
        "in "++
            "letrec x = 2+z and y = 2*z "++
            "in x*y*z "++
            "end "++
        "end $"

-- TS5 : T
{-
    considera una lista di liste Z e produce una lista semplice che contiene
    tanti interi quante sono le liste contenute in Z e l'intero corrispondente ad
    una lista contenuta in Z è la somma dei fattoriali dei suoi elementi: 
        f2=fattoriale, 
        f1=calcola somma dei fattoriali degli elementi di una
            lista di interi
        f0=distribuisce f1 sulle liste contenute in Z *)
-}
ts5 = "letrec f0 = lambda (x) "++
                    "letrec f1 = lambda(y) "++
                                    "letrec f2 = lambda (z) " ++
                                                    "if eq(z,1) "++
                                                        "then 1 "++
                                                        "else z*f2(z-1) "++
                                    "in "++
                                        "if eq(y,nil) "++
                                            "then 0 "++
                                            "else f2(car(y)) + f1(cdr(y)) "++
                                    "end "++
                    "in "++
                        "if eq(x,nil) "++
                            "then nil "++
                            "else cons(f1(car(x)),f0(cdr(x))) "++
                    "end "++
        "in " ++
            "f0(cons(cons(3,cons(3,nil)),cons(cons(3,nil),nil))) "++
        "end $"

-- TS6 : T
{-
    esempio di funzione che restituisce una funzione locale f2
    (upwardfun result)
-}
ts6 =   "let f1 = lambda () "++
                    "letrec f2 = lambda (z) "++
                                    "if eq(z,1) then 1 else z*f2(z-1) "++
                    "in f2 "++
                    "end "++
        "in let x = f1 () "++
                    "in x(8) "++
                    "end "++
        "end $"