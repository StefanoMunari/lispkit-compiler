module InterpreterTest(
i0,
i1,
i2,
i3
)
where

i0="letrec  FACT = lambda ( X ) "++
    "if  eq ( X, 0 ) "++
  "then 1 "++
  "else  X*FACT(  X - 1 ) "++ 
      "and G = lambda ( H L ) "++ 
        "if  eq ( nil, L ) "++
      "then L "++
      "else cons( H(car( L ) ), G ( H, cdr ( L ) )) "++
     "in G ( FACT, cons(1 ,cons(2, cons(3, nil))) ) "++
     "end $"
i1="let x= 5 and y= 6 in x*3 + y * 2* x + x*y end $"
i2="letrec f0 = lambda (x) "++
    "if eq(x,0) "++
        "then true "++
        "else f0(x-1) "++
    "in f0(5) "++
    "end $"
i3="letrec useless = lambda(x) x+0 and "++
                "min = lambda( m l ) "++
                  "if eq( nil , l) "++
                    "then "++
                      "useless(m) "++
                    "else "++
                      "if eq(eq(nil, car(l)), true) "++
                        "then useless(m) "++
                        "else "++
                            "if leq( m , car(l)) "++
                              "then min(m , cdr(l)) "++
                              "else min(car(l) , cdr(l)) "++
                  "and minimum = lambda(list) "++  
                          "if leq( car(list) , car(cdr(list))) "++
                          "then "++
                            "min( car(cdr(list)) , cdr(cdr(list))) "++
                          "else "++
                            "min( car(list) , cdr(cdr(list))) "++
        "in minimum(cons(3 ,cons(7, cons(4, cons(1, cons(~5, cons(200, cons(7, nil)))))))) "++
        "end $"