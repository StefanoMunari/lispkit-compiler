module CompilerTest(
c0,
c1,
c2,
c3,
c4
)
where

c0="letrec  FACT = lambda ( X ) "++
    "if  eq ( X, 0 ) "++
  "then 1 "++
  "else  X*FACT(  X - 1 ) "++ 
      "and G = lambda ( H L ) "++ 
        "if  eq ( nil, L ) "++
      "then L "++
      "else cons( H(car( L ) ), G ( H, cdr ( L ) )) "++
     "in G ( FACT, cons(1 ,cons(2, cons(3, nil))) ) "++
     "end $"
c1="let x= 5 and y= 6 in x*3 + y * 2* x + x*y end $"
c2="letrec f0 = lambda (x) "++
    "if eq(x,0) "++
        "then true "++
        "else f0(x-1) "++
    "in f0(5) "++
    "end $"
c3="let x= 5 and y= 6 in x + y end $"
c4="letrec fact= lambda(x y) if eq(x,0) then y else fact(x-1, x*y) "++
    "in fact(5,1) end $"
