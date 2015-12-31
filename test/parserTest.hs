-- @tofix: tpnt6, tpnt7, tpnt18
module ParserTest (
tpt0,
tpt1,
tpt2,
tpt3,
tpt4,
tpt5,
tpt6,
tpt7,
tpt8,
tpt9,
tpt10,
tpt11,
tpt12,
tpt13,
tpt14,
tpt15,
tpnt0,
tpnt1,
tpnt2,
tpnt3,
tpnt4,
tpnt5,
tpnt6,
tpnt7,
tpnt8,
tpnt9,
tpnt10,
tpnt11,
tpnt12,
tpnt13,
tpnt14,
tpnt15,
tpnt16,
tpnt17,
tpnt18,
tpnt19
)
where

--tpx= "let x=5 and y= 6 in x + y * 2 end $";

-- Test sui parser di simboli terminali : TPT

-- TPT0 : F
-- rec_key
tpt0 = "x=5 in x end $";

-- TPT1 : T
-- rec_key, rec_in, rec_end, rec_dollar
tpt1 = "let x=5 in x end $";

-- TPT2 : T
-- rec_key, rec_in, rec_end
tpt2 = "letrec x=5 in x end $";

-- TPT3 : F
-- rec_in -> OK ma Raise in funx
tpt3 = "let x=5 x end $";

-- TPT4 : F
-- rec_end
tpt4 = "let x=5 in x $";

-- TPT5 : F
-- rec_then
tpt5 = "let x= if true false else true in x $";

-- TPT6 : T
-- rec_then, rec_else
tpt6 = "let x= if true then false else true in x end $";

-- TPT7 : F
-- rec_else
tpt7 = "let x= if true then false true in x end $";

-- TPT8 : F
-- rec_lp
tpt8 = "let x= lambda y) y in x(3) end $";

-- TPT9 : T
-- rec_lp, rec_rp
tpt9 = "let x= lambda (y) y in x(3) end $";

-- TPT10 : F
-- rec_rp -> OK ma Raise in seq_var
tpt10 = "let x= lambda (y y in x(3) end $";

-- TPT11 : F
-- rec_virg -> OK ma Raise in sep_exp
tpt11 = "let x= lambda (a b c) a+b+c in x(1 2 3) end $";

-- TPT12 : T
-- rec_virg
tpt12 = "let x= lambda (a b c) a+b+c in x(1, 2, 3) end $";

-- TPT13 : F
-- rec_equals
tpt13 = "let x lambda (a) a in x(1) end $";

-- TPT14 : T
-- rec_equals
tpt14 = "let x = lambda (a) "++
                    "let y = 3 " ++
                    "in y+a "++
                    "end "++
        "in x(2) "++
        "end $";

-- TPT15 : F
-- rec_dollar -> OK ma error in lexi
tpt15 = "let x=5 in x end ";

-- Test sui parser di simboli NON terminali : TPNT

-- TPNT0 : F
-- bind
tpnt0 = "let let x=5 in x end $";

-- TPNT1 : T
-- bind
tpnt1 = "let x=5 in x end $";

-- TPNT2 : F
-- funx
tpnt2 = "let x=5 y=7 in x end $";

-- TPNT3 : T
-- funx {in, and}
tpnt3 = "let x=5 and y=7 in x+y end $";

-- TPNT4 : T
-- exp {cons, eq, atom}
tpnt4 = "let x= lambda (a b c) " ++
                "eq (a, b) " ++
            "in " ++
        "x ( cons(0, cons(1, nil)), cons(0, cons(1, nil)), atom(3) ) " ++
            "end $";

-- TPNT5 : T
-- exp {car}
tpnt5 = "let x= lambda (a) " ++
                    "car(a) " ++
        "in " ++
            "x ( cons(1, nil) ) " ++
        "end $";

-- TPNT6 : F 
-- exp {car}
-- Result :
-- *** Exception: "ERRORE in funf, TROVATO Operator CONS"
-- FIX: modificare in modo da permettere a CAR e CDR di accettare cons() non
-- ancora tradotti in LKC
tpnt6 = "let x= lambda (a) " ++
                    "a " ++
        "in " ++
            "x ( car(cons(1, nil)) ) " ++
        "end $";

-- TPNT7 : F 
-- exp {car}
-- Result :
-- LETC (CALL (VAR "x") [CARC (NUM 1)]) [(VAR "x",LAMBDAC [VAR "a"] (VAR "a"))]
-- FIX: dovrebbe accettare SOLO LISTE e generare un errore se passo un NUM
-- correggere per CAR e CDR
tpnt7 = "let x= lambda (a b) " ++
                    "a " ++
        "in " ++
            "x ( car(1), cdr(2) )" ++
        "end $";

-- TPNT8 : T
-- exp {leq, if}
tpnt8 = "let x = lambda (a b) " ++
                    "if leq(a,b) then 1 else 0 " ++
        "in " ++
            "x (1, 2) " ++
        "end $";

-- TPNT9 : F
-- expa, fune1
tpnt9 = "let x = 2 +  " ++
        "in " ++
            "x " ++
        "end $";

-- TPNT10 : T
-- expa, fune1
tpnt10 = "let x = 2 + 3 " ++
        "in " ++
            "x " ++
        "end $";

-- TPNT11 : T
-- expa
tpnt11 = "let x = lambda (a b) a + b " ++
        "in " ++
            "x ( 2+(~2), 4) " ++
        "end $";

-- TPNT12 : T
-- funt, funt1
tpnt12 = "let x = lambda (a b c) (a * b) / c " ++
        "in " ++
            "x ( 2*2, 4/2 , 3) " ++
        "end $";

-- TPNT13 : F
-- funt, funt1
tpnt13 = "let x = lambda (a b c) (a b) / c " ++
        "in " ++
            "x ( 2*2, 4/2 , 3) " ++
        "end $";

-- TPNT14 : F
-- funt, funf
tpnt14 = "let x = lambda (a b c) (a * ) / c " ++
        "in " ++
            "x ( 2*2, 4/2 , 3) " ++
        "end $";

-- TPNT15 : T
-- funt, funf, funy, seq_exp, sep_expr
tpnt15 = "let x = lambda (a b c d) cons(a, b) " ++
        "in " ++
            "x ( (22), nil , true, \"fish\") " ++
        "end $";

-- TPNT16 : F
-- seq_var
tpnt16 = "let x = lambda (a , ) cons(a, b) " ++
        "in " ++
            "x ( \"blow\", \"fish\") " ++
        "end $";

-- TPNT17 : T
-- seq_var
tpnt17 = "let x = lambda (a b) cons(a, b) " ++
        "in " ++
            "x ( \"blow\", \"fish\") " ++
        "end $";

-- TPNT18 : F
-- Result:
-- LETC (CALL (VAR "x") []) [(VAR "x",LAMBDAC [VAR "a",VAR "b"] (CONSC (VAR "a") (VAR "b")))]
-- FIX: aggiungere un controllo che sollevi un'eccezione nel caso di mismatch
-- sull'arietà dei parametri formali / attuali
tpnt18 = "let x = lambda (a b) cons(a, b) " ++
        "in " ++
            "x () " ++
        "end $";

-- TPNT19 : F
-- sep_expr
tpnt19 = "let x = lambda (a b) cons(a, b) " ++
        "in " ++
            "x ( \"blow\" \"fish\") " ++
        "end $";