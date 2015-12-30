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
tpnt5
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

-- @tofix : *** Exception: "ERRORE in funf, TROVATO Operator CONS"
-- TPNT5 : T
-- exp {car, cdr, leq}
tpnt5 = "let x= lambda (a b) " ++
				"leq (a, b) " ++
    		"in " ++
        "x ( car( cons(1, nil) ), cdr( cons(1, nil) ) ) " ++
    		"end $";
