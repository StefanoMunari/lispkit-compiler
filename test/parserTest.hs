module ParserTest (
tpt0,
tpt1,
tpt2,
tpt3,
tpt4,
tpt5,
tpt6,
tpt7
) 
where

--tpx= "let x=5 and y= 6 in x + y * 2 end $";

-- Test sui parser di simboli terminali : TPT

-- TPT0 : F
-- rec_key
tpt0 = "x=5 in x end $";

-- TPT1 : T
-- rec_key, rec_in, rec_end
tpt1 = "let x=5 in x end $";

-- TPT2 : T
-- rec_key, rec_in, rec_end
tpt2 = "letrec x=5 in x end $";

-- TPT3 : F
-- rec_in -> OK ma Raise fatto da funx
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
