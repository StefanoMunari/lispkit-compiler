-- LEXER: ==== TEST UNITA ====
module LexerTest (
ltu0,
ltu1,
ltu2,
ltu3,
ltu4,
ltu5,
ltu6,
ltu7,
ltu8,
ltu9,
ltu10,
ltu11,
ltu12,
ltu13,
ltu14,
ltu15,
ltu16,
ltu17,
ltu18,
ltu19
) 
where

-- == TEST SUI CASI DI PATTERN MATCHING ==
-- TU0 : test su programma malformato : F
ltu0 = "";

-- TU1 : test su programma vuoto : T
ltu1 = "$"

-- TU2 : test su programma con spazi : T
ltu2 = "      $"

-- TU3 : test su programma con altre tipologie di spazi : T
ltu3 = "\t \n \f \t$"

-- TU4 : test su programma con spazi malformato : F
ltu4 = "   \n  "

-- == TEST SUI CASI DI n ==
-- ** TEST SU ~n **

-- TU5 : test su programma malformato per n : F
ltu5 = "let x = ~ $";

-- ** TEST SU n **
-- TU6 : test su programma corretto per n : T
ltu6 = "let y = 5340 $";

-- == TEST SUI CASI DI sc ==
-- TU7 : test su programma errato per sc : F
ltu7 = " \"\"\" $";

-- TU8 : test su programma corretto per sc : T
ltu8 = " \" sdfs~~~dfsd gfh 5 thwdTRHGFG-F$\" $";

-- TU9 : test su programma corretto per sc : T
ltu9 = " \" sdfs~~~dfsd gfh 5 thwdTRHGFG-F$\" $";

-- TU10 : test su programma corretto per sc : F
ltu10 = " \"~  $";

-- TU11 : test su programma corretto per sc : T
ltu11 = " \"\"$";

-- TU12 : test su programma corretto per symbol : F
ltu12 = "()=+-*/$";

-- TU13 : test su programma corretto per symbol : T
ltu13 = "() $";

-- TU14 : test su programma corretto per s : T -> variabile/identificatori
ltu14 = "abcds $";

-- TU15 : test su programma corretto per s : T -> variabili/identificatori
ltu15 = "t asdc $";

-- TU16 : test su programma corretto per s : T
ltu16 = "True and False $";

-- TU17 : test su programma corretto per s : F
ltu17 = "True and False ";

-- TU18 : test su programma errato : F -> malformed input
ltu18 = "T 0";

-- TU19 : test su programma corretto per n : T
ltu19 = "let k = (~233 + 10) $";
