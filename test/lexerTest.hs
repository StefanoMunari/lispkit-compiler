-- == TEST FOR Lexer MODULE ==
module LexerTest (
l0,
l1,
l2,
l3,
l4,
l5,
l6,
l7,
l8,
l9,
l10,
l11,
l12,
l13,
l14,
l15,
l16,
l17,
l18,
l19
)
where

-- == TEST ON PATTERN MATCHING ==
l0 = "";

l1 = "$"

l2 = "      $"

l3 = "\t \n \f \t$"

l4 = "   \n  "

--  n
l5 = "let x = ~ $";

l6 = "let y = 5340 $";

-- sc
l7 = " \"\"\" $";

l8 = " \" sdfs~~~dfsd gfh 5 thwdTRHGFG-F$\" $";

l9 = " let y = \"pippo\" in y $";

l10 = " \"~  $";

l11 = " \"\"$";

l12 = "()=+-*/$";

l13 = "() $";

-- s
l14 = "abcds $";

l15 = "t asdc $";

l16 = "True and False $";

l17 = "True and False ";

l18 = "T 0";

l19 = "let k = (~233 + 10) $";
