-- == TEST FOR parser MODULE ==
module ParserTest (
p0,
p1,
p2,
p3,
p4
)
where

p0 = "let x= y = in p $";

p1 = "let y= lambda in $"

p2 = "let in 5 $"

p3 = "let x=\"peter henderson\" in x $"

p4 = "let x=\"peter henderson\" in x end $"
