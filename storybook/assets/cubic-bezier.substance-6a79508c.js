const p=`Point p1, p2, p3, p4

Label p1 $P_1$
Label p2 $P_2$
Label p3 $P_3$
Label p4 $P_4$

Curve c := CurveFromPoints( p1, p2, p3, p4 )

Point p12 := Lerp( p1, p2 )
Point p23 := Lerp( p2, p3 )
Point p34 := Lerp( p3, p4 )

Label p12 $P_{12}$
Label p23 $P_{23}$
Label p34 $P_{34}$

Point p123 := Lerp( p12, p23 )
Point p234 := Lerp( p23, p34 )

Label p123 $P_{123}$
Label p234 $P_{234}$

Point p1234 := Lerp( p123, p234 )

Label p1234 $P_{1234}$

Label c $\\gamma$
`;export{p as s};
//# sourceMappingURL=cubic-bezier.substance-6a79508c.js.map
