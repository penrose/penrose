import{m as r}from"./resolver-8e6b3de6.js";const e=r("matrix-ops"),o=`canvas {
    width = 240
    height = 200
}

global {
   scalar scale = 18.0
   string labelSize = "10px"

   shape box = Rectangle {
      center: (0,0)
      width: scale*10
      height: scale*10
      fillColor: #1b1f8a11
      strokeWidth: 2.0
   }

   shape xAxis = Line {
      start: (-scale*5.,0)
      end: (scale*5.,0)
      strokeColor: #888888ff
      strokeWidth: 0.65
      endArrowhead: "straight"
      endArrowheadSize: 0.5
   }

   shape yAxis = Line {
      start: (0,-scale*5.)
      end: (0,scale*5.)
      strokeColor: #888888ff
      strokeWidth: 0.65
      endArrowhead: "straight"
      endArrowheadSize: 0.5
   }

   scalar gridStrokeWidth = 0.35
   color gridStrokeColor = #88888844

   shape gridX1 = Line {
      start: scale*(1.,-5.)
      end: scale*(1.,5.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridX2 = Line {
      start: scale*(2.,-5.)
      end: scale*(2.,5.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridX3 = Line {
      start: scale*(3.,-5.)
      end: scale*(3.,5.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridX4 = Line {
      start: scale*(4.,-5.)
      end: scale*(4.,5.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridXn4 = Line {
      start: scale*(-4.,-5.)
      end: scale*(-4.,5.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridXn3 = Line {
      start: scale*(-3.,-5.)
      end: scale*(-3.,5.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridXn2 = Line {
      start: scale*(-2.,-5.)
      end: scale*(-2.,5.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridXn1 = Line {
      start: scale*(-1.,-5.)
      end: scale*(-1.,5.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }

   shape gridY1 = Line {
      start: scale*(-5.,1.)
      end: scale*(5.,1.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridY2 = Line {
      start: scale*(-5.,2.)
      end: scale*(5.,2.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridY3 = Line {
      start: scale*(-5.,3.)
      end: scale*(5.,3.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridY4 = Line {
      start: scale*(-5.,4.)
      end: scale*(5.,4.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridYn4 = Line {
      start: scale*(-5.,-4.)
      end: scale*(5.,-4.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridYn3 = Line {
      start: scale*(-5.,-3.)
      end: scale*(5.,-3.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridYn2 = Line {
      start: scale*(-5.,-2.)
      end: scale*(5.,-2.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridYn1 = Line {
      start: scale*(-5.,-1.)
      end: scale*(5.,-1.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
}

forall Matrix M {

   vec2 M.r1 = (1,0)
   vec2 M.r2 = (0,1)

   mat2x2 M.mat = ( M.r1, M.r2 )

   shape M.row1 = Line {
      start: (0,0)
      end: global.scale * M.r1
      strokeColor: #aaaaaaff
      strokeWidth: 1.0
      endArrowhead: "straight"
      endArrowheadSize: 0.5
   }

   shape M.row2 = Line {
      start: (0,0)
      end: global.scale * M.r2
      strokeColor: #aaaaaaff
      strokeWidth: 1.0
      endArrowhead: "straight"
      endArrowheadSize: 0.5
   }

   shape M.r1Label = Equation {
      center: global.scale*M.r1 + unit(M.r1)*7
      string: M.label + "_1"
      fillColor: M.row1.strokeColor
      fontSize: global.labelSize
   }

   shape M.r2Label = Equation {
      center: global.scale*M.r2 + unit(M.r2)*7
      string: M.label + "_2"
      fillColor: M.row2.strokeColor
      fontSize: global.labelSize
   }
}

forall Vector v {

   scalar θ = ?
   vec2 v.vec = (cos(θ),sin(θ))

   shape v.icon = Line {
      start: (0,0)
      end: global.scale * v.vec
      strokeColor: #aaaaaaff
      strokeWidth: 1.0
      endArrowhead: "straight"
      endArrowheadSize: 0.75
   }

   shape v.labelText = Equation {
      center: global.scale*v.vec + unit(v.vec)*7
      string: v.label
      fillColor: v.icon.strokeColor
      fontSize: global.labelSize
   }
}

forall Matrix A; Matrix B
where B := transpose(A)
{
   B.mat = A.mat'
   B.r1 = (B.mat[0][0],B.mat[0][1])
   B.r2 = (B.mat[1][0],B.mat[1][1])
}

forall Matrix A; Vector u; Vector v
where v := mvmul( A, u )
{
   v.vec = A.mat * u.vec
   v.icon.strokeColor = #0000aaff
}

forall Matrix A; Vector u; Vector v
where v := vmmul( u, A )
{
   v.vec = u.vec * A.mat
   v.icon.strokeColor = #ff6600ff
}

forall Scalar c
{
   c.val = ?

   shape c.icon = Circle {
      center: (0,0)
      r: global.scale * c.val
      fillColor : #ff660033
   }

   shape c.labelText = Equation {
      center: (global.scale*c.val + 5)*sqrt(2.)*(1,1)/2.
      string: c.label
      fillColor : c.icon.fillColor
      fontSize: global.labelSize
   }
}

forall Scalar c; Matrix A; Matrix B
where B := smmul( c, A )
{
   override B.mat = c.val * A.mat
   override B.r1 = c.val * A.r1
   override B.r2 = c.val * A.r2
}

forall Scalar c; Matrix A; Matrix B
where B := smmulr( A, c )
{
   override B.mat = A.mat * c.val
   override B.r1 = A.r1 * c.val 
   override B.r2 = A.r2 * c.val 
}

forall Scalar c; Matrix A; Matrix B
where B := msdiv( A, c )
{
   override B.mat = A.mat / c.val
   override B.r1 = A.r1 / c.val
   override B.r2 = A.r2 / c.val
}

forall Matrix A; Vector u; Vector v
where A := outer( u, v )
{
   override A.mat = outerProduct( u.vec, v.vec )
   override A.r1 = ( A.mat[0][0], A.mat[0][1] )
   override A.r2 = ( A.mat[1][0], A.mat[1][1] )
}

forall Vector u; Vector v; Vector w
where w := vadd(u,v)
{
   override w.vec = u.vec + v.vec
   layer u.icon above w.icon
   layer v.icon above w.icon
}

forall Vector u; Vector v; Vector w
where w := vsub(u,v)
{
   override w.vec = u.vec - v.vec
   layer u.icon above w.icon
   layer v.icon above w.icon
}

forall Vector u; Vector v; Vector w
where w := ewvvmul(u,v)
{
   override w.vec = u.vec .* v.vec
   layer u.icon above w.icon
   layer v.icon above w.icon
}

forall Vector u; Vector v; Vector w
where w := ewvvdiv(u,v)
{
   override w.vec = u.vec ./ v.vec
   layer u.icon above w.icon
   layer v.icon above w.icon
}

forall Scalar c; Vector u; Vector v
where v := lvmul(c,u)
{
   override v.vec = c.val * u.vec
   layer u.icon above v.icon
}

forall Scalar c; Vector u; Vector v
where v := rvmul(u,c)
{
   override v.vec = u.vec * c.val
   layer u.icon above v.icon
}

forall Scalar c; Vector u; Vector v
where v := vdiv(u,c)
{
   override v.vec = u.vec / c.val
   layer u.icon above v.icon
}

forall Matrix A; Matrix B; Matrix C
where C := mmadd(A,B)
{
   override C.mat = A.mat + B.mat
   override C.r1 = ( C.mat[0][0], C.mat[0][1] )
   override C.r2 = ( C.mat[1][0], C.mat[1][1] )

   override C.label = "(" + A.label + "+" + B.label + ")"
}

forall Matrix A; Matrix B; Matrix C
where C := mmsub(A,B)
{
   override C.mat = A.mat - B.mat
   override C.r1 = ( C.mat[0][0], C.mat[0][1] )
   override C.r2 = ( C.mat[1][0], C.mat[1][1] )

   override C.label = "(" + A.label + "-" + B.label + ")"
}

forall Matrix A; Matrix B; Matrix C
where C := mmmul(A,B)
{
   override C.mat = A.mat * B.mat
   override C.r1 = ( C.mat[0][0], C.mat[0][1] )
   override C.r2 = ( C.mat[1][0], C.mat[1][1] )

   override C.label = "(" + A.label + B.label + ")"
}

forall Matrix A; Matrix B; Matrix C
where C := ewmmmul(A,B)
{
   override C.mat = A.mat .* B.mat
   override C.r1 = ( C.mat[0][0], C.mat[0][1] )
   override C.r2 = ( C.mat[1][0], C.mat[1][1] )

   override C.label = "(" + A.label + "\\odot " + B.label + ")"
}

forall Matrix A; Matrix B; Matrix C
where C := ewmmdiv(A,B)
{
   override C.mat = A.mat ./ B.mat
   override C.r1 = ( C.mat[0][0], C.mat[0][1] )
   override C.r2 = ( C.mat[1][0], C.mat[1][1] )

   override C.label = "(" + A.label + "\\oslash " + B.label + ")"
}

---- set constants (for debugging)

forall Scalar \`c\` {
   override \`c\`.val = 2
}

forall Vector \`u\` {
   override \`u\`.vec = (-3,2)
   override \`u\`.icon.strokeColor = #00aa00ff
}

forall Vector \`v\` {
   override \`v\`.vec = (2,1)
   override \`v\`.icon.strokeColor = #0000aaff
}

forall Matrix \`A\` {
   override \`A\`.mat = ( (-1,1), (0,-1) )
   override \`A\`.r1 = ( \`A\`.mat[0][0], \`A\`.mat[0][1] )
   override \`A\`.r2 = ( \`A\`.mat[1][0], \`A\`.mat[1][1] )
   override \`A\`.row1.strokeColor = #ff0000ff
   override \`A\`.row2.strokeColor = #ff0000ff
}

forall Matrix \`B\` {
   override \`B\`.mat = ( (4,2), (1,4) )
   override \`B\`.r1 = ( \`B\`.mat[0][0], \`B\`.mat[0][1] )
   override \`B\`.r2 = ( \`B\`.mat[1][0], \`B\`.mat[1][1] )
   override \`B\`.row1.strokeColor = #00aa00ff
   override \`B\`.row2.strokeColor = #00aa00ff
}

forall Matrix \`AT\` {
   override \`AT\`.r1 = ( \`AT\`.mat[0][0], \`AT\`.mat[0][1] )
   override \`AT\`.r2 = ( \`AT\`.mat[1][0], \`AT\`.mat[1][1] )
   override \`AT\`.row1.strokeColor = #ff000077
   override \`AT\`.row2.strokeColor = #ff000077
}

forall Matrix \`X\` {
   override \`X\`.mat = ( (-1,2), (0,-2) )
   override \`X\`.r1 = ( \`X\`.mat[0][0], \`X\`.mat[0][1] )
   override \`X\`.r2 = ( \`X\`.mat[1][0], \`X\`.mat[1][1] )
   override \`X\`.row1.strokeColor = #33dd00ff
   override \`X\`.row2.strokeColor = #33dd00ff
}

forall Matrix \`Y\` {
   override \`Y\`.mat = ( (2,1), (1,2) )
   override \`Y\`.r1 = ( \`Y\`.mat[0][0], \`Y\`.mat[0][1] )
   override \`Y\`.r2 = ( \`Y\`.mat[1][0], \`Y\`.mat[1][1] )
   override \`Y\`.row1.strokeColor = #4400ccff
   override \`Y\`.row2.strokeColor = #4400ccff
}

`,a=`type Scalar
type Vector
type Matrix

function transpose( Matrix A ) -> Matrix
function smmul( Scalar c, Matrix A ) -> Matrix
function smmulr( Matrix A, Scalar c ) -> Matrix
function msdiv( Matrix A, Scalar c ) -> Matrix
function mvmul( Matrix A, Vector v ) -> Vector
function vmmul( Vector v, Matrix A ) -> Vector
function outer( Vector u, Vector v ) -> Matrix
function vadd( Vector u, Vector v ) -> Vector
function vsub( Vector u, Vector v ) -> Vector
function lvmul( Scalar c, Vector u ) -> Vector
function rvmul( Vector u, Scalar c ) -> Vector
function vdiv( Vector u, Scalar c ) -> Vector
function mmmul( Matrix A, Matrix B ) -> Matrix
function mmadd( Matrix A, Matrix B ) -> Matrix
function mmsub( Matrix A, Matrix B ) -> Matrix
function ewvvmul( Vector u, Vector v ) -> Vector
function ewvvdiv( Vector u, Vector v ) -> Vector
function ewmmmul( Matrix A, Matrix B ) -> Matrix
function ewmmdiv( Matrix A, Matrix B ) -> Matrix

`;export{a as d,e as r,o as s};
//# sourceMappingURL=matrix-ops.domain-8a3df65a.js.map
