type Point

type ReverseL
type Shape
type Star
type Line
type Polyline
type Text
type Ellipse

Polyline <: Shape
ReverseL <: Shape
Star <: Shape
Line <: Shape
Text <: Shape
Ellipse <: Shape

predicate Around(Point p, Shape s)