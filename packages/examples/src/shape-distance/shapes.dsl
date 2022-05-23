type ReverseL
type Shape
type Point
type Star
type Line
type Polyline
type Text

Polyline <: Shape
ReverseL <: Shape
Star <: Shape
Line <: Shape
Text <: Shape

predicate Around(Point p, Shape s)