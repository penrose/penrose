type Center
type Shape

type Point
type Line
type Circle
type Triangle

Point <: Shape
Line <: Shape
Circle <: Shape
Triangle <: Shape

constructor Inversion( Shape original, Center center ) -> Shape