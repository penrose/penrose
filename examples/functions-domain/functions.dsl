type Set
type Point
type Map

-- NOTE: we are not porting all the predicates on Intervals and Functions into this domain, as we are only drawing continuous functions and bounded, connected intervals.
type Interval
type Function
type Reals
type Real
Reals <: Set
Interval <: Set
Reals <: Interval
Function <: Map

constructor CreateInterval: Real left * Real right -> Interval
constructor CreateFunction: Set domain * Set codomain -> Function

predicate Not: Prop p1 -- higher-order predicate 
predicate From : Map f * Set domain * Set codomain
predicate In : Point p * Set s
predicate Injection : Map m
predicate Surjection : Map m
predicate Bijection : Map m
