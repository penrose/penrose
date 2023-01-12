-- Atoms

type Atom

type Carbon <: Atom
type Hydrogen <: Atom
type Oxygen <: Atom
type Nitrogen <: Atom

-- Bonds

type Bond

type SingleBond <: Bond
type DoubleBond <: Bond
type TripleBond <: Bond

constructor MakeSingleBond(Atom a, Atom b) -> SingleBond
constructor MakeDoubleBond(Atom a, Atom b) -> DoubleBond
constructor MakeTripleBond(Atom a, Atom b) -> TripleBond
