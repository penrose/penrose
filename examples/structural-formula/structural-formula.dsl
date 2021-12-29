type Molecule

type FunctionalGroup
FunctionalGroup <: Molecule

type Atom
Atom <: Molecule

type Hydrogen
type Carbon
type Nitrogen
type Oxygen
Hydrogen <: Atom
  Carbon <: Atom
Nitrogen <: Atom
  Oxygen <: Atom

predicate SingleBond: Molecule m1 * Molecule m2
predicate DoubleBond: Molecule m1 * Molecule m2

