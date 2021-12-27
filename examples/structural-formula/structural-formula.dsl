type Molecule

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

predicate SingleBonded: Molecule m1 * Molecule m2
predicate DoubleBonded: Molecule m1 * Molecule m2

