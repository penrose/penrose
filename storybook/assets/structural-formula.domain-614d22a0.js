const e=`-- structural-formula.domain
--
-- This Penrose Domain schema is used to encode molecular
-- structures in a format suitable for drawing a variety
-- of different kinds of molecular structure diagrams.

-- a Node is any collection of atoms that is treated
-- as a single logical unit, such as a functional
-- group (or just a single atom)
type Node

-- a FunctionalGroup represents a collection of atoms
-- in the same molecule (such as an alcohol or ester)
type FunctionalGroup <: Node

-- an Atom represents a single atom within a larger
-- molecule (or as an isolated ion)
type Atom <: Node

-- specific types of atoms (more could be added here)
type Hydrogen <: Atom
type   Carbon <: Atom
type Nitrogen <: Atom
type   Oxygen <: Atom
type   Sodium <: Atom
type Chlorine <: Atom

-- predicates used to specify bonds between Nodes
predicate SingleBond(Node n1, Node n2)
predicate DoubleBond(Node n1, Node n2)
predicate  IonicBond(Node n1, Node n2)

-- a Molecule is a collection of Atoms, or more generally,
-- Nodes, held together by bonds.  It is not essential to
-- annotate which collections of Nodes are connected, but
-- grouping Nodes is helpful for, e.g., labeling molecules
-- and/or grouping reactants/products.
type Molecule

predicate Contains(Molecule m, Node n)

-- these predicates are used to delineate reactants and
-- produces in a chemical equation
predicate IsReactant(Molecule m)
predicate IsProduct(Molecule m)

-- a reaction involving all reactants and products
type Reaction

-- predicates to mark the type of reaction
predicate     IsNetForward(Reaction r)
predicate IsStoichiometric(Reaction r)
predicate    IsEquilibrium(Reaction r)
predicate  IsBidirectional(Reaction r)

-- used just to supply a label for the overall diagram
type Title

`;export{e as d};
//# sourceMappingURL=structural-formula.domain-614d22a0.js.map
