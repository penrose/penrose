import euclideanStyle from "@penrose/examples/dist/geometry-domain/euclidean.style.js";
import geometryDomain from "@penrose/examples/dist/geometry-domain/geometry.domain.js";
import c02p01 from "@penrose/examples/dist/geometry-domain/textbook_problems/c02p01.substance.js";
import simpleDirectedGraphStyle from "@penrose/examples/dist/graph-domain/simple-directed-graph.style.js";
import sec5ex21 from "@penrose/examples/dist/graph-domain/textbook/sec5/ex21.substance.js";
import lewisStyle from "@penrose/examples/dist/molecules/lewis.style.js";
import methane from "@penrose/examples/dist/molecules/methane.substance.js";

export type LLMPrompt = {
  systemInst: string;
  penroseContext: string;
  domain: string;
  bnf?: string;
  sampleSubstance?: {
    prog: string;
    name: string;
  };
  descriptionPrelude: string;
  description: string;
  finalInst: string;
  prompt: string; // full prompt
  style: string;
};

export type LLMPromptCollection = {
  [s: string]: LLMPrompt;
};

export type LLMResult =
  | {
      tag: "Ok";
      prompt: LLMPrompt;
      date: Date;
      inferenceTime: number;
      substance: string;
    }
  | {
      tag: "Error";
      prompt: LLMPrompt;
      date: Date;
      inferenceTime: number;
      APIError: string;
    };

export const systemInsts = [
  `You are a code generator that is generating a new program in the Substance programming language, which draws from the Domain programming language program also given below. To write comments, begin with \`--\`. Return only the Substance program; do not explain your reasoning whatsoever.

`,
  `You are a code generator that is generating a new program in the Substance programming language, whose BNF grammar is given below. To write comments, begin with \`--\`. Return only the Substance program; do not explain your reasoning whatsoever.

`,
];

export const penroseContexts = [
  `We have been working on a platform called Penrose for authoring mathematical diagrams. The system involves a family of 3 domain specific languages: Substance (for specifying the mathematical objects and the relationships between those objects, Style (for mapping the mathematical objects to shapes and mathematical relationships to layout constraints and objectives), and Domain (for specifying the types of mathematical objects and relationships; this is a meta-language or schema language). Those three programs are used to synthesize a layout problem which we then solve to create a corresponding diagram.

`,
];

export const grammars = {
  moleculesGrammar: `id ::= letter (letter | digit)*
substance ::= stmt*
stmt      ::= decl | predicate | function
decl      ::= tname id
predicate ::= pname "(" id ")"
function  ::= tname id ":=" fname "(" id "," id ")"
tname     ::= "Hydrogen" // This type describes a hydrogen atom. Example usage which holds for all following elements: \n\t\t'Hydrogen h1\n\t\tHydrogen h2, h3'
            | "Helium" | "Lithium" | "Beryllium" | "Boron" | "Carbon" | "Nitrogen" | "Oxygen" | "Fluorine" | "Neon" | "Sodium" | "Magnesium" | "Aluminium" | "Silicon" | "Phosphorus" | "Sulfur" | "Chlorine" | "Argon" | "Potassium" | "Calcium" | "Scandium" | "Titanium" | "Vanadium" | "Chromium" | "Manganese" | "Iron" | "Cobalt" | "Nickel" | "Copper" | "Zinc" | "Gallium" | "Germanium" | "Arsenic" | "Selenium" | "Bromine" | "Krypton" | "Rubidium" | "Strontium" | "Yttrium" | "Zirconium" | "Niobium" | "Molybdenum" | "Technetium" | "Ruthenium" | "Rhodium" | "Palladium" | "Silver" | "Cadmium" | "Indium" | "Tin" | "Antimony" | "Tellurium" | "Iodine" | "Xenon" | "Cesium" | "Barium" | "Lanthanum" | "Cerium" | "Praseodymium" | "Neodymium" | "Promethium" | "Samarium" | "Europium" | "Gadolinium" | "Terbium" | "Dysprosium" | "Holmium" | "Erbium" | "Thulium" | "Ytterbium" | "Lutetium" | "Hafnium" | "Tantalum" | "Tungsten" | "Rhenium" | "Osmium" | "Iridium" | "Platinum" | "Gold" | "Mercury" | "Thallium" | "Lead" | "Bismuth" | "Polonium" | "Astatine" | "Radon" | "Francium" | "Radium" | "Actinium" | "Thorium" | "Protactinium" | "Uranium" | "Neptunium" | "Plutonium" | "Americium" | "Curium" | "Berkelium" | "Californium" | "Einsteinium" | "Fermium" | "Mendelevium" | "Nobelium" | "Lawrencium" | "Rutherfordium" | "Dubnium" | "Seaborgium" | "Bohrium" | "Hassium" | "Meitnerium" | "Darmstadtium" | "Roentgenium" | "Copernicium"
            | "Bond" // This type describes a bond. See construction of a bond below.
fname     ::= "MakeSingleBond" // This function creates a single bond between two atoms. Example usage: \n\t\t'Hydrogen h1, h2\n\t\tBond b1\n\t\tb1 := MakeSingleBond(h1, h2)'
            | "MakeDoubleBond" // This function creates a double bond between two atoms. Example usage: \n\t\t'Oxygen o1, o2\n\t\tBond b1\n\t\tb1 := MakeDoubleBond(o1, o2)\n\t\tFourDots(o1)\n\t\tFourDots(o2)'
            | "MakeTripleBond" // This function creates a triple bond between two atoms. Example usage: \n\t\t'Nitrogen n1, n2\n\t\tBond b1\n\t\tb1 := MakeTripleBond(n1, n2)\n\t\tTwoDots(n1)\n\t\tTwoDots(n2)'
pname     ::= "ZeroDots" // This predicate displays no dots on an atom. Example usage: \n\t\t'Hydrogen h1, h2\n\t\tBond b1 := MakeSingleBond(h1, h2)\n\t\tZeroDots(h1)\n\t\tZeroDots(h2)'
            | "TwoDots" // This predicate displays two dots on an atom. Example usage: \n\t\t'Nitrogen n1, n2\n\t\tBond b1 := MakeTripleBond(n1, n2)\n\t\tTwoDots(n1)\n\t\tTwoDots(n2)'
            | "FourDots" // This predicate displays four dots on an atom. Example usage: \n\t\t'Oxygen o1, o2\n\t\tBond b1 := MakeDoubleBond(o1, o2)\n\t\tFourDots(o1)\n\t\tFourDots(o2)'
            | "SixDots" // This predicate displays six dots on an atom. Example usage: \n\t\t'Fluorine f1, f2\n\t\tBond b1 := MakeSingleBond(f1, f2)\n\t\tSixDots(f1)\n\t\tSixDots(f2)'
letter    ::= "A" | "B" | ... | "Z" | "a" | "b" | ... | "z"
digit     ::= "0" | "1" | ... | "9"
`,
  simpleDirectedGraphGrammar: `id ::= letter (letter | digit)*
substance ::= stmt*
stmt      ::= decl | predicate | ctrl
decl      ::= tname id
predicate ::= pname "(" id "," id ")"
ctrl      ::= "AutoLabel All" 
tname     ::= "Vertex" // This type describes a vertex. Example usage: 'Vertex v1, v2, v3'
pname     ::= "Arc" // This predicate creates an arc from one vertex to another. Example usage: \n\t\t'Vertex v1, v2, v3\n\t\tArc(v1, v2)\n\t\tArc(v2, v3)\n\t\tAutoLabel All'
            | "HighlightVertex" // This predicate highlights a vertex. Example usage: \n\t\t'Vertex v1, v2, v3\n\t\tArc(v1, v2)\n\t\tArc(v2, v3)\n\t\tHighlightVertex(v1)\n\t\tAutoLabel All'
            | "HighlightArc" // This predicate highlights an arc given the outgoing and incoming vertices of the arc. Example usage: \n\t\t'Vertex v1, v2, v3\n\t\tArc(v1, v2)\n\t\tArc(v2, v3)\n\t\tHighlightArc(v1, v2)\n\t\tAutoLabel All'
letter    ::= "A" | "B" | ... | "Z" | "a" | "b" | ... | "z"
digit     ::= "0" | "1" | ... | "9"
`,

  geometryGrammar: `id ::= letter (letter | digit)*
substance ::= stmt*
stmt      ::= decl | func1 | func2 | func3 | func4 | pred1 | pred2 | pred3 | ctrl
decl      ::= tname id
func1     ::= tname id ":=" f1name "(" id ")"
func2     ::= tname id ":=" f2name "(" id "," id ")"
func3     ::= tname id ":=" f3name "(" id "," id "," id ")"
func4     ::= tname id ":=" f4name "(" id "," id "," id "," id ")"
pred1     ::= p1name "(" id ")"
pred2     ::= p2name "(" id "," id ")"
pred3     ::= p3name "(" id "," id "," id ")"
ctrl      ::= "AutoLabel " id ("," id)* // This control statement automatically labels the given point(s) or plane(s). Example usage: 'Point A, B, C\n\t\tAutoLabel A, B, C'
tname     ::= "Point" // This type describes a point. Example usage: \n\t\t'Point A, B, C\n\t\tAutoLabel A, B, C'
            | "Plane" // This type describes a plane. Example usage: \n\t\t'Plane p\n\t\tAutoLabel p'
            | "Ray" // This type describes a ray. See construction of a ray below.
            | "Line" // This type describes a line. See construction of a line below.
            | "Segment" // This type describes a line segment. See construction of a line segment below.
            | "Angle" // This type describes an angle. See construction of an angle below.
            | "Triangle" // This type describes a triangle. See construction of a triangle below.
            | "Quadrilateral" // This type describes a quadrilateral. See construction of a quadrilateral below.
            | "Rectangle" // This type describes a rectangle. See construction of a rectangle below.
            | "Circle" // This type describes a circle. See construction of a circle below.
f1name    ::= "Midpoint" // This function creates a point as the midpoint of a line. Example usage: \n\t\t'Point A, B\n\t\tSegment segmentAB\n\t\tsegmentAB := Segment(A, B)\n\t\tPoint midpointAB := Midpoint(AB)\n\t\tAutoLabel A, B, midpointAB'
            | "Bisector" // This function creates a ray as the angle bisector of an angle. Example usage: \n\t\t'Point A, B, C\n\t\tAngle angleABC\n\t\tangleABC := InteriorAngle(A, B, C)\n\t\tRay bisectorABC := Bisector(angleABC)\n\t\tAutoLabel A, B, C, bisectorABC'
f2name    ::= "Segment" // This function creates a line segment from two points. Example usage: \n\t\t'Point A, B\n\t\tSegment AB := Segment(A, B)\n\t\tAutoLabel A, B'
            | "Ray" // This function creates a ray from two points, a base and a direction point. Example usage: \n\t\t'Point A, B\n\t\tRay rayAB := Ray(A, B)\n\t\tAutoLabel A, B'
            | "Line" // This function creates a line from two points. Example usage: \n\t\t'Point A, B\n\t\tLine lineAB := Line(A, B)\n\t\tAutoLabel A, B'
            | "CircleR" // This function creates a circle from a center point and a radius point. Example usage: \n\t\t'Point A, B\n\t\tCircle circleAB := CircleR(A, B)\n\t\tAutoLabel A, B'
            | "PerpendicularBisector" // This function creates a perpendicular bisector from a line segment. Example usage: \n\t\t'Point A, B, C\n\t\tSegment AB\n\t\tAB := Segment(A, B)\n\t\tSegment perpendicularBisectorAB := PerpendicularBisector(AB, C)\n\t\tAutoLabel A, B, C'
            | "Radius" // This function creates a radius from a circle and a point on the circle. Example usage: \n\t\t'Point A, B\n\t\tCircle circleAB\n\t\tcircleAB := CircleR(A, B)\n\t\tSegment radiusAB := Radius(circleAB, B)\n\t\tAutoLabel A, B'
f3name    ::= "InteriorAngle" // This function creates an angle from three points. Example usage: \n\t\t'Point A, B, C\n\t\tAngle angleABC := InteriorAngle(A, B, C)\n\t\tAutoLabel A, B, C'
            | "Triangle" // This function creates a triangle from three points. Example usage: 'Point A, B, C\n\t\tTriangle triangleABC := Triangle(A, B, C)\n\t\tAutoLabel A, B, C'
            | "PerpendicularBisectorLabelPts" // This function creates a perpendicular bisector from a segment to bisect, a base point, and a direction point. Example usage: \n\t\t'Point A, B, C, D, d\n\t\tSegment AB, CD\n\t\tAB := Segment(A, B)\n\t\tCD := Segment(C, D)\n\t\tPerpendicularBisectorLabelPts(AB, C, d)\n\t\tAutoLabel A, B, C, D, d'
            | "MidSegment" // This function creates a midsegment from a triangle and two points on the triangle. Example usage: \n\t\t'Point A, B, C, D, E\n\t\tTriangle triangleABC := Triangle(A, B, C)\n\t\tSegment midsegmentDE := MidSegment(triangleABC, D, E)\n\t\tAutoLabel A, B, C, D, E'
            | "Chord" // This function creates a chord from a circle and two points on the circle. Example usage: \n\t\t'Point A, B, C\n\t\tCircle circleAB\n\t\tcircleAB := CircleR(A, B)\n\t\tSegment chordAC := Chord(circleAB, A, C)\n\t\tAutoLabel A, B, C'
            | "Diameter" // This function creates a diameter from a circle and two points. Example usage: \n\t\t'Point A, B\n\t\tCircle circleAB\n\t\t\n\t\tSegment diameterAC := Diameter(circleAB, A, B)\n\t\tAutoLabel A, B'
f4name    ::= "Rectangle" // This function creates a rectangle from four points. Example usage: \n\t\t'Point A, B, C, D\n\t\tRectangle rectangleABCD := Rectangle(A, B, C, D)\n\t\tAutoLabel A, B, C, D'
            | "Quadrilateral" // This function creates a quadrilateral from four points. Example usage: \n\t\t'Point A, B, C, D\n\t\tQuadrilateral quadrilateralABCD := Quadrilateral(A, B, C, D)\n\t\tAutoLabel A, B, C, D'
p1name    ::= "Acute" // This predicate makes an angle acute. Example use: 'Point A, B, C\n\t\tAngle angleABC\n\t\tangleABC := InteriorAngle(A, B, C)\n\t\tAcute(angleABC)\n\t\tAutoLabel A, B, C'
            | "Obtuse" // This predicate makes an angle obtuse. Example use: 'Point A, B, C\n\t\tAngle angleABC\n\t\tangleABC := InteriorAngle(A, B, C)\n\t\tObtuse(angleABC)\n\t\tAutoLabel A, B, C'
            | "RightMarked" // This predicate makes an angle right and marks it with a square. Example use: 'Point A, B, C\n\t\tAngle angleABC\n\t\tangleABC := InteriorAngle(A, B, C)\n\t\tRightMarked(angleABC)\n\t\tAutoLabel A, B, C'
            | "RightUnmarked" // This predicate makes an angle right and does not mark it with a square. Example use: 'Point A, B, C\n\t\tAngle angleABC\n\t\tangleABC := InteriorAngle(A, B, C)\n\t\tRightUnmarked(angleABC)\n\t\tAutoLabel A, B, C'
            | "Parallelogram" // This predicate makes a quadrilateral a parallelogram. Example use: 'Point A, B, C, D\n\t\tQuadrilateral quadrilateralABCD\n\t\tquadrilateralABCD := Quadrilateral(A, B, C, D)\n\t\tParallelogram(quadrilateralABCD)\n\t\tAutoLabel A, B, C, D'
p2name    ::= "On" // This predicate makes a point be on a line. Example use: 'Point A, B\n\t\tLine lineAB\n\t\tlineAB := Line(A, B)\n\t\tOn(A, lineAB)\n\t\tAutoLabel A, B'
            | "In" // This predicate makes a point be in a plane. Example use: 'Point A, B\n\t\tPlane planeAB\n\t\tplaneAB := Plane(A, B)\n\t\tIn(A, planeAB)\n\t\tAutoLabel A, B'
            | "Midpoint" // This predicate makes a point be the midpoint of a line. Example use: 'Point A, B, C\n\t\tSegment segmentAB\n\t\tsegmentAB := Segment(A, B)\n\t\tMidpoint(segmentAB, C)\n\t\tAutoLabel A, B, C'
            | "EqualLength" // This predicate makes two segments have equal length. Example use: 'Point A, B, C, D\n\t\tSegment segmentAB, segmentCD\n\t\tsegmentAB := Segment(A, B)\n\t\tsegmentCD := Segment(C, D)\n\t\tEqualLength(segmentAB, segmentCD)\n\t\tAutoLabel A, B, C, D'
            | "EqualLengthMarker" // This predicate only marks two segments with a tick indicating that they have equal length. Only use if 'EqualLength' precedes it. Example use: 'Point A, B, C, D\n\t\tSegment segmentAB, segmentCD\n\t\tsegmentAB := Segment(A, B)\n\t\tsegmentCD := Segment(C, D)\n\t\tEqualLength(segmentAB, segmentCD)\n\t\tEqualLengthMarker(segmentAB, segmentCD)\n\t\tAutoLabel A, B, C, D'
            | "Parallel" // This predicate makes two lines parallel. Example use: 'Point A, B, C, D\n\t\tLine lineAB, lineCD\n\t\tlineAB := Line(A, B)\n\t\tlineCD := Line(C, D)\n\t\tParallel(lineAB, lineCD)\n\t\tAutoLabel A, B, C, D'
            | "ParallelMarker1" // This predicate marks two lines parallel. Only use if 'Parallel' precedes it. Example use: 'Point A, B, C, D\n\t\tLine lineAB, lineCD\n\t\tlineAB := Line(A, B)\n\t\tlineCD := Line(C, D)\n\t\tParallel(lineAB, lineCD)\n\t\tParallelMarker1(lineAB, lineCD)\n\t\tAutoLabel A, B, C, D'
            | "AngleBisector" // This predicate makes a ray be the angle bisector of an angle. Example use: 'Point A, B, C\n\t\tAngle angleABC\n\t\tangleABC := InteriorAngle(A, B, C)\n\t\tRay rayABC\n\t\trayABC := Ray(A, B)\n\t\tAngleBisector(angleABC, rayABC)\n\t\tAutoLabel A, B, C'
            | "EqualAngle" // This predicate makes two angles have equal measure. Example use: 'Point A, B, C, D, E, F\n\t\tAngle angleABC, angleDEF\n\t\tangleABC := InteriorAngle(A, B, C)\n\t\tangleDEF := InteriorAngle(D, E, F)\n\t\tEqualAngle(angleABC, angleDEF)\n\t\tAutoLabel A, B, C, D, E, F'
            | "EqualAngleMarker" // This predicate only marks two angles with a tick indicating that they have equal measure. Only use if 'EqualAngle' precedes it. Example use: 'Point A, B, C, D, E, F\n\t\tAngle angleABC, angleDEF\n\t\tangleABC := InteriorAngle(A, B, C)\n\t\tangleDEF := InteriorAngle(D, E, F)\n\t\tEqualAngle(angleABC, angleDEF)\n\t\tEqualAngleMarker(angleABC, angleDEF)\n\t\tAutoLabel A, B, C, D, E, F'
            | "OnCircle" // This predicate makes a point be on a circle. Example use: 'Point A, B, C\n\t\tCircle circleAB\n\t\tcircleAB := CircleR(A, B)\n\t\tOnCircle(circleAB, C)\n\t\tAutoLabel A, B, C'
            | "CircleCenter" // Do not use.
            | "Incenter" // This predicate makes a point be the incenter of a triangle. Example use: 'Point A, B, C, D\n\t\tTriangle triangleABC\n\t\ttriangleABC := Triangle(A, B, C)\n\t\tIncenter(D, triangleABC)\n\t\tAutoLabel A, B, C, D'
            | "Orthocenter" // This predicate makes a point be the orthocenter of a triangle. Example use: 'Point A, B, C, D\n\t\tTriangle triangleABC\n\t\ttriangleABC := Triangle(A, B, C)\n\t\tOrthocenter(D, triangleABC)\n\t\tAutoLabel A, B, C, D'
            | "Centroid" // This predicate makes a point be the centroid of a triangle. Example use: 'Point A, B, C, D\n\t\tTriangle triangleABC\n\t\ttriangleABC := Triangle(A, B, C)\n\t\tCentroid(D, triangleABC)\n\t\tAutoLabel A, B, C, D'
            | "Circumcenter" // This predicate makes a point be the circumcenter of a triangle. Example use: 'Point A, B, C, D\n\t\tTriangle triangleABC\n\t\ttriangleABC := Triangle(A, B, C)\n\t\tCircumcenter(D, triangleABC)\n\t\tAutoLabel A, B, C, D'
p3name    ::= "Collinear" // This predicate makes three points collinear. Example use: 'Point A, B, C\n Segment AB, BC\n AB := Segment(A, B)\n BC := Segment(B, C)\n Collinear(A, B, C)'
letter    ::= "A" | "B" | ... | "Z" | "a" | "b" | ... | "z"
digit     ::= "0" | "1" | ... | "9"
`,
};

export const commentedDomains = {
  moleculesDomain: `-- Atoms

type Atom

type Hydrogen <: Atom -- This type describes a hydrogen atom. Example usage which holds for all following elements: 
-- Hydrogen h1
-- Hydrogen h2, h3
type Helium <: Atom
type Lithium <: Atom
type Beryllium <: Atom
type Boron <: Atom
type Carbon <: Atom
type Nitrogen <: Atom
type Oxygen <: Atom
type Fluorine <: Atom
type Neon <: Atom
type Sodium <: Atom
type Magnesium <: Atom
type Aluminium <: Atom
type Silicon <: Atom
type Phosphorus <: Atom
type Sulfur <: Atom
type Chlorine <: Atom
type Argon <: Atom
type Potassium <: Atom
type Calcium <: Atom
type Scandium <: Atom
type Titanium <: Atom
type Vanadium <: Atom
type Chromium <: Atom
type Manganese <: Atom
type Iron <: Atom
type Cobalt <: Atom
type Nickel <: Atom
type Copper <: Atom
type Zinc <: Atom
type Gallium <: Atom
type Germanium <: Atom
type Arsenic <: Atom
type Selenium <: Atom
type Bromine <: Atom
type Krypton <: Atom
type Rubidium <: Atom
type Strontium <: Atom
type Yttrium <: Atom
type Zirconium <: Atom
type Niobium <: Atom
type Molybdenum <: Atom
type Technetium <: Atom
type Ruthenium <: Atom
type Rhodium <: Atom
type Palladium <: Atom
type Silver <: Atom
type Cadmium <: Atom
type Indium <: Atom
type Tin <: Atom
type Antimony <: Atom
type Tellurium <: Atom
type Iodine <: Atom
type Xenon <: Atom
type Cesium <: Atom
type Barium <: Atom
type Lanthanum <: Atom
type Cerium <: Atom
type Praseodymium <: Atom
type Neodymium <: Atom
type Promethium <: Atom
type Samarium <: Atom
type Europium <: Atom
type Gadolinium <: Atom
type Terbium <: Atom
type Dysprosium <: Atom
type Holmium <: Atom
type Erbium <: Atom
type Thulium <: Atom
type Ytterbium <: Atom
type Lutetium <: Atom
type Hafnium <: Atom
type Tantalum <: Atom
type Tungsten <: Atom
type Rhenium <: Atom
type Osmium <: Atom
type Iridium <: Atom
type Platinum <: Atom
type Gold <: Atom
type Mercury <: Atom
type Thallium <: Atom
type Lead <: Atom
type Bismuth <: Atom
type Polonium <: Atom
type Astatine <: Atom
type Radon <: Atom
type Francium <: Atom
type Radium <: Atom
type Actinium <: Atom
type Thorium <: Atom
type Protactinium <: Atom
type Uranium <: Atom
type Neptunium <: Atom
type Plutonium <: Atom
type Americium <: Atom
type Curium <: Atom
type Berkelium <: Atom
type Californium <: Atom
type Einsteinium <: Atom
type Fermium <: Atom
type Mendelevium <: Atom
type Nobelium <: Atom
type Lawrencium <: Atom
type Rutherfordium <: Atom
type Dubnium <: Atom
type Seaborgium <: Atom
type Bohrium <: Atom
type Hassium <: Atom
type Meitnerium <: Atom
type Darmstadtium <: Atom
type Roentgenium <: Atom
type Copernicium <: Atom

-- Bonds

type Bond -- This type describes a bond. See construction of a bond below.

constructor MakeSingleBond(Atom a, Atom b) -> Bond -- This function creates a single bond between two atoms. Example usage: 
-- Hydrogen h1, h2
-- Bond b1
-- b1 := MakeSingleBond(h1, h2)
constructor MakeDoubleBond(Atom a, Atom b) -> Bond -- This function creates a double bond between two atoms. Example usage: 
-- Oxygen o1, o2
-- Bond b1
-- b1 := MakeDoubleBond(o1, o2)
-- FourDots(o1)
-- FourDots(o2)
constructor MakeTripleBond(Atom a, Atom b) -> Bond -- This function creates a triple bond between two atoms. Example usage: 
-- Nitrogen n1, n2
-- Bond b1
-- b1 := MakeTripleBond(n1, n2)
-- TwoDots(n1)
-- TwoDots(n2)

-- Electrons 

-- these correspond to dots in a Lewis structure
predicate ZeroDots(Atom) -- This predicate displays no dots on an atom. Example usage: 
-- Hydrogen h1, h2
-- Bond b1 := MakeSingleBond(h1, h2)
-- ZeroDots(h1)
-- ZeroDots(h2)
predicate TwoDots(Atom) -- This predicate displays two dots on an atom. Example usage: 
-- Nitrogen n1, n2
-- Bond b1 := MakeTripleBond(n1, n2)
-- TwoDots(n1)
-- TwoDots(n2)
predicate FourDots(Atom) -- This predicate displays four dots on an atom. Example usage: 
-- Oxygen o1, o2
-- Bond b1 := MakeDoubleBond(o1, o2)
-- FourDots(o1)
-- FourDots(o2)
predicate SixDots(Atom) -- This predicate displays six dots on an atom. Example usage:
-- Fluorine f1, f2
-- Bond b1 := MakeSingleBond(f1, f2)
-- SixDots(f1)
-- SixDots(f2)

-- layout
-- predicate Collinear(Atom, Atom, Atom)
-- predicate VerticalAlign(Atom, Atom)
-- predicate HorizontalAlign(Atom, Atom)
`,
  simpleDirectedGraphDomain: `
type Vertex -- This type describes a vertex. Example usage: 
-- Vertex v1, v2, v3
predicate Arc(Vertex a, Vertex b) -- This predicate creates an arc from one vertex to another. Example usage: 
-- Vertex v1, v2, v3
-- Arc(v1, v2)
-- Arc(v2, v3)
-- AutoLabel All
predicate HighlightVertex(Vertex a) -- This predicate highlights a vertex. Example usage: 
-- Vertex v1, v2, v3
-- Arc(v1, v2)
-- Arc(v2, v3)
-- HighlightVertex(v1)
-- AutoLabel All
predicate HighlightArc(Vertex a, Vertex b) -- This predicate highlights an arc given the outgoing and incoming vertices of the arc. Example usage: 
-- Vertex v1, v2, v3
-- Arc(v1, v2)
-- Arc(v2, v3)
-- HighlightArc(v1, v2)
-- AutoLabel All  
`,
  geometryDomain: `-- ~~~~~~~~~~~~~~~~ TYPES ~~~~~~~~~~~~~~~~
type Shape
type Point <: Shape -- This type describes a point. Example usage: 
-- Point A, B, C
-- AutoLabel A, B, C
type Linelike <: Shape -- This type describes a plane. Example usage: 
-- Plane p
-- AutoLabel p
type Ray <: Linelike -- This type describes a ray. See construction of a ray below.
type Line <: Linelike -- This type describes a line. See construction of a line below.
type Segment <: Linelike -- This type describes a line segment. See construction of a line segment below.

type Angle <: Shape -- This type describes an angle. See construction of an angle below.

type Triangle <: Shape -- This type describes a triangle. See construction of a triangle below.
type Quadrilateral <: Shape -- This type describes a quadrilateral. See construction of a quadrilateral below.
type Rectangle <: Quadrilateral -- This type describes a rectangle. See construction of a rectangle below.
type Circle <: Shape -- This type describes a circle. See construction of a circle below.

type Plane <: Shape -- This type describes a plane. Example usage: 
-- Plane p
-- AutoLabel p

-- ~~~~~~~~~~~~~~~~ CONSTRUCTORS ~~~~~~~~~~~~~~~~
-- Lines and Points
constructor Segment(Point p, Point q) -- This constructor creates a line segment from two points. Example usage: 
-- Point A, B
-- Segment AB := Segment(A, B)
-- AutoLabel A, B
constructor Ray(Point base, Point direction) -- This constructor creates a ray from two points, a base and a direction point. Example usage: 
-- Point A, B
-- Ray rayAB := Ray(A, B)
-- AutoLabel A, B
constructor Line(Point p, Point q) -- This constructor creates a line from two points. Example usage: 
-- Point A, B
-- Line lineAB := Line(A, B)
-- AutoLabel A, B
constructor Midpoint(Linelike l) -> Point -- This constructor creates a point as the midpoint of a line. Example usage: 
-- Point A, B
-- Segment segmentAB
-- segmentAB := Segment(A, B)
-- Point midpointAB := Midpoint(AB)
-- AutoLabel A, B, midpointAB

-- Angles
constructor InteriorAngle(Point p, Point q, Point r) -> Angle -- This constructor creates an angle from three points. Example usage: 
-- Point A, B, C
-- Angle angleABC := InteriorAngle(A, B, C)
-- AutoLabel A, B, C

-- Polygons/Shapes
constructor Triangle(Point p, Point q, Point r) -- This constructor creates a triangle from three points. Example usage: Point A, B, C
-- Triangle triangleABC := Triangle(A, B, C)
-- AutoLabel A, B, C
constructor Rectangle(Point p, Point q, Point r, Point s) -- This constructor creates a rectangle from four points. Example usage: 
-- Point A, B, C, D
-- Rectangle rectangleABCD := Rectangle(A, B, C, D)
-- AutoLabel A, B, C, D
constructor Quadrilateral(Point p, Point q, Point r, Point s) -- This function creates a quadrilateral from four points. Example usage: 
-- Point A, B, C, D
-- Quadrilateral quadrilateralABCD := Quadrilateral(A, B, C, D)
-- AutoLabel A, B, C, D
constructor CircleR(Point center, Point radius) -> Circle -- This constructor creates a circle from a center point and a radius point. Example usage: 
-- Point A, B
-- Circle circleAB := CircleR(A, B)
-- AutoLabel A, B
-- constructor CircleD(Point diam1, Point diam2) -> Circle  -- TODO can be reimplemented when #621 is resolved

-- ~~~~~~~~~~~~~~~~ FUNCTIONS ~~~~~~~~~~~~~~~~
-- Lines and Points
function Bisector(Angle) -> Ray -- This function creates a ray as the angle bisector of an angle. Example usage: 
-- Point A, B, C
-- Angle angleABC
-- angleABC := InteriorAngle(A, B, C)
-- Ray bisectorABC := Bisector(angleABC)
-- AutoLabel A, B, C, bisectorABC
function PerpendicularBisector(Segment, Point) -> Segment -- This function creates a perpendicular bisector from a line segment. Example usage: 
-- Point A, B, C
-- Segment AB
-- AB := Segment(A, B)
-- Segment perpendicularBisectorAB := PerpendicularBisector(AB, C)
-- AutoLabel A, B, C
function PerpendicularBisectorLabelPts(Segment, Point, Point) -> Segment -- This function creates a perpendicular bisector from a segment to bisect, a base point, and a direction point. Example usage: 
-- Point A, B, C, D, d
-- Segment AB, CD
-- AB := Segment(A, B)
-- CD := Segment(C, D)
-- PerpendicularBisectorLabelPts(AB, C, d)
-- AutoLabel A, B, C, D, d

-- Polygons/Shapes
function MidSegment(Triangle, Point, Point) -> Segment -- This function creates a midsegment from a triangle and two points on the triangle. Example usage: 
-- Point A, B, C, D, E
-- Triangle triangleABC := Triangle(A, B, C)
-- Segment midsegmentDE := MidSegment(triangleABC, D, E)
-- AutoLabel A, B, C, D, E
function Radius(Circle c, Point p) -> Segment -- This function creates a radius from a circle and a point on the circle. Example usage: 
-- Point A, B
-- Circle circleAB
-- circleAB := CircleR(A, B)
-- Segment radiusAB := Radius(circleAB, B)
-- AutoLabel A, B
function Chord(Circle c, Point p, Point q) -> Segment -- This function creates a chord from a circle and two points on the circle. Example usage: 
-- Point A, B, C
-- Circle circleAB
-- circleAB := CircleR(A, B)
-- Segment chordAC := Chord(circleAB, A, C)
-- AutoLabel A, B, C
function Diameter(Circle c, Point p, Point q) -> Segment -- This function creates a diameter from a circle and two points. Example usage: 
-- Point A, B
-- Circle circleAB
-- 
-- Segment diameterAC := Diameter(circleAB, A, B)
-- AutoLabel A, B

-- Unimplemented
-- function Sum(Angle, Angle) -> Angle
-- function Intersection(Linelike, Linelike) -> Point
-- function Altitude(Triangle, Angle) -> Segment
-- function Endpoint(Segment) -> Point

-- ~~~~~~~~~~~~~~~~ PREDICATES ~~~~~~~~~~~~~~~~
-- Lines and Points
predicate On(Point, Linelike) -- This predicate makes a point be on a line. Example use: Point A, B
-- Line lineAB
-- lineAB := Line(A, B)
-- On(A, lineAB)
-- AutoLabel A, B
predicate In(Point, Plane) -- This predicate makes a point be in a plane. Example use: Point A, B
-- Plane planeAB
-- planeAB := Plane(A, B)
-- In(A, planeAB)
-- AutoLabel A, B
predicate Midpoint(Linelike, Point) -- This predicate makes a point be the midpoint of a line. Example use: Point A, B, C
-- Segment segmentAB
-- segmentAB := Segment(A, B)
-- Midpoint(segmentAB, C)
-- AutoLabel A, B, C
predicate Collinear(Point, Point, Point) -- This predicate makes three points collinear. Example use: Point A, B, C
-- Segment AB, BC
-- AB := Segment(A, B)
-- BC := Segment(B, C)
-- Collinear(A, B, C)
predicate ParallelMarker1(Linelike, Linelike) -- This predicate marks two lines parallel. Only use if Parallel precedes it. Example use: Point A, B, C, D
-- Line lineAB, lineCD
-- lineAB := Line(A, B)
-- lineCD := Line(C, D)
-- Parallel(lineAB, lineCD)
-- ParallelMarker1(lineAB, lineCD)
-- AutoLabel A, B, C, D
predicate EqualLengthMarker(Linelike, Linelike) -- This predicate only marks two segments with a tick indicating that they have equal length. Only use if EqualLength precedes it. Example use: Point A, B, C, D
-- Segment segmentAB, segmentCD
-- segmentAB := Segment(A, B)
-- segmentCD := Segment(C, D)
-- EqualLength(segmentAB, segmentCD)
-- EqualLengthMarker(segmentAB, segmentCD)
-- AutoLabel A, B, C, D
predicate EqualLength(Linelike, Linelike) -- This predicate makes two segments have equal length. Example use: Point A, B, C, D
-- Segment segmentAB, segmentCD
-- segmentAB := Segment(A, B)
-- segmentCD := Segment(C, D)
-- EqualLength(segmentAB, segmentCD)
-- AutoLabel A, B, C, D
predicate Parallel(Linelike, Linelike) -- This predicate makes two lines parallel. Example use: Point A, B, C, D
-- Line lineAB, lineCD
-- lineAB := Line(A, B)
-- lineCD := Line(C, D)
-- Parallel(lineAB, lineCD)
-- AutoLabel A, B, C, D

-- Angles
predicate Acute(Angle) -- This predicate makes an angle acute. Example use: Point A, B, C
-- Angle angleABC
-- angleABC := InteriorAngle(A, B, C)
-- Acute(angleABC)
-- AutoLabel A, B, C
predicate Obtuse(Angle) -- This predicate makes an angle obtuse. Example use: Point A, B, C
-- Angle angleABC
-- angleABC := InteriorAngle(A, B, C)
-- Obtuse(angleABC)
-- AutoLabel A, B, C
predicate RightMarked(Angle) -- This predicate makes an angle right and marks it with a square. Example use: Point A, B, C
-- Angle angleABC
-- angleABC := InteriorAngle(A, B, C)
-- RightMarked(angleABC)
-- AutoLabel A, B, C
predicate RightUnmarked(Angle) -- This predicate makes an angle right and does not mark it with a square. Example use: Point A, B, C
-- Angle angleABC
-- angleABC := InteriorAngle(A, B, C)
-- RightUnmarked(angleABC)
-- AutoLabel A, B, C
predicate AngleBisector(Angle, Linelike) -- This predicate makes a ray be the angle bisector of an angle. Example use: Point A, B, C
-- Angle angleABC
-- angleABC := InteriorAngle(A, B, C)
-- Ray rayABC
-- rayABC := Ray(A, B)
-- AngleBisector(angleABC, rayABC)
-- AutoLabel A, B, C
predicate EqualAngleMarker(Angle, Angle) -- This predicate only marks two angles with a tick indicating that they have equal measure. Only use if EqualAngle precedes it. Example use: Point A, B, C, D, E, F
-- Angle angleABC, angleDEF
-- angleABC := InteriorAngle(A, B, C)
-- angleDEF := InteriorAngle(D, E, F)
-- EqualAngle(angleABC, angleDEF)
-- EqualAngleMarker(angleABC, angleDEF)
-- AutoLabel A, B, C, D, E, F
predicate EqualAngle(Angle, Angle) -- This predicate makes two angles have equal measure. Example use: Point A, B, C, D, E, F
-- Angle angleABC, angleDEF
-- angleABC := InteriorAngle(A, B, C)
-- angleDEF := InteriorAngle(D, E, F)
-- EqualAngle(angleABC, angleDEF)
-- AutoLabel A, B, C, D, E, F

-- Polygons/Shapes
predicate Parallelogram(Quadrilateral) -- This predicate makes a quadrilateral a parallelogram. Example use: Point A, B, C, D
-- Quadrilateral quadrilateralABCD
-- quadrilateralABCD := Quadrilateral(A, B, C, D)
-- Parallelogram(quadrilateralABCD)
-- AutoLabel A, B, C, D
predicate OnCircle(Circle, Point) -- This predicate makes a point be on a circle. Example use: Point A, B, C
-- Circle circleAB
-- circleAB := CircleR(A, B)
-- OnCircle(circleAB, C)
-- AutoLabel A, B, C
predicate CircleCenter(Circle, Point) -- Do not use.
predicate Incenter(Point, Triangle) -- This predicate makes a point be the incenter of a triangle. Example use: Point A, B, C, D
-- Triangle triangleABC
-- triangleABC := Triangle(A, B, C)
-- Incenter(D, triangleABC)
-- AutoLabel A, B, C, D
predicate Orthocenter(Point, Triangle) -- This predicate makes a point be the orthocenter of a triangle. Example use: Point A, B, C, D
-- Triangle triangleABC
-- triangleABC := Triangle(A, B, C)
-- Orthocenter(D, triangleABC)
-- AutoLabel A, B, C, D
predicate Centroid(Point, Triangle) -- This predicate makes a point be the centroid of a triangle. Example use: Point A, B, C, D
-- Triangle triangleABC
-- triangleABC := Triangle(A, B, C)
-- Centroid(D, triangleABC)
-- AutoLabel A, B, C, D
predicate Circumcenter(Point, Triangle) -- This predicate makes a point be the circumcenter of a triangle. Example use: Point A, B, C, D
-- Triangle triangleABC
-- triangleABC := Triangle(A, B, C)
-- Circumcenter(D, triangleABC)
-- AutoLabel A, B, C, D

-- notation "{p, q}" ~ "Segment(p, q)"
-- notation "{p, q, r}" ~ "Triangle(p, q, r)"
-- notation "{p, q, r, s}" ~ "Rectangle(p, q, r, s)"
-- notation "∠(p, q, r)" ~ "InteriorAngle(p, q, r)"

`,
};

export const sampleSubstances = {
  methane: {
    prog: methane,
    name: "lewis_1; Methane",
  },
  acetyleneAndSulfuricAcid: {
    prog: ` -- Lewis structure of nitrogen molecule
Nitrogen n1, n2

Bond b1 := MakeTripleBond(n1, n2)

-- Lewis structure of Sulfuric Acid
Hydrogen h3, h4
Sulfur s1
Oxygen o1, o2, o3, o4

Bond b2 := MakeSingleBond(h3, o1)
Bond b3 := MakeSingleBond(h4, o2)
Bond b4 := MakeSingleBond(o1, s1)
Bond b5 := MakeSingleBond(o2, s1)
Bond b6 := MakeDoubleBond(o3, s1)
Bond b7 := MakeDoubleBond(o4, s1)

ZeroDots(h3)
ZeroDots(h4)
ZeroDots(s1)
FourDots(o1)
FourDots(o2)
FourDots(o3)
FourDots(o4)
`,
    name: "Lewis structures of nitrogen and sulfuric acid",
  },
  eulerCircuit1: {
    prog: sec5ex21,
    name: "graph_2; Euler Circuit 1",
  },
  angleBisector: {
    prog: c02p01,
    name: "c02p01: Angle Bisector",
  },
  geometry: {
    prog: `-- Defining the points
Point A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q

-- Defining the Line, Segment and Ray
Line lineAB := Line(A, B)
Segment segmentBC := Segment(B, C)
Ray rayCD := Ray(C, D)

-- Defining a Point as Midpoint of a Line
Point AB := Midpoint(lineAB)

-- Defining the Angles
Angle angleABC := InteriorAngle(A, B, C)
Angle angleBCD := InteriorAngle(B, C, D)

-- Defining Polygons/Shapes
Triangle triangleABC := Triangle(A, B, C)
Rectangle rectangleEFGH := Rectangle(E, F, G, H)
Quadrilateral quadrilateralIJKL := Quadrilateral(I, J, K, L)
Circle circleMN := CircleR(M, N)

-- Defining the Plane
Plane V

-- Using the functions
Ray rayBA := Bisector(angleABC)
Segment segmentEF := PerpendicularBisector(segmentBC, E)
Segment segmentFG := PerpendicularBisectorLabelPts(segmentBC, F, G)
Segment segmentKL := MidSegment(triangleABC, K, L)
Segment segmentMN := Radius(circleMN, N)
Segment segmentNO := Chord(circleMN, N, O)
Segment segmentMO := Diameter(circleMN, O, P)

-- Using the predicates
On(A, lineAB)
In(A, V)
Midpoint(lineAB, AB)
Collinear(A, B, C)
ParallelMarker1(lineAB, rayCD)
EqualLengthMarker(segmentBC, segmentEF)
EqualLength(segmentBC, segmentEF)
Parallel(lineAB, rayCD)
Acute(angleABC)
Obtuse(angleBCD)
RightMarked(angleABC)
RightUnmarked(angleBCD)
-- AngleBisector(angleABC, rayBA)
EqualAngleMarker(angleABC, angleBCD)
EqualAngle(angleABC, angleBCD)
Parallelogram(quadrilateralIJKL)
OnCircle(circleMN, N)
-- CircleCenter(circleMN, M)
-- Incenter(A, triangleABC)
-- Orthocenter(B, triangleABC)
-- Centroid(C, triangleABC)
-- Circumcenter(D, triangleABC)

AutoLabel A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, V, AB
`,
    name: "Complete Geometry domain",
  },
};

// description prelude
export const descriptionPreludes = [
  `Given the context above, can you generate a new Substance program which describes the following:`,
];

export const finalInsts = [
  `To write comments, begin with \`--\`. Return only the Substance program; do not explain your reasoning whatsoever.`,
  `To write comments, begin with \`--\`. Return only the Substance program; do not explain your reasoning whatsoever. Avoid duplicate naming of variables.`,
];

export const generatePrompt = (
  systemInst: string,
  penroseContext: string,
  domain: string,
  descriptionPrelude: string,
  description: string,
  finalInst: string,
  style: string,
  sampleSubstance?: { prog: string; name: string },
  bnf?: string,
) => {
  let prompt = "";
  if (sampleSubstance && bnf) {
    prompt = `${systemInst}${penroseContext}Here is a BNF grammar that the Substance program you generate should conform to:

\`\`\`
${bnf}
\`\`\`

${descriptionPrelude} ${description} 

${finalInst}`;
    return {
      systemInst,
      penroseContext,
      domain,
      descriptionPrelude,
      description,
      finalInst,
      prompt,
      style,
      sampleSubstance,
      bnf,
    };
  } else if (sampleSubstance) {
    prompt = `${systemInst}${penroseContext}Here is a Domain program which would inform a Substance program:
\`\`\`
${domain}
\`\`\`

Here is a sample Substance program named \"${sampleSubstance.name}\":

\`\`\`
${sampleSubstance.prog}
\`\`\`

${descriptionPrelude} ${description} 

${finalInst}`;
    return {
      systemInst,
      penroseContext,
      domain,
      descriptionPrelude,
      description,
      finalInst,
      prompt,
      style,
      sampleSubstance,
    };
  } else {
    prompt = `${systemInst}${penroseContext}Here is a Domain program which would inform a Substance program:
\`\`\`
${domain}
\`\`\`

Here is a BNF grammar that the Substance program you generate should conform to:

\`\`\`
${bnf}
\`\`\`

${descriptionPrelude} ${description} 

${finalInst}`;
    return {
      systemInst,
      penroseContext,
      domain,
      descriptionPrelude,
      description,
      finalInst,
      prompt,
      style,
      bnf,
    };
  }
};
/**/

export const llmPrompts: LLMPromptCollection = {
  lewis_0_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of water?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
  ),
  lewis_0_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of H2O, with two hydrogen atoms each single bonded to one oxygen atom and with four dots on the oxygen atom?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
  ),
  lewis_0_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of water?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar,
  ),
  lewis_0_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of H2O, with two hydrogen atoms each single bonded to one oxygen atom and with four dots on the oxygen atom?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar,
  ),
  lewis_1_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of nitric acid?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
  ),
  lewis_1_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of nitric acid? Draw 3 oxygen atoms, 1 nitrogen, and 1 hydrogen; draw a single bond from one of the oxygen atoms to the nitrogen and the hydrogen and draw 4 dots on it. Draw 6 dots on another oxygen atom and a single bond from it to the nitrogen. Draw 4 dots on the last oxygen and a double bond to the nitrogen.`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
  ),
  lewis_1_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of nitric acid?
      
      `,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar,
  ),
  lewis_1_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of nitric acid? Draw 3 oxygen atoms, 1 nitrogen, and 1 hydrogen; draw a single bond from one of the oxygen atoms to the nitrogen and the hydrogen and draw 4 dots on it. Draw 6 dots on another oxygen atom and a single bond from it to the nitrogen. Draw 4 dots on the last oxygen and a double bond to the nitrogen.`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar,
  ),
  lewis_2_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of salt?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
  ),
  lewis_2_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of salt? Draw 1 sodium and 1 chlorine atom with a single bond between them and 6 dots on the chlorine.`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
  ),
  lewis_2_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of salt?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar,
  ),
  lewis_2_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of salt? Draw 1 sodium and 1 chlorine atom with a single bond between them and 6 dots on the chlorine.`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar,
  ),
  lewis_3_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of acetylene?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
  ),
  lewis_3_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of acetylene? Draw 2 carbon atoms with a triple bond between them. Draw 2 hydrogen atoms, each single bonded to one of the carbon atoms.`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
  ),
  lewis_3_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of acetylene?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar,
  ),
  lewis_3_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of acetylene? Draw 2 carbon atoms with a triple bond between them. Draw 2 hydrogen atoms, each single bonded to one of the carbon atoms.`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar,
  ),
  lewis_4_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of carbon tetrachloride?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
  ),
  lewis_4_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of carbon tetrachloride? Draw 1 carbon atom. Draw 4 chlorine atoms, each single bonded to the carbon atom. Draw six dots on each chlorine atom.`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
  ),
  lewis_4_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of carbon tetrachloride?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar,
  ),
  lewis_4_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of carbon tetrachloride? Draw 1 carbon atom. Draw 4 chlorine atoms, each single bonded to the carbon atom. Draw six dots on each chlorine atom.`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar,
  ),
  lewis_5_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of formaldehyde?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
  ),
  lewis_5_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of formaldehyde? Draw 1 carbon, 1 oxygen, and 2 hydrogen atoms. Draw a single bond from each hydrogen atom to the carbon atom. Draw a double bond from the oxygen atom to the carbon atom. Draw 4 dots on the oxygen atom.`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
  ),
  lewis_5_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of formaldehyde?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar,
  ),
  lewis_5_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of formaldehyde? Draw 1 carbon, 1 oxygen, and 2 hydrogen atoms. Draw a single bond from each hydrogen atom to the carbon atom. Draw a double bond from the oxygen atom to the carbon atom. Draw 4 dots on the oxygen atom.`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar,
  ),
  graph_0_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a Hamiltonian graph?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
  ),
  graph_0_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a Hamiltonian graph with 5 nodes and 8 edges with a cycle that visits each node exactly once?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
  ),
  graph_0_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a Hamiltonian graph?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar,
  ),
  graph_0_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a Hamiltonian graph with 5 nodes and 8 edges with a cycle that visits each node exactly once?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar,
  ),
  graph_1_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a bipartite graph?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
  ),
  graph_1_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a bipartite graph with 3 nodes in one set and 3 nodes in the other, where nodes are only connected to nodes in the other set?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
  ),
  graph_1_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a bipartite graph?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar,
  ),
  graph_1_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a bipartite graph with 3 nodes in one set and 3 nodes in the other, where nodes are only connected to nodes in the other set?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar,
  ),
  graph_2_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a strongly connected graph?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
  ),
  graph_2_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a strongly connected graph with 5 nodes where each node is connected to every other node by an edge?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
  ),
  graph_2_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a strongly connected graph?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar,
  ),
  graph_2_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a strongly connected graph with 5 nodes where each node is connected to every other node by an edge?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar,
  ),
  graph_3_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a graph with 7 nodes which is an arborescence?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
  ),
  graph_3_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a graph with 7 nodes which is an arborescence, meaning it is connected and acyclic, with all arcs pointing outward from the root?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
  ),
  graph_3_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a graph with 7 nodes which is an arborescence?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar,
  ),
  graph_3_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a graph with 7 nodes which is an arborescence, meaning it is connected and acyclic, with all arcs pointing outward from the root?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar,
  ),
  graph_4_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a graph with one strong and one weak component?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
  ),
  graph_4_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a graph with 9 nodes which has one strong component, where all nodes are reachable from all other nodes, and one weak component?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
  ),
  graph_4_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a graph with one strong and one weak component?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar,
  ),
  graph_4_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a graph with 9 nodes which has one strong component, where all nodes are reachable from all other nodes, and one weak component?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar,
  ),
  graph_5_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a graph with three different connected components?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
  ),
  graph_5_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a graph with three different connected components, which are components in which all the nodes are always reachable from each other?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
  ),
  graph_5_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a graph with three different connected components?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar,
  ),
  graph_5_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    commentedDomains.simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a graph with three different connected components, which are components in which all the nodes are always reachable from each other?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar,
  ),
  geometry_0_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `two congruent triangles that demonstrate the SAS theorem?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
  ),
  geometry_0_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `two triangles ABC and DEF where AB is equal to DE, BC is equal to EF, and angle B is equal to angle E?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
  ),
  geometry_0_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `two congruent triangles that demonstrate the SAS theorem?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
    grammars.geometryGrammar,
  ),
  geometry_0_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `two triangles ABC and DEF where AB is equal to DE, BC is equal to EF, and angle B is equal to angle E?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
    grammars.geometryGrammar,
  ),
  geometry_1_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a pair of complementary angles?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
  ),
  geometry_1_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a right angle ABC and a pair of angles ABD and DBC that add up to 90 degrees and are adjacent to each other?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
  ),
  geometry_1_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a pair of complementary angles?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
    grammars.geometryGrammar,
  ),
  geometry_1_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a right angle ABC and a pair of angles ABD and DBC that add up to 90 degrees and are adjacent to each other?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
    grammars.geometryGrammar,
  ),
  geometry_2_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a square inscribed in a circle?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
  ),
  geometry_2_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a circle with four chords AB, BC, CD, and DA of equal length such that AB is parallel to CD and BC is parallel to DA to form a square?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
  ),
  geometry_2_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a square inscribed in a circle?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
    grammars.geometryGrammar,
  ),
  geometry_2_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a circle with four chords AB, BC, CD, and DA of equal length such that AB is parallel to CD and BC is parallel to DA to form a square?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
    grammars.geometryGrammar,
  ),
  geometry_3_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a line segment which has a line segment at its midpoint forming an acute and an obtuse angle?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
  ),
  geometry_3_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `Draw three points A, B, C, and D. Draw segments AB and CD. Make C the midpoint of AB. Draw an acute angle ACD and an obtuse angle DCB.`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
  ),
  geometry_3_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a line segment which has a line segment at its midpoint forming an acute and an obtuse angle?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
    grammars.geometryGrammar,
  ),
  geometry_3_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `Draw three points A, B, C, and D. Draw segments AB and CD. Make C the midpoint of AB. Draw an acute angle ACD and an obtuse angle DCB.`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
    grammars.geometryGrammar,
  ),
  geometry_4_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a parallelogram with opposite sides and angles marked equal?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
  ),
  geometry_4_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `Draw a parallelogram ABCD. Draw the segments AB, BC, CD, and DA. Draw all four angles ABC, BCD, CDA, and DAB. Mark the opposite angles (ABC & CDA; BCD & DAB) equal. Mark the opposite sides (AB & CD; BC & DA) equal.`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
  ),
  geometry_4_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a parallelogram with opposite sides and angles marked equal?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
    grammars.geometryGrammar,
  ),
  geometry_4_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `Draw a parallelogram ABCD. Draw the segments AB, BC, CD, and DA. Draw all four angles ABC, BCD, CDA, and DAB. Mark the opposite angles (ABC & CDA; BCD & DAB) equal. Mark the opposite sides (AB & CD; BC & DA) equal.`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
    grammars.geometryGrammar,
  ),
  geometry_5_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a scalene triangle?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
  ),
  geometry_5_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `Draw a scalene triangle ABC. Draw the segments AB, BC, and CA. Draw all three angles ABC, BCA, and CAB. Make the angles ABC and BCA acute. Make the angle CAB obtuse.`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
  ),
  geometry_5_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a scalene triangle?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
    grammars.geometryGrammar,
  ),
  geometry_5_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `Draw a scalene triangle ABC. Draw the segments AB, BC, and CA. Draw all three angles ABC, BCA, and CAB. Make the angles ABC and BCA acute. Make the angle CAB obtuse.`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry,
    grammars.geometryGrammar,
  ),
};

// takes in LLMPrompt
export const generateSubstanceLLM = async ({
  prompt,
  openaiApiKey,
}: {
  prompt: LLMPrompt;
  openaiApiKey: string;
}): Promise<LLMResult> => {
  let output = "";

  const apiUrl = "https://api.openai.com/v1/chat/completions";

  const headers = {
    "Content-Type": "application/json",
    Authorization: `Bearer ${openaiApiKey}`,
  };

  console.log(prompt.prompt);
  const data = {
    //model: "gpt-3.5-turbo-16k",
    model: "gpt-4",
    messages: [{ role: "user", content: prompt.prompt }],
    max_tokens: 2000,
    temperature: 0.7,
  };

  const date = new Date();
  const start = date.getTime();

  const response = await fetch(apiUrl, {
    method: "POST",
    headers: headers,
    body: JSON.stringify(data),
  });
  let end: number = 0;
  if (response.ok) {
    end = Date.now();
    console.log(`Inference time: ${end - start}ms`);
    const result = await response.json();
    // Process the result
    let output = result.choices[0].message.content;
    // remove backticks from output
    output = output.replace(/`/g, "");
    return {
      tag: "Ok",
      prompt,
      date,
      inferenceTime: end - start,
      substance: output,
    };
  } else {
    end = Date.now();
    const error = await response.json();
    return {
      tag: "Error",
      prompt,
      date,
      inferenceTime: end - start,
      APIError: error,
    };
  }
};
