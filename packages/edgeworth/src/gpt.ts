import euclideanStyle from "@penrose/examples/dist/geometry-domain/euclidean.style.js";
import geometryDomain from "@penrose/examples/dist/geometry-domain/geometry.domain.js";
import c02p01 from "@penrose/examples/dist/geometry-domain/textbook_problems/c02p01.substance.js";
import simpleDirectedGraphDomain from "@penrose/examples/dist/graph-domain/simple-directed-graph.domain.js";
import simpleDirectedGraphStyle from "@penrose/examples/dist/graph-domain/simple-directed-graph.style.js";
import sec5ex21 from "@penrose/examples/dist/graph-domain/textbook/sec5/ex21.substance.js";
import lewisStyle from "@penrose/examples/dist/molecules/lewis.style.js";
import methane from "@penrose/examples/dist/molecules/methane.substance.js";
import moleculesDomain from "@penrose/examples/dist/molecules/molecules.domain.js";

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
tname     ::= "Atom" | "Hydrogen" | "Helium" | "Lithium" | "Beryllium" | "Boron" | "Carbon" | "Nitrogen" | "Oxygen" | "Fluorine" | "Neon" | "Sodium" | "Magnesium" | "Aluminium" | "Silicon" | "Phosphorus" | "Sulfur" | "Chlorine" | "Argon" | "Potassium" | "Calcium" | "Scandium" | "Titanium" | "Vanadium" | "Chromium" | "Manganese" | "Iron" | "Cobalt" | "Nickel" | "Copper" | "Zinc" | "Gallium" | "Germanium" | "Arsenic" | "Selenium" | "Bromine" | "Krypton" | "Rubidium" | "Strontium" | "Yttrium" | "Zirconium" | "Niobium" | "Molybdenum" | "Technetium" | "Ruthenium" | "Rhodium" | "Palladium" | "Silver" | "Cadmium" | "Indium" | "Tin" | "Antimony" | "Tellurium" | "Iodine" | "Xenon" | "Cesium" | "Barium" | "Lanthanum" | "Cerium" | "Praseodymium" | "Neodymium" | "Promethium" | "Samarium" | "Europium" | "Gadolinium" | "Terbium" | "Dysprosium" | "Holmium" | "Erbium" | "Thulium" | "Ytterbium" | "Lutetium" | "Hafnium" | "Tantalum" | "Tungsten" | "Rhenium" | "Osmium" | "Iridium" | "Platinum" | "Gold" | "Mercury" | "Thallium" | "Lead" | "Bismuth" | "Polonium" | "Astatine" | "Radon" | "Francium" | "Radium" | "Actinium" | "Thorium" | "Protactinium" | "Uranium" | "Neptunium" | "Plutonium" | "Americium" | "Curium" | "Berkelium" | "Californium" | "Einsteinium" | "Fermium" | "Mendelevium" | "Nobelium" | "Lawrencium" | "Rutherfordium" | "Dubnium" | "Seaborgium" | "Bohrium" | "Hassium" | "Meitnerium" | "Darmstadtium" | "Roentgenium" | "Copernicium" | "Bond"
fname     ::= "MakeSingleBond" | "MakeDoubleBond" | "MakeTripleBond"
pname     ::= "ZeroDots" | "TwoDots" | "FourDots" | "SixDots"
letter    ::= "A" | "B" | ... | "Z" | "a" | "b" | ... | "z"
digit     ::= "0" | "1" | ... | "9"
`,
  simpleDirectedGraphGrammar: `id ::= letter (letter | digit)*
substance ::= stmt*
stmt      ::= decl | predicate
decl      ::= tname id
predicate ::= pname "(" id "," id ")"
tname     ::= "Vertex"
pname     ::= "Arc"
letter    ::= "A" | "B" | ... | "Z" | "a" | "b" | ... | "z"
digit     ::= "0" | "1" | ... | "9"
`,

  geometryGrammar: `id ::= letter (letter | digit)*
substance ::= stmt*
stmt      ::= decl | func1 | func2 | func3 | func4 | pred1 | pred2 | pred3
decl      ::= tname id
func1     ::= tname id ":=" f1name "(" id ")"
func2     ::= tname id ":=" f2name "(" id "," id ")"
func3     ::= tname id ":=" f3name "(" id "," id "," id ")"
func4     ::= tname id ":=" f4name "(" id "," id "," id "," id ")"
pred1     ::= p1name "(" id ")"
pred2     ::= p2name "(" id "," id ")"
pred3     ::= p3name "(" id "," id "," id ")"
tname     ::= "Shape" | "Point" | "Linelike" | "Ray" | "Line" | "Segment" | "Angle" | "Triangle" | "Quadrilateral" | "Rectangle" | "Circle" | "Plane"
f1name    ::= "Midpoint" | "Bisector"
f2name    ::= "Segment" | "Ray" | "Line" | "CircleR" | "PerpendicularBisector" | "Radius"
f3name    ::= "InteriorAngle" | "Triangle" | "PerpendicularBisectorLabelPts" | "MidSegment" | "Chord" | "Diameter"
f4name    ::= "Rectangle" | "Quadrilateral"
p1name    ::= "Acute" | "Obtuse" | "RightMarked" | "RightUnmarked" | "Parallelogram"
p2name    ::= "On" | "In" | "Midpoint" | "ParallelMarker1" | "EqualLengthMarker" | "EqualLength" | "Parallel" | "AngleBisector" | "EqualAngleMarker" | "EqualAngle" | "OnCircle" | "CircleCenter" | "Incenter" | "Orthocenter" | "Centroid" | "Circumcenter"
p3name    ::= "Collinear"
letter    ::= "A" | "B" | ... | "Z" | "a" | "b" | ... | "z"
digit     ::= "0" | "1" | ... | "9"
`,
};

export const sampleSubstances = {
  methane: {
    prog: methane,
    name: "lewis_1; Methane",
  },
  acetyleneAndSulfuricAcid: {
    prog: `-- Lewis structure of Acetylene
Hydrogen h1, h2
Carbon c1, c2

Bond b1 := MakeTripleBond(c1, c2)
Bond b2 := MakeSingleBond(c1, h1)
Bond b3 := MakeSingleBond(c1, h2)

ZeroDots(h1)
ZeroDots(h2)
ZeroDots(c1)
ZeroDots(c2)

-- Lewis structure of Sulfuric Acid
Hydrogen h3, h4
Sulfur s1
Oxygen o1, o2, o3, o4

Bond b4 := MakeSingleBond(h3, o1)
Bond b5 := MakeSingleBond(h4, o2)
Bond b6 := MakeSingleBond(o1, s1)
Bond b7 := MakeSingleBond(o2, s1)
Bond b8 := MakeDoubleBond(o3, s1)
Bond b9 := MakeDoubleBond(o4, s1)

ZeroDots(h3)
ZeroDots(h4)
ZeroDots(s1)
FourDots(o1)
FourDots(o2)
FourDots(o3)
FourDots(o4)
`,
    name: "Lewis structures of acetylene and sulfuric acid",
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
  bnf?: string
) => {
  let prompt = "";
  if (sampleSubstance && bnf) {
    prompt = `${systemInst}${penroseContext}

Here is a BNF grammar that the Substance program you generate should conform to:

\`\`\`
${bnf}
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

export const llmPrompts: LLMPromptCollection = {
  lewis_0_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of water?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid
  ),
  lewis_0_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of H2O, with two hydrogen atoms each single bonded to one oxygen atom and with four dots on the oxygen atom?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid
  ),
  lewis_0_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of water?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar
  ),
  lewis_0_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of H2O, with two hydrogen atoms each single bonded to one oxygen atom and with four dots on the oxygen atom?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar
  ),
  lewis_1_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of nitric acid?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid
  ),
  lewis_1_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of nitric acid? Draw 3 oxygen atoms, 1 nitrogen, and 1 hydrogen; draw a single bond from one of the oxygen atoms to the nitrogen and the hydrogen and draw 4 dots on it. Draw 6 dots on another oxygen atom and a single bond from it to the nitrogen. Draw 4 dots on the last oxygen and a double bond to the nitrogen.`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid
  ),
  lewis_1_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of nitric acid?
      
      `,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar
  ),
  lewis_1_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of nitric acid? Draw 3 oxygen atoms, 1 nitrogen, and 1 hydrogen; draw a single bond from one of the oxygen atoms to the nitrogen and the hydrogen and draw 4 dots on it. Draw 6 dots on another oxygen atom and a single bond from it to the nitrogen. Draw 4 dots on the last oxygen and a double bond to the nitrogen.`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar
  ),
  lewis_2_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of salt?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid
  ),
  lewis_2_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of salt? Draw 1 sodium and 1 chlorine atom with a single bond between them and 6 dots on the chlorine.`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid
  ),
  lewis_2_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of salt?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar
  ),
  lewis_2_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    moleculesDomain,
    descriptionPreludes[0],
    `the Lewis structure of salt? Draw 1 sodium and 1 chlorine atom with a single bond between them and 6 dots on the chlorine.`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.acetyleneAndSulfuricAcid,
    grammars.moleculesGrammar
  ),
  graph_0_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a Hamiltonian graph?`,
    finalInsts[0],
    lewisStyle,
    sampleSubstances.eulerCircuit1
  ),
  graph_0_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a Hamiltonian graph with 5 nodes and 8 edges with a cycle that visits each node exactly once?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1
  ),
  graph_0_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a Hamiltonian graph?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar
  ),
  graph_0_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a Hamiltonian graph with 5 nodes and 8 edges with a cycle that visits each node exactly once?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar
  ),
  graph_1_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a bipartite graph?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1
  ),
  graph_1_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a bipartite graph with 3 nodes in one set and 3 nodes in the other, where nodes are only connected to nodes in the other set?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1
  ),
  graph_1_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a bipartite graph?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar
  ),
  graph_1_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a bipartite graph with 3 nodes in one set and 3 nodes in the other, where nodes are only connected to nodes in the other set?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar
  ),
  graph_2_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a strongly connected graph?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1
  ),
  graph_2_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a strongly connected graph with 5 nodes where each node is connected to every other node by an edge?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1
  ),
  graph_2_1_0: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a strongly connected graph?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar
  ),
  graph_2_1_1: generatePrompt(
    systemInsts[1],
    penroseContexts[0],
    simpleDirectedGraphDomain,
    descriptionPreludes[0],
    `a strongly connected graph with 5 nodes where each node is connected to every other node by an edge?`,
    finalInsts[0],
    simpleDirectedGraphStyle,
    sampleSubstances.eulerCircuit1,
    grammars.simpleDirectedGraphGrammar
  ),
  geometry_0_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `two congruent triangles that demonstrate the SAS theorem?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry
  ),
  geometry_0_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `two triangles ABC and DEF where AB is equal to DE, BC is equal to EF, and angle B is equal to angle E?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry
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
    grammars.geometryGrammar
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
    grammars.geometryGrammar
  ),
  geometry_1_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a pair of complementary angles?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry
  ),
  geometry_1_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a right angle ABC and a pair of angles ABD and DBC that add up to 90 degrees and are adjacent to each other?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry
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
    grammars.geometryGrammar
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
    grammars.geometryGrammar
  ),
  geometry_2_0_0: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a square inscribed in a circle?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry
  ),
  geometry_2_0_1: generatePrompt(
    systemInsts[0],
    penroseContexts[0],
    geometryDomain,
    descriptionPreludes[0],
    `a circle with four chords AB, BC, CD, and DA of equal length such that AB is parallel to CD and BC is parallel to DA to form a square?`,
    finalInsts[1],
    euclideanStyle,
    sampleSubstances.geometry
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
    grammars.geometryGrammar
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
    grammars.geometryGrammar
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
