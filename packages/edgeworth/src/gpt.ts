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
  `You are a code generator that is generating a new program in the Substance programming language, which draws from the specific Domain whose BNF grammar is given below. To write comments, begin with \`--\`. Return only the Substance program; do not explain your reasoning whatsoever.

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
Line AB := Line(A, B)
Segment BC := Segment(B, C)
Ray CD := Ray(C, D)

-- Defining a Point as Midpoint of a Line
Point MID_AB := Midpoint(AB)

-- Defining the Angles
Angle ABC := InteriorAngle(A, B, C)
Angle BCD := InteriorAngle(B, C, D)

-- Defining Polygons/Shapes
Triangle ABC_T := Triangle(A, B, C)
Rectangle EFGH := Rectangle(E, F, G, H)
Quadrilateral IJKL := Quadrilateral(I, J, K, L)
Circle M_Circle := CircleR(M, N)

-- Defining the Plane
Plane plane

-- Using the functions
Ray BA := Bisector(ABC)
Segment EF := PerpendicularBisector(BC, E)
Segment FG := PerpendicularBisectorLabelPts(BC, F, G)
Segment KL := MidSegment(ABC_T, K, L)
Segment MN := Radius(M_Circle, N)
Segment NO := Chord(M_Circle, N, O)
Segment MO := Diameter(M_Circle, O, P)

-- Using the predicates
On(A, AB)
In(A, plane)
Midpoint(AB, MID_AB)
Collinear(A, B, C)
ParallelMarker1(AB, CD)
EqualLengthMarker(BC, EF)
EqualLength(BC, EF)
Parallel(AB, CD)
Acute(ABC)
Obtuse(BCD)
RightMarked(ABC)
RightUnmarked(BCD)
AngleBisector(ABC, BA)
EqualAngleMarker(ABC, BCD)
EqualAngle(ABC, BCD)
Parallelogram(IJKL)
OnCircle(M_Circle, N)
CircleCenter(M_Circle, M)
Incenter(A, ABC_T)
Orthocenter(B, ABC_T)
Centroid(C, ABC_T)
Circumcenter(D, ABC_T)

AutoLabel All
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
];

export const generatePromptText = (
  systemInst: string,
  penroseContext: string,
  domain: string,
  descriptionPrelude: string,
  description: string,
  finalInst: string,
  sampleSubstance?: { prog: string; name: string },
  bnf?: string
) => {
  if (sampleSubstance && bnf) {
    return `${systemInst}${penroseContext}Here is a Domain program which would inform a Substance program:
\`\`\`
${domain}
\`\`\`

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
  } else if (sampleSubstance) {
    return `${systemInst}${penroseContext}Here is a Domain program which would inform a Substance program:
\`\`\`
${domain}
\`\`\`

Here is a sample Substance program named \"${sampleSubstance.name}\":

\`\`\`
${sampleSubstance.prog}
\`\`\`

${descriptionPrelude} ${description} 

${finalInst}`;
  } else {
    return `${systemInst}${penroseContext}Here is a Domain program which would inform a Substance program:
\`\`\`
${domain}
\`\`\`

Here is a BNF grammar that the Substance program you generate should conform to:

\`\`\`
${bnf}
\`\`\`

${descriptionPrelude} ${description} 

${finalInst}`;
  }
};

export const llmPrompts: LLMPromptCollection = {
  lewis_0_0_0: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: moleculesDomain,
    sampleSubstance: sampleSubstances.acetyleneAndSulfuricAcid,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `the Lewis structure of water?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      moleculesDomain,
      descriptionPreludes[0],
      `the Lewis structure of water?`,
      finalInsts[0],
      sampleSubstances.acetyleneAndSulfuricAcid
    ),
    style: lewisStyle,
  },
  lewis_0_0_1: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: moleculesDomain,
    sampleSubstance: sampleSubstances.acetyleneAndSulfuricAcid,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `the Lewis structure of H2O, with two hydrogen atoms each single bonded to one oxygen atom and with four dots on the oxygen atom?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      moleculesDomain,
      descriptionPreludes[0],
      `the Lewis structure of H2O, with two hydrogen atoms each single bonded to one oxygen atom and with four dots on the oxygen atom?`,
      finalInsts[0],
      sampleSubstances.acetyleneAndSulfuricAcid
    ),
    style: lewisStyle,
  },
  lewis_0_1_0: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: moleculesDomain,
    bnf: grammars.moleculesGrammar,
    sampleSubstance: sampleSubstances.acetyleneAndSulfuricAcid,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `the Lewis structure of water?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      moleculesDomain,
      descriptionPreludes[0],
      `the Lewis structure of water?`,
      finalInsts[0],
      sampleSubstances.acetyleneAndSulfuricAcid,
      grammars.moleculesGrammar
    ),
    style: lewisStyle,
  },
  lewis_0_1_1: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: moleculesDomain,
    bnf: grammars.moleculesGrammar,
    sampleSubstance: sampleSubstances.acetyleneAndSulfuricAcid,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `the Lewis structure of H2O, with two hydrogen atoms each single bonded to one oxygen atom and with four dots on the oxygen atom?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      moleculesDomain,
      descriptionPreludes[0],
      `the Lewis structure of H2O, with two hydrogen atoms each single bonded to one oxygen atom and with four dots on the oxygen atom?`,
      finalInsts[0],
      sampleSubstances.acetyleneAndSulfuricAcid,
      grammars.moleculesGrammar
    ),
    style: lewisStyle,
  },
  lewis_1_0_0: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: moleculesDomain,
    sampleSubstance: sampleSubstances.acetyleneAndSulfuricAcid,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `the Lewis structure of nitric acid?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      moleculesDomain,
      descriptionPreludes[0],
      `the Lewis structure of nitric acid?`,
      finalInsts[0],
      sampleSubstances.acetyleneAndSulfuricAcid
    ),
    style: lewisStyle,
  },
  lewis_1_0_1: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: moleculesDomain,
    sampleSubstance: sampleSubstances.acetyleneAndSulfuricAcid,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `the Lewis structure of nitric acid? Draw 3 oxygen atoms, 1 nitrogen, and 1 hydrogen; draw a single bond from one of the oxygen atoms to the nitrogen and the hydrogen and draw 4 dots on it. Draw 6 dots on another oxygen atom and a single bond from it to the nitrogen. Draw 4 dots on the last oxygen and a double bond to the nitrogen.
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      moleculesDomain,
      descriptionPreludes[0],
      `the Lewis structure of nitric acid? Draw 3 oxygen atoms, 1 nitrogen, and 1 hydrogen; draw a single bond from one of the oxygen atoms to the nitrogen and the hydrogen and draw 4 dots on it. Draw 6 dots on another oxygen atom and a single bond from it to the nitrogen. Draw 4 dots on the last oxygen and a double bond to the nitrogen.`,
      finalInsts[0],
      sampleSubstances.acetyleneAndSulfuricAcid
    ),
    style: lewisStyle,
  },
  lewis_1_1_0: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: moleculesDomain,
    bnf: grammars.moleculesGrammar,
    sampleSubstance: sampleSubstances.acetyleneAndSulfuricAcid,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `the Lewis structure of nitric acid?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      moleculesDomain,
      descriptionPreludes[0],
      `the Lewis structure of nitric acid?
      
      `,
      finalInsts[0],
      sampleSubstances.acetyleneAndSulfuricAcid,
      grammars.moleculesGrammar
    ),
    style: lewisStyle,
  },
  lewis_1_1_1: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: moleculesDomain,
    bnf: grammars.moleculesGrammar,
    sampleSubstance: sampleSubstances.acetyleneAndSulfuricAcid,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `the Lewis structure of nitric acid? Draw 3 oxygen atoms, 1 nitrogen, and 1 hydrogen; draw a single bond from one of the oxygen atoms to the nitrogen and the hydrogen and draw 4 dots on it. Draw 6 dots on another oxygen atom and a single bond from it to the nitrogen. Draw 4 dots on the last oxygen and a double bond to the nitrogen.
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      moleculesDomain,
      descriptionPreludes[0],
      `the Lewis structure of nitric acid? Draw 3 oxygen atoms, 1 nitrogen, and 1 hydrogen; draw a single bond from one of the oxygen atoms to the nitrogen and the hydrogen and draw 4 dots on it. Draw 6 dots on another oxygen atom and a single bond from it to the nitrogen. Draw 4 dots on the last oxygen and a double bond to the nitrogen.`,
      finalInsts[0],
      sampleSubstances.acetyleneAndSulfuricAcid,
      grammars.moleculesGrammar
    ),
    style: lewisStyle,
  },
  lewis_2_0_0: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: moleculesDomain,
    sampleSubstance: sampleSubstances.acetyleneAndSulfuricAcid,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `the Lewis structure of salt?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      moleculesDomain,
      descriptionPreludes[0],
      `the Lewis structure of salt?`,
      finalInsts[0],
      sampleSubstances.acetyleneAndSulfuricAcid
    ),
    style: lewisStyle,
  },
  lewis_2_0_1: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: moleculesDomain,
    sampleSubstance: sampleSubstances.acetyleneAndSulfuricAcid,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `the Lewis structure of salt? Draw 1 sodium and 1 chlorine atom with a single bond between them and 6 dots on the chlorine.
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      moleculesDomain,
      descriptionPreludes[0],
      `the Lewis structure of salt? Draw 1 sodium and 1 chlorine atom with a single bond between them and 6 dots on the chlorine.`,
      finalInsts[0],
      sampleSubstances.acetyleneAndSulfuricAcid
    ),
    style: lewisStyle,
  },
  lewis_2_1_0: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: moleculesDomain,
    bnf: grammars.moleculesGrammar,
    sampleSubstance: sampleSubstances.acetyleneAndSulfuricAcid,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `the Lewis structure of salt?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      moleculesDomain,
      descriptionPreludes[0],
      `the Lewis structure of salt?`,
      finalInsts[0],
      sampleSubstances.acetyleneAndSulfuricAcid,
      grammars.moleculesGrammar
    ),
    style: lewisStyle,
  },
  lewis_2_1_1: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: moleculesDomain,
    bnf: grammars.moleculesGrammar,
    sampleSubstance: sampleSubstances.acetyleneAndSulfuricAcid,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `the Lewis structure of salt? Draw 1 sodium and 1 chlorine atom with a single bond between them and 6 dots on the chlorine.
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      moleculesDomain,
      descriptionPreludes[0],
      `the Lewis structure of salt? Draw 1 sodium and 1 chlorine atom with a single bond between them and 6 dots on the chlorine.`,
      finalInsts[0],
      sampleSubstances.acetyleneAndSulfuricAcid,
      grammars.moleculesGrammar
    ),
    style: lewisStyle,
  },
  graph_0_0_0: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: simpleDirectedGraphDomain,
    sampleSubstance: sampleSubstances.eulerCircuit1,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a Hamiltonian graph?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      simpleDirectedGraphDomain,
      descriptionPreludes[0],
      `a Hamiltonian graph?`,
      finalInsts[0],
      sampleSubstances.eulerCircuit1
    ),
    style: simpleDirectedGraphStyle,
  },
  graph_0_0_1: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: simpleDirectedGraphDomain,
    sampleSubstance: sampleSubstances.eulerCircuit1,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a Hamiltonian graph with 5 nodes and 8 edges with a cycle that visits each node exactly once?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      simpleDirectedGraphDomain,
      descriptionPreludes[0],
      `a Hamiltonian graph with 5 nodes and 8 edges with a cycle that visits each node exactly once?`,
      finalInsts[0],
      sampleSubstances.eulerCircuit1
    ),
    style: simpleDirectedGraphStyle,
  },
  graph_0_1_0: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: simpleDirectedGraphDomain,
    bnf: grammars.simpleDirectedGraphGrammar,
    sampleSubstance: sampleSubstances.eulerCircuit1,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a Hamiltonian graph?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      simpleDirectedGraphDomain,
      descriptionPreludes[0],
      `a Hamiltonian graph?`,
      finalInsts[0],
      sampleSubstances.eulerCircuit1,
      grammars.simpleDirectedGraphGrammar
    ),
    style: simpleDirectedGraphStyle,
  },
  graph_0_1_1: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: simpleDirectedGraphDomain,
    bnf: grammars.simpleDirectedGraphGrammar,
    sampleSubstance: sampleSubstances.eulerCircuit1,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a Hamiltonian graph with 5 nodes and 8 edges with a cycle that visits each node exactly once?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      simpleDirectedGraphDomain,
      descriptionPreludes[0],
      `a Hamiltonian graph with 5 nodes and 8 edges with a cycle that visits each node exactly once?`,
      finalInsts[0],
      sampleSubstances.eulerCircuit1,
      grammars.simpleDirectedGraphGrammar
    ),

    style: simpleDirectedGraphStyle,
  },
  graph_1_0_0: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: simpleDirectedGraphDomain,
    sampleSubstance: sampleSubstances.eulerCircuit1,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a bipartite graph?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      simpleDirectedGraphDomain,
      descriptionPreludes[0],
      `a bipartite graph?`,
      finalInsts[0],
      sampleSubstances.eulerCircuit1
    ),
    style: simpleDirectedGraphStyle,
  },
  graph_1_0_1: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: simpleDirectedGraphDomain,
    sampleSubstance: sampleSubstances.eulerCircuit1,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a bipartite graph with 3 nodes in one set and 3 nodes in the other, where nodes are only connected to nodes in the other set?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      simpleDirectedGraphDomain,
      descriptionPreludes[0],
      `a bipartite graph with 3 nodes in one set and 3 nodes in the other, where nodes are only connected to nodes in the other set?`,
      finalInsts[0],
      sampleSubstances.eulerCircuit1
    ),
    style: simpleDirectedGraphStyle,
  },
  graph_1_1_0: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: simpleDirectedGraphDomain,
    bnf: grammars.simpleDirectedGraphGrammar,
    sampleSubstance: sampleSubstances.eulerCircuit1,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a bipartite graph?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      simpleDirectedGraphDomain,
      descriptionPreludes[0],
      `a bipartite graph?`,
      finalInsts[0],
      sampleSubstances.eulerCircuit1,
      grammars.simpleDirectedGraphGrammar
    ),
    style: simpleDirectedGraphStyle,
  },
  graph_1_1_1: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: simpleDirectedGraphDomain,
    bnf: grammars.simpleDirectedGraphGrammar,
    sampleSubstance: sampleSubstances.eulerCircuit1,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a bipartite graph with 3 nodes in one set and 3 nodes in the other, where nodes are only connected to nodes in the other set?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      simpleDirectedGraphDomain,
      descriptionPreludes[0],
      `a bipartite graph with 3 nodes in one set and 3 nodes in the other, where nodes are only connected to nodes in the other set?`,
      finalInsts[0],
      sampleSubstances.eulerCircuit1,
      grammars.simpleDirectedGraphGrammar
    ),
    style: simpleDirectedGraphStyle,
  },
  graph_2_0_0: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: simpleDirectedGraphDomain,
    sampleSubstance: sampleSubstances.eulerCircuit1,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a strongly connected graph?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      simpleDirectedGraphDomain,
      descriptionPreludes[0],
      `a strongly connected graph?`,
      finalInsts[0],
      sampleSubstances.eulerCircuit1
    ),
    style: simpleDirectedGraphStyle,
  },
  graph_2_0_1: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: simpleDirectedGraphDomain,
    sampleSubstance: sampleSubstances.eulerCircuit1,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a strongly connected graph with 5 nodes where each node is connected to every other node by an edge?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      simpleDirectedGraphDomain,
      descriptionPreludes[0],
      `a strongly connected graph with 5 nodes where each node is connected to every other node by an edge?`,
      finalInsts[0],
      sampleSubstances.eulerCircuit1
    ),
    style: simpleDirectedGraphStyle,
  },
  graph_2_1_0: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: simpleDirectedGraphDomain,
    bnf: grammars.simpleDirectedGraphGrammar,
    sampleSubstance: sampleSubstances.eulerCircuit1,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a strongly connected graph?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      simpleDirectedGraphDomain,
      descriptionPreludes[0],
      `a strongly connected graph?`,
      finalInsts[0],
      sampleSubstances.eulerCircuit1,
      grammars.simpleDirectedGraphGrammar
    ),
    style: simpleDirectedGraphStyle,
  },
  graph_2_1_1: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: simpleDirectedGraphDomain,
    bnf: grammars.simpleDirectedGraphGrammar,
    sampleSubstance: sampleSubstances.eulerCircuit1,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a strongly connected graph with 5 nodes where each node is connected to every other node by an edge?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      simpleDirectedGraphDomain,
      descriptionPreludes[0],
      `a strongly connected graph with 5 nodes where each node is connected to every other node by an edge?`,
      finalInsts[0],
      sampleSubstances.eulerCircuit1,
      grammars.simpleDirectedGraphGrammar
    ),
    style: simpleDirectedGraphStyle,
  },
  geometry_0_0_0: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: geometryDomain,
    sampleSubstance: sampleSubstances.geometry,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `two congruent triangles that demonstrate the SAS theorem?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      geometryDomain,
      descriptionPreludes[0],
      `two congruent triangles that demonstrate the SAS theorem?`,
      finalInsts[0],
      sampleSubstances.geometry
    ),
    style: euclideanStyle,
  },
  geometry_0_0_1: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: geometryDomain,
    sampleSubstance: sampleSubstances.geometry,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `two triangles ABC and DEF where AB is equal to DE, BC is equal to EF, and angle B is equal to angle E?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      geometryDomain,
      descriptionPreludes[0],
      `two triangles ABC and DEF where AB is equal to DE, BC is equal to EF, and angle B is equal to angle E?`,
      finalInsts[0],
      sampleSubstances.geometry
    ),
    style: euclideanStyle,
  },
  geometry_0_1_0: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: geometryDomain,
    bnf: grammars.geometryGrammar,
    sampleSubstance: sampleSubstances.geometry,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `two congruent triangles that demonstrate the SAS theorem?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      geometryDomain,
      descriptionPreludes[0],
      `two congruent triangles that demonstrate the SAS theorem?`,
      finalInsts[0],
      sampleSubstances.geometry,
      grammars.geometryGrammar
    ),
    style: euclideanStyle,
  },
  geometry_0_1_1: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: geometryDomain,
    bnf: grammars.geometryGrammar,
    sampleSubstance: sampleSubstances.geometry,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `two triangles ABC and DEF where AB is equal to DE, BC is equal to EF, and angle B is equal to angle E?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      geometryDomain,
      descriptionPreludes[0],
      `two triangles ABC and DEF where AB is equal to DE, BC is equal to EF, and angle B is equal to angle E?`,
      finalInsts[0],
      sampleSubstances.geometry,
      grammars.geometryGrammar
    ),
    style: euclideanStyle,
  },
  geometry_1_0_0: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: geometryDomain,
    sampleSubstance: sampleSubstances.geometry,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a pair of complementary angles?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      geometryDomain,
      descriptionPreludes[0],
      `a pair of complementary angles?`,
      finalInsts[0],
      sampleSubstances.geometry
    ),
    style: euclideanStyle,
  },
  geometry_1_0_1: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: geometryDomain,
    sampleSubstance: sampleSubstances.geometry,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a right angle ABC and a pair of angles ABD and DBC that add up to 90 degrees and are adjacent to each other?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      geometryDomain,
      descriptionPreludes[0],
      `a right angle ABC and a pair of angles ABD and DBC that add up to 90 degrees and are adjacent to each other?`,
      finalInsts[0],
      sampleSubstances.geometry
    ),
    style: euclideanStyle,
  },
  geometry_1_1_0: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: geometryDomain,
    bnf: grammars.geometryGrammar,
    sampleSubstance: sampleSubstances.geometry,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a pair of complementary angles?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      geometryDomain,
      descriptionPreludes[0],
      `a pair of complementary angles?`,
      finalInsts[0],
      sampleSubstances.geometry,
      grammars.geometryGrammar
    ),
    style: euclideanStyle,
  },
  geometry_1_1_1: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: geometryDomain,
    bnf: grammars.geometryGrammar,
    sampleSubstance: sampleSubstances.geometry,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a right angle ABC and a pair of angles ABD and DBC that add up to 90 degrees and are adjacent to each other?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      geometryDomain,
      descriptionPreludes[0],
      `a right angle ABC and a pair of angles ABD and DBC that add up to 90 degrees and are adjacent to each other?`,
      finalInsts[0],
      sampleSubstances.geometry,
      grammars.geometryGrammar
    ),
    style: euclideanStyle,
  },
  geometry_2_0_0: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: geometryDomain,
    sampleSubstance: sampleSubstances.geometry,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a square inscribed in a circle?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      geometryDomain,
      descriptionPreludes[0],
      `a square inscribed in a circle?`,
      finalInsts[0],
      sampleSubstances.geometry
    ),
    style: euclideanStyle,
  },
  geometry_2_0_1: {
    systemInst: systemInsts[0],
    penroseContext: penroseContexts[0],
    domain: geometryDomain,
    sampleSubstance: sampleSubstances.geometry,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a circle with four chords AB, BC, CD, and DA of equal length such that AB is parallel to CD and BC is parallel to DA to form a square?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[0],
      penroseContexts[0],
      geometryDomain,
      descriptionPreludes[0],
      `a circle with four chords AB, BC, CD, and DA of equal length such that AB is parallel to CD and BC is parallel to DA to form a square?`,
      finalInsts[0],
      sampleSubstances.geometry
    ),
    style: euclideanStyle,
  },
  geometry_2_1_0: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: geometryDomain,
    bnf: grammars.geometryGrammar,
    sampleSubstance: sampleSubstances.geometry,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a square inscribed in a circle?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      geometryDomain,
      descriptionPreludes[0],
      `a square inscribed in a circle?`,
      finalInsts[0],
      sampleSubstances.geometry,
      grammars.geometryGrammar
    ),
    style: euclideanStyle,
  },
  geometry_2_1_1: {
    systemInst: systemInsts[1],
    penroseContext: penroseContexts[0],
    domain: geometryDomain,
    bnf: grammars.geometryGrammar,
    sampleSubstance: sampleSubstances.geometry,
    descriptionPrelude: `${descriptionPreludes[0]} `,
    description: `a circle with four chords AB, BC, CD, and DA of equal length such that AB is parallel to CD and BC is parallel to DA to form a square?
    
    `,
    finalInst: finalInsts[0],
    prompt: generatePromptText(
      systemInsts[1],
      penroseContexts[0],
      geometryDomain,
      descriptionPreludes[0],
      `a circle with four chords AB, BC, CD, and DA of equal length such that AB is parallel to CD and BC is parallel to DA to form a square?`,
      finalInsts[0],
      sampleSubstances.geometry,
      grammars.geometryGrammar
    ),
    style: euclideanStyle,
  },
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
    //model: "gpt-3.5-turbo",
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
