import euclideanStyle from "@penrose/examples/dist/geometry-domain/euclidean.style";
import geometryDomain from "@penrose/examples/dist/geometry-domain/geometry.domain";
import c01p01 from "@penrose/examples/dist/geometry-domain/textbook_problems/c01p01.substance";
import c01p10 from "@penrose/examples/dist/geometry-domain/textbook_problems/c01p10.substance";
import c02p01 from "@penrose/examples/dist/geometry-domain/textbook_problems/c02p01.substance";
import c03p01 from "@penrose/examples/dist/geometry-domain/textbook_problems/c03p01.substance";
import c04p01 from "@penrose/examples/dist/geometry-domain/textbook_problems/c04p01.substance";
import c04p12 from "@penrose/examples/dist/geometry-domain/textbook_problems/c04p12.substance";
import c05p01 from "@penrose/examples/dist/geometry-domain/textbook_problems/c05p01.substance";
import c05p13 from "@penrose/examples/dist/geometry-domain/textbook_problems/c05p13.substance";
import c06p06 from "@penrose/examples/dist/geometry-domain/textbook_problems/c06p06.substance";
import c07p06 from "@penrose/examples/dist/geometry-domain/textbook_problems/c07p06.substance";
import c07p10 from "@penrose/examples/dist/geometry-domain/textbook_problems/c07p10.substance";
import c07p22 from "@penrose/examples/dist/geometry-domain/textbook_problems/c07p22.substance";
import c08p08 from "@penrose/examples/dist/geometry-domain/textbook_problems/c08p08.substance";
import c10p08 from "@penrose/examples/dist/geometry-domain/textbook_problems/c10p08.substance";
import c11p07 from "@penrose/examples/dist/geometry-domain/textbook_problems/c11p07.substance";
import c11p25 from "@penrose/examples/dist/geometry-domain/textbook_problems/c11p25.substance";
import c12p20 from "@penrose/examples/dist/geometry-domain/textbook_problems/c12p20.substance";
import simpleDirectedGraphDomain from "@penrose/examples/dist/graph-domain/simple-directed-graph.domain";
import simpleDirectedGraphStyle from "@penrose/examples/dist/graph-domain/simple-directed-graph.style";
import simpleGraphDomain from "@penrose/examples/dist/graph-domain/simple-graph.domain";
import simpleGraphStyle from "@penrose/examples/dist/graph-domain/simple-graph.style";
import sec2eg9 from "@penrose/examples/dist/graph-domain/textbook/sec2/eg9.substance";
import sec2ex22 from "@penrose/examples/dist/graph-domain/textbook/sec2/ex22.substance";
import sec3ex50 from "@penrose/examples/dist/graph-domain/textbook/sec3/ex50.substance";
import sec4ex12b from "@penrose/examples/dist/graph-domain/textbook/sec4/ex12b.substance";
import sec5ex18 from "@penrose/examples/dist/graph-domain/textbook/sec5/ex18.substance";
import sec5ex21 from "@penrose/examples/dist/graph-domain/textbook/sec5/ex21.substance";
import sec5ex47d from "@penrose/examples/dist/graph-domain/textbook/sec5/ex47d.substance";
import hydrazine from "@penrose/examples/dist/molecules/hydrazine.substance";
import hydrogencyanide from "@penrose/examples/dist/molecules/hydrogencyanide.substance";
import lewisStyle from "@penrose/examples/dist/molecules/lewis.style";
import methane from "@penrose/examples/dist/molecules/methane.substance";
import moleculesDomain from "@penrose/examples/dist/molecules/molecules.domain";
import nitrogen from "@penrose/examples/dist/molecules/nitrogen.substance";
import phosgene from "@penrose/examples/dist/molecules/phosgene.substance";
import phosphorustrichloride from "@penrose/examples/dist/molecules/phosphorustrichloride.substance";
import xenontetroxide from "@penrose/examples/dist/molecules/xenontetroxide.substance";
import { SynthesizerSetting } from "./synthesis/Synthesizer.js";

export interface Preset {
  displayName: string;
  prompt: string;
  substance: string;
  domain: string;
  style: string;
  setting: SynthesizerSetting;
}
interface PresetCollection {
  [s: string]: Preset;
}

const lewisParams: SynthesizerSetting = {
  mutationCount: [1, 4],
  argOption: "existing",
  argReuse: "distinct",
  weights: {
    type: 0.15,
    predicate: 0.5,
    constructor: 0.35,
  },
  opWeights: {
    add: 0,
    delete: 0,
    edit: 1,
  },
  add: {
    type: "*",
    function: "*",
    constructor: "*",
    predicate: "*",
  },
  delete: {
    type: "*",
    function: "*",
    constructor: "*",
    predicate: "*",
  },
  edit: {
    type: "*",
    function: "*",
    constructor: "*",
    predicate: "*",
  },
};

const geometryParams: SynthesizerSetting = {
  mutationCount: [1, 4],
  argOption: "existing",
  argReuse: "distinct",
  weights: {
    type: 0.15,
    predicate: 0.5,
    constructor: 0.35,
  },
  opWeights: {
    add: 0,
    delete: 0.2,
    edit: 0.8,
  },
  // TODO: need weights for the three ops
  add: {
    type: "*",
    function: "*",
    constructor: "*",
    predicate: "*",
  },
  delete: {
    type: "*",
    function: "*",
    constructor: "*",
    predicate: "*",
  },
  edit: {
    type: "*",
    function: "*",
    constructor: "*",
    predicate: "*",
  },
};

const graphParams: SynthesizerSetting = {
  mutationCount: [1, 4],
  argOption: "existing",
  argReuse: "distinct",
  weights: {
    type: 0.15,
    predicate: 0.5,
    constructor: 0.35,
  },
  opWeights: {
    add: 0.5,
    delete: 0.4,
    edit: 0.1,
  },
  // TODO: need weights for the three ops
  add: {
    type: "*",
    function: "*",
    constructor: "*",
    predicate: "*",
  },
  delete: {
    type: "*",
    function: "*",
    constructor: "*",
    predicate: "*",
  },
  edit: {
    type: "*",
    function: "*",
    constructor: "*",
    predicate: "*",
  },
};

export const presets: PresetCollection = {
  c01p01: {
    displayName: "c01p01: Collinear Points",
    prompt:
      "In which of the following diagrams are points $B$, $D$, $E$ collinear?",
    substance: c01p01,
    domain: geometryDomain,
    style: euclideanStyle,
    setting: {
      ...geometryParams,
    },
  },
  c01p10: {
    displayName: "c01p10: Complementary angles",
    prompt:
      "Which of the following diagrams contains exactly 2 pairs of complementary angles?",
    substance: c01p10,
    domain: geometryDomain,
    style: euclideanStyle,
    setting: {
      ...geometryParams,
    },
  },
  c02p01: {
    displayName: "c02p01: Angle bisector",
    prompt:
      "In which of the following diagrams is the statement $\\angle ADC=2(m \\angle ADB)$ true?",
    substance: c02p01,
    domain: geometryDomain,
    style:
      euclideanStyle +
      `
  forall Angle a {
    override a.radius = 50
  }
    `,
    setting: {
      ...geometryParams,
    },
  },
  c03p01: {
    displayName: "c03p01: Alternate interior angles",
    prompt:
      "Which diagram illustrates $\\angle JKM$ and $\\angle KML$ as alternate interior angles?",
    substance: c03p01,
    domain: geometryDomain,
    style:
      euclideanStyle +
      `
forall Angle a
with Point p, q, r
where a := InteriorAngle(p, q, r) {
  a.color = #00000000
}
    `,
    setting: {
      ...geometryParams,
    },
  },
  c04p01: {
    displayName: "c04p01: Congruent triangles",
    prompt:
      "In which of the following diagrams are triangles $\\triangle DEC$ an $\\triangle DEA$ congruent?",
    substance: c04p01,
    domain: geometryDomain,
    style:
      euclideanStyle +
      `
forall Point \`D\`, \`E\`, \`C\` {
  Path {
    d: pathFromPoints("closed", [\`D\`.vec, \`E\`.vec, \`C\`.vec])
    fillColor: #FBB04080
  }
}
forall Point \`D\`, \`E\`, \`A\` {
  Path {
    d: pathFromPoints("closed", [\`D\`.vec, \`E\`.vec, \`A\`.vec])
    fillColor: #27AAE180
  }
}
    `,
    setting: {
      ...geometryParams,
    },
  },
  c04p12: {
    displayName: "c04p12: Complementary angles",
    prompt:
      "In which of these diagrams, can you find the value of $\\angle BCE$ given the value of $x$?",
    substance: c04p12,
    domain: geometryDomain,
    style: euclideanStyle,
    setting: {
      ...geometryParams,
    },
  },
  c05p01: {
    displayName: "c05p01: Mid-segement",
    prompt:
      "Which diagram shows that $HK$ is a midsegment of $\\triangle GJF$?",
    substance: c05p01,
    domain: geometryDomain,
    style: euclideanStyle,
    setting: {
      ...geometryParams,
    },
  },
  c05p13: {
    displayName: "c05p13: Incenter",
    prompt: "Which diagram shows $P$ as the incenter of $\\triangle JKL$?",
    substance: c05p13,
    domain: geometryDomain,
    style: euclideanStyle,
    setting: {
      ...geometryParams,
    },
  },
  c06p06: {
    displayName: "c06p06: Parallelogram",
    prompt:
      "Which of the following diagrams shows that $JKLM$ is a parallelogram?",
    substance: c06p06,
    domain: geometryDomain,
    style: euclideanStyle,
    setting: {
      ...geometryParams,
    },
  },
  c07p06: {
    displayName: "c07p06: Parallelogram 2",
    prompt: "In which of the following diagrams is $ABCD$ a parallelogram?",
    substance: c07p06,
    domain: geometryDomain,
    style: euclideanStyle,
    setting: {
      ...geometryParams,
    },
  },
  c07p10: {
    displayName: "c07p10: Orthocenter",
    prompt:
      "In which of the following diagrams is $G$ the orthocenter of $\\triangle FGH$?",
    substance: c07p10,
    domain: geometryDomain,
    style: euclideanStyle,
    setting: {
      ...geometryParams,
    },
  },
  c07p22: {
    displayName: "c07p22: Similar triangles",
    prompt: "In which of the following diagrams are the two triangles similar?",
    substance: c07p22,
    domain: geometryDomain,
    style: euclideanStyle,
    setting: {
      ...geometryParams,
    },
  },
  c08p08: {
    displayName: "c08p08: Nested Right Angle Triangles",
    prompt:
      "In which of the following diagrams is $\\triangle PQR$ similar to $\\triangle TSR$?",
    substance: c08p08,
    domain: geometryDomain,
    style: euclideanStyle,
    setting: {
      ...geometryParams,
    },
  },
  c10p08: {
    displayName: "c10p08: Triangle Congruency",
    prompt:
      "In which of the following diagrams is $\\triangle DEF$ congruent to $\\triangle ABC$?",
    substance: c10p08,
    domain: geometryDomain,
    style: euclideanStyle,
    setting: {
      ...geometryParams,
    },
  },
  c11p07: {
    displayName: "c11p07: Circle with Secant",
    prompt:
      "Which of the following diagrams is the length of $PS$ represented by $\\frac{PQ \\times PR}{PU}$?",
    substance: c11p07,
    domain: geometryDomain,
    style: euclideanStyle,
    setting: {
      ...geometryParams,
    },
  },
  c11p25: {
    displayName: "c11p25: Trapezoid inside Circle",
    prompt: "In which of the following diagrams are $AD = BC$?",
    substance: c11p25,
    domain: geometryDomain,
    style: euclideanStyle,
    setting: {
      ...geometryParams,
    },
  },
  c12p20: {
    displayName: "c12p20: Triangle Congruency",
    prompt:
      "In which of the following diagrams is $\\triangle ABC$ congruent to $\\triangle BCD$?",
    substance: c12p20,
    domain: geometryDomain,
    style: euclideanStyle,
    setting: {
      ...geometryParams,
    },
  },
  lewis_0: {
    displayName: "lewis_0: Hydrogen Cyanide",
    prompt: "Choose the correct Lewis structure for $\\mathrm{HCN}$.",
    substance: hydrogencyanide,
    domain: moleculesDomain,
    style: lewisStyle,
    setting: {
      ...lewisParams,
    },
  },
  lewis_1: {
    displayName: "lewis_1: Methane",
    prompt: "Choose the correct Lewis structure for $\\mathrm{CH_4}$.",
    substance: methane,
    domain: moleculesDomain,
    style: lewisStyle,
    setting: {
      ...lewisParams,
    },
  },
  lewis_2: {
    displayName: "lewis_2: Phosphorus Trichoride",
    prompt: "Choose the correct Lewis structure for $\\mathrm{PCl_3}$.",
    substance: phosphorustrichloride,
    domain: moleculesDomain,
    style: lewisStyle,
    setting: {
      ...lewisParams,
    },
  },
  lewis_3: {
    displayName: "lewis_3: Xenon Tetroxide",
    prompt: "Choose the correct Lewis structure for $\\mathrm{XeO_4}$.",
    substance: xenontetroxide,
    domain: moleculesDomain,
    style: lewisStyle,
    setting: {
      ...lewisParams,
    },
  },
  lewis_4: {
    displayName: "lewis_4: Phosgene",
    prompt: "Choose the correct Lewis structure for $\\mathrm{COCl_2}$.",
    substance: phosgene,
    domain: moleculesDomain,
    style: lewisStyle,
    setting: {
      ...lewisParams,
    },
  },
  lewis_5: {
    displayName: "lewis_5: Nitrogen",
    prompt: "Choose the correct Lewis structure for $\\mathrm{N_2}$.",
    substance: nitrogen,
    domain: moleculesDomain,
    style: lewisStyle,
    setting: {
      ...lewisParams,
    },
  },
  lewis_6: {
    displayName: "lewis_6: Hydrazine",
    prompt: "Choose the correct Lewis structure for $\\mathrm{N_2H_4}$.",
    substance: hydrazine,
    domain: moleculesDomain,
    style: lewisStyle,
    setting: {
      ...lewisParams,
    },
  },
  graph_0: {
    displayName: "graph_0: Bipartite Graph",
    prompt: "Which of the following diagrams are bipartite graphs?",
    substance: sec2ex22,
    domain: simpleGraphDomain,
    style: simpleGraphStyle,
    setting: {
      ...graphParams,
    },
  },
  graph_1: {
    displayName: "graph_1: Self-complementary Graph",
    prompt: "Which of the following diagrams are self-complementary graphs?",
    substance: sec3ex50,
    domain: simpleGraphDomain,
    style: simpleGraphStyle,
    setting: {
      ...graphParams,
    },
  },
  graph_2: {
    displayName: "graph_2: Euler Circuit 1",
    prompt: "Which diagram has an Euler circuit?",
    substance: sec5ex21,
    domain: simpleDirectedGraphDomain,
    style: simpleDirectedGraphStyle,
    setting: {
      ...graphParams,
    },
  },
  graph_3: {
    displayName: "graph_3: Euler Circuit 2",
    prompt: "Which diagram has an Euler circuit?",
    substance: sec5ex18,
    domain: simpleDirectedGraphDomain,
    style: simpleDirectedGraphStyle,
    setting: {
      ...graphParams,
    },
  },
  graph_4: {
    displayName: "graph_4: Bipartite Graph 2",
    prompt: "Which of the following diagrams are bipartite graphs?",
    substance: sec2eg9,
    domain: simpleGraphDomain,
    style: simpleGraphStyle,
    setting: {
      ...graphParams,
    },
  },
  graph_5: {
    displayName: "graph_5: Strongly Connected Graph",
    prompt: "Which of the following diagrams are strongly connected graphs?",
    substance: sec4ex12b,
    domain: simpleDirectedGraphDomain,
    style: simpleDirectedGraphStyle,
    setting: {
      ...graphParams,
    },
  },
  graph_6: {
    displayName: "graph_6: Hamilton Circuit",
    prompt: "Which diagram has a Hamilton circuit?",
    substance: sec5ex47d,
    domain: simpleGraphDomain,
    style: simpleGraphStyle,
    setting: {
      ...graphParams,
    },
  },
};
