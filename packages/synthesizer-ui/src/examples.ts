import { SynthesizerSetting } from "@penrose/core";
import { examples } from "@penrose/examples";

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

const defaultParams: SynthesizerSetting = {
  mutationCount: [1, 4],
  argOption: "existing",
  argReuse: "distinct",
  weights: {
    type: 0.1,
    predicate: 0.3,
    constructor: 0.2,
  },
  opWeights: {
    add: 0.2,
    delete: 0.3,
    edit: 0.5,
  },
  // TODO: need weights for the three ops
  add: {
    type: [],
    function: [],
    constructor: [],
    predicate: [],
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
    prompt: "In which of the following diagrams are points B, D, E collinear?",
    substance: examples["geometry-domain"].textbook_problems["c01p01.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"],
    setting: {
      ...defaultParams,
      delete: {
        type: [],
        function: [],
        constructor: [],
        predicate: ["Collinear"],
      },
      edit: {
        type: [],
        function: [],
        constructor: ["Ray", "Line"],
        predicate: ["Collinear"],
      },
    },
  },
  c01p10: {
    displayName: "c01p10: Complementary angles",
    prompt:
      "Which of the following diagrams contains exactly 2 pairs of complementary angles?",
    substance: examples["geometry-domain"].textbook_problems["c01p10.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"],
    setting: {
      ...defaultParams,
      add: {
        type: [],
        function: [],
        constructor: ["MkTriangle"],
        predicate: [],
      },
      delete: {
        type: [],
        function: [],
        constructor: ["MkSegment"],
        predicate: ["Collinear", "In"],
      },
      edit: {
        type: [],
        function: [],
        constructor: ["Ray", "Line"],
        predicate: ["RightMarked"],
      },
    },
  },
  c02p01: {
    displayName: "c02p01: Angle bisector",
    prompt:
      "In which of the following diagrams is the statement, <ADC=2(m<ADB)?",
    substance: examples["geometry-domain"].textbook_problems["c02p01.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style:
      examples["geometry-domain"]["euclidean.sty"] +
      `
  forall Angle a {
    override a.radius = 50
  }
    `,
    setting: {
      ...defaultParams,
      delete: {
        type: [],
        function: [],
        constructor: [],
        predicate: ["EqualAngleMarker", "AngleBisector"],
      },
      edit: {
        type: [],
        function: [],
        constructor: [],
        predicate: [],
      },
    },
  },
  c03p01: {
    displayName: "c03p01: Alternate interior angles",
    prompt:
      "Which diagram illustrates <JKM and <KML as alternate interior angles?",
    substance: examples["geometry-domain"].textbook_problems["c03p01.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style:
      examples["geometry-domain"]["euclidean.sty"] +
      `
forall Angle a
with Point p, q, r
where a := InteriorAngle(p, q, r) {
  a.color = #00000000
}
    `,
    setting: {
      ...defaultParams,
      add: {
        type: [],
        function: [],
        constructor: ["InteriorAngle"],
        predicate: ["Obtuse", "RightMarked"],
      },
      delete: {
        type: [],
        function: [],
        constructor: [],
        predicate: ["Parallel", "ParallelMarker1"],
      },
      edit: {
        type: [],
        function: [],
        constructor: [],
        predicate: ["Acute"],
      },
    },
  },
  c04p01: {
    displayName: "c04p01: Congruent triangles",
    prompt:
      "Which of the following diagrams contains exactly 2 pairs of complementary angles?",
    substance: examples["geometry-domain"].textbook_problems["c04p01.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style:
      examples["geometry-domain"]["euclidean.sty"] +
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
      ...defaultParams,
      add: {
        type: [],
        function: [],
        constructor: [],
        predicate: ["RightUnmarked", "RightMarked", "EqualLengthMarker"],
      },
      delete: {
        type: [],
        function: [],
        constructor: [],
        predicate: ["RightMarked", "EqualLengthMarker", "EqualLength"],
      },
      edit: {
        type: [],
        function: [],
        constructor: ["InteriorAngle", "MkSegment"],
        predicate: [
          "RightMarked",
          "Collinear",
          "EqualLengthMarker",
          "EqualLength",
        ],
      },
    },
  },
  c04p12: {
    displayName: "c04p12: Complementary angles",
    prompt:
      "In which of these diagrams, can you find the value of <BCE given the value of x?",
    substance: examples["geometry-domain"].textbook_problems["c04p12.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c05p01: {
    displayName: "c05p01: Mid-segement",
    prompt: "Which diagram shows that HK is a midsegment of tGJF?",
    substance: examples["geometry-domain"].textbook_problems["c05p01.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c05p13: {
    displayName: "c05p13: Incenter",
    prompt: "Which diagram shows P as the incenter of tJKL?",
    substance: examples["geometry-domain"].textbook_problems["c05p13.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c06p06: {
    displayName: "c06p06: Parallelogram",
    prompt:
      "Which of the following diagrams shows that JKLM is a parallelogram?",
    substance: examples["geometry-domain"].textbook_problems["c06p06.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c07p06: {
    displayName: "c07p06: Parallelogram 2",
    prompt: "In which of the following diagrams is ABCD a parallelogram?",
    substance: examples["geometry-domain"].textbook_problems["c07p06.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c07p10: {
    displayName: "c07p10: Orthocenter",
    prompt: "In which of the following diagrams is G the orthocenter of tFGH?",
    substance: examples["geometry-domain"].textbook_problems["c07p10.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c07p22: {
    displayName: "c07p22: Similar triangles",
    prompt: "In which of the following diagrams are the two triangles similar?",
    substance: examples["geometry-domain"].textbook_problems["c07p22.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c08p08: {
    displayName: "c08p08: Nested Right Angle Triangles",
    prompt: "In which of the following diagrams is PQR similar to TSR?",
    substance: examples["geometry-domain"].textbook_problems["c08p08.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c08p18: {
    displayName: "c08p18: Measures of Triangle",
    prompt: "TODO",
    substance: examples["geometry-domain"].textbook_problems["c08p18.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c09p02: {
    displayName: "c09p02: Perpendicular Bisector of Triangle",
    prompt: "TODO",
    substance: examples["geometry-domain"].textbook_problems["c09p02.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c10p08: {
    displayName: "c10p08: Triangle Congruency",
    prompt: "In which of the following diagrams is tDEF congruent to tABC?",
    substance: examples["geometry-domain"].textbook_problems["c10p08.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c11p07: {
    displayName: "c11p07: Circle with Secant",
    prompt:
      "Which of the following diagrams is the length of PS represented by PQ*PR/PU?",
    substance: examples["geometry-domain"].textbook_problems["c11p07.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c11p12: {
    displayName: "c11p12: Circle with Chords, Radii, Diameters",
    prompt: "Which of the line segments is NOT a chord of F? (TODO: reword)",
    substance: examples["geometry-domain"].textbook_problems["c11p12.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c11p21: {
    displayName: "c11p21: Chord Intersection",
    prompt: "TODO",
    substance: examples["geometry-domain"].textbook_problems["c11p21.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c11p25: {
    displayName: "c11p25: Trapezoid inside Circle",
    prompt: "TODO",
    substance: examples["geometry-domain"].textbook_problems["c11p25.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c12p12: {
    displayName: "c12p12: Circle with Chords, Radii",
    prompt: "TODO",
    substance: examples["geometry-domain"].textbook_problems["c12p12.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c12p20: {
    displayName: "c12p20: Triangle Congruency",
    prompt: "TODO",
    substance: examples["geometry-domain"].textbook_problems["c12p20.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"] + ``,
    setting: {
      ...defaultParams,
    },
  },
};
