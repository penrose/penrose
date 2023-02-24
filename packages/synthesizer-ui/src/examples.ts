import { SynthesizerSetting } from "@penrose/core";
import geometryDomain from "@penrose/examples/dist/geometry-domain";
import molecules from "@penrose/examples/dist/molecules";

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
    add: 0.01,
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

export const presets: PresetCollection = {
  c01p01: {
    displayName: "c01p01: Collinear Points",
    prompt: "In which of the following diagrams are points B, D, E collinear?",
    substance: geometryDomain.textbook_problems["c01p01.substance"],
    domain: geometryDomain["geometry.domain"],
    style: geometryDomain["euclidean.style"],
    setting: {
      ...defaultParams,
    },
  },
  c01p10: {
    displayName: "c01p10: Complementary angles",
    prompt:
      "Which of the following diagrams contains exactly 2 pairs of complementary angles?",
    substance: geometryDomain.textbook_problems["c01p10.substance"],
    domain: geometryDomain["geometry.domain"],
    style: geometryDomain["euclidean.style"],
    setting: {
      ...defaultParams,
    },
  },
  c02p01: {
    displayName: "c02p01: Angle bisector",
    prompt:
      "In which of the following diagrams is the statement, <ADC=2(m<ADB)?",
    substance: geometryDomain.textbook_problems["c02p01.substance"],
    domain: geometryDomain["geometry.domain"],
    style:
      geometryDomain["euclidean.style"] +
      `
  forall Angle a {
    override a.radius = 50
  }
    `,
    setting: {
      ...defaultParams,
    },
  },
  c03p01: {
    displayName: "c03p01: Alternate interior angles",
    prompt:
      "Which diagram illustrates <JKM and <KML as alternate interior angles?",
    substance: geometryDomain.textbook_problems["c03p01.substance"],
    domain: geometryDomain["geometry.domain"],
    style:
      geometryDomain["euclidean.style"] +
      `
forall Angle a
with Point p, q, r
where a := InteriorAngle(p, q, r) {
  a.color = #00000000
}
    `,
    setting: {
      ...defaultParams,
    },
  },
  c04p01: {
    displayName: "c04p01: Congruent triangles",
    prompt:
      "In which of the following diagrams are triangles tDEC an tDEA congruent?",
    substance: geometryDomain.textbook_problems["c04p01.substance"],
    domain: geometryDomain["geometry.domain"],
    style:
      geometryDomain["euclidean.style"] +
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
    },
  },
  c04p12: {
    displayName: "c04p12: Complementary angles",
    prompt:
      "In which of these diagrams, can you find the value of <BCE given the value of x?",
    substance: geometryDomain.textbook_problems["c04p12.substance"],
    domain: geometryDomain["geometry.domain"],
    style: geometryDomain["euclidean.style"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c05p01: {
    displayName: "c05p01: Mid-segement",
    prompt: "Which diagram shows that HK is a midsegment of tGJF?",
    substance: geometryDomain.textbook_problems["c05p01.substance"],
    domain: geometryDomain["geometry.domain"],
    style: geometryDomain["euclidean.style"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c05p13: {
    displayName: "c05p13: Incenter",
    prompt: "Which diagram shows P as the incenter of tJKL?",
    substance: geometryDomain.textbook_problems["c05p13.substance"],
    domain: geometryDomain["geometry.domain"],
    style: geometryDomain["euclidean.style"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c06p06: {
    displayName: "c06p06: Parallelogram",
    prompt:
      "Which of the following diagrams shows that JKLM is a parallelogram?",
    substance: geometryDomain.textbook_problems["c06p06.substance"],
    domain: geometryDomain["geometry.domain"],
    style: geometryDomain["euclidean.style"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c07p06: {
    displayName: "c07p06: Parallelogram 2",
    prompt: "In which of the following diagrams is ABCD a parallelogram?",
    substance: geometryDomain.textbook_problems["c07p06.substance"],
    domain: geometryDomain["geometry.domain"],
    style: geometryDomain["euclidean.style"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c07p10: {
    displayName: "c07p10: Orthocenter",
    prompt: "In which of the following diagrams is G the orthocenter of tFGH?",
    substance: geometryDomain.textbook_problems["c07p10.substance"],
    domain: geometryDomain["geometry.domain"],
    style: geometryDomain["euclidean.style"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c07p22: {
    displayName: "c07p22: Similar triangles",
    prompt: "In which of the following diagrams are the two triangles similar?",
    substance: geometryDomain.textbook_problems["c07p22.substance"],
    domain: geometryDomain["geometry.domain"],
    style: geometryDomain["euclidean.style"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c08p08: {
    displayName: "c08p08: Nested Right Angle Triangles",
    prompt: "In which of the following diagrams is PQR similar to TSR?",
    substance: geometryDomain.textbook_problems["c08p08.substance"],
    domain: geometryDomain["geometry.domain"],
    style: geometryDomain["euclidean.style"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  // NOTE: deprecated due to lack of conceptual prompt
  // c08p18: {
  //   displayName: "c08p18: Measures of Triangle",
  //   prompt: "TODO",
  //   substance: geometryDomain.textbook_problems["c08p18.substance"],
  //   domain: geometryDomain["geometry.domain"],
  //   style: geometryDomain["euclidean.style"] + ``,
  //   setting: {
  //     ...defaultParams,
  //   },
  // },

  // NOTE: deprecated due to lack of conceptual prompt
  // c09p02: {
  //   displayName: "c09p02: Perpendicular Bisector of Triangle",
  //   prompt: "TODO",
  //   substance: geometryDomain.textbook_problems["c09p02.substance"],
  //   domain: geometryDomain["geometry.domain"],
  //   style: geometryDomain["euclidean.style"] + ``,
  //   setting: {
  //     ...defaultParams,
  //   },
  // },
  c10p08: {
    displayName: "c10p08: Triangle Congruency",
    prompt: "In which of the following diagrams is tDEF congruent to tABC?",
    substance: geometryDomain.textbook_problems["c10p08.substance"],
    domain: geometryDomain["geometry.domain"],
    style: geometryDomain["euclidean.style"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c11p07: {
    displayName: "c11p07: Circle with Secant",
    prompt:
      "Which of the following diagrams is the length of PS represented by PQ*PR/PU?",
    substance: geometryDomain.textbook_problems["c11p07.substance"],
    domain: geometryDomain["geometry.domain"],
    style: geometryDomain["euclidean.style"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  c11p12: {
    displayName: "c11p12: Circle with Chords, Radii, Diameters",
    prompt: "Which of the line segments is NOT a chord of F? (TODO: reword)",
    substance: geometryDomain.textbook_problems["c11p12.substance"],
    domain: geometryDomain["geometry.domain"],
    style: geometryDomain["euclidean.style"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  // NOTE: deprecated due to lack of conceptual prompt
  // c11p21: {
  //   displayName: "c11p21: Chord Intersection",
  //   prompt: "TODO",
  //   substance: geometryDomain.textbook_problems["c11p21.substance"],
  //   domain: geometryDomain["geometry.domain"],
  //   style: geometryDomain["euclidean.style"] + ``,
  //   setting: {
  //     ...defaultParams,
  //   },
  // },
  c11p25: {
    displayName: "c11p25: Trapezoid inside Circle",
    prompt: "In which of the following diagrams are AD = BC?",
    substance: geometryDomain.textbook_problems["c11p25.substance"],
    domain: geometryDomain["geometry.domain"],
    style: geometryDomain["euclidean.style"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  // NOTE: deprecated due to lack of conceptual prompt
  // c12p12: {
  //   displayName: "c12p12: Circle with Chords, Radii",
  //   prompt: "TODO",
  //   substance: geometryDomain.textbook_problems["c12p12.substance"],
  //   domain: geometryDomain["geometry.domain"],
  //   style: geometryDomain["euclidean.style"] + ``,
  //   setting: {
  //     ...defaultParams,
  //   },
  // },
  c12p20: {
    displayName: "c12p20: Triangle Congruency",
    prompt: "In which of the following diagrams is tABC congruent to tBCD?",
    substance: geometryDomain.textbook_problems["c12p20.substance"],
    domain: geometryDomain["geometry.domain"],
    style: geometryDomain["euclidean.style"] + ``,
    setting: {
      ...defaultParams,
    },
  },
  lewis_0: {
    displayName: "lewis_0: Hydrogen Cyanide",
    prompt: "Choose the correct Lewis structure for HCN.",
    substance: molecules["hydrogencyanide.substance"],
    domain: molecules["molecules.domain"],
    style: molecules["lewis.style"],
    setting: {
      ...defaultParams,
      mutationCount: [15, 30],
      opWeights: {
        add: 0.5,
        delete: 0.2,
        edit: 1.5,
      },
      add: {
        type: ["Electron"],
        function: "*",
        constructor: ["ValanceElectron"],
        predicate: "*",
      },
      edit: {
        type: ["Bond"],
        constructor: "*",
        function: [],
        predicate: [],
      },
      delete: {
        type: ["Electron"],
        function: [],
        predicate: [],
        constructor: [],
      },
    },
  },
  lewis_1: {
    displayName: "lewis_1: Methane",
    prompt: "Choose the correct Lewis structure for CH4.",
    substance: molecules["methane.substance"],
    domain: molecules["molecules.domain"],
    style: molecules["lewis.style"],
    setting: {
      ...defaultParams,
      mutationCount: [5, 10],
      opWeights: {
        add: 1,
        delete: 0,
        edit: 1.5,
      },
      add: {
        type: ["Electron"],
        function: [],
        constructor: [
          "ValanceElectron",
          // "MakeSingleBond",
          // "MakeDoubleBond",
          // "MakeTripleBond",
        ],
        predicate: [],
      },
      edit: {
        type: ["Bond"],
        constructor: ["MakeSingleBond", "MakeDoubleBond", "MakeTripleBond"],
        function: [],
        predicate: [],
      },
      delete: {
        type: ["Electron"],
        constructor: [],
        predicate: [],
        function: [],
      },
    },
  },
  lewis_2: {
    displayName: "lewis_2: Phosphorus Trichoride",
    prompt: "Choose the correct Lewis structure for PCl3.",
    substance: molecules["phosphorustrichloride.substance"],
    domain: molecules["molecules.domain"],
    style: molecules["lewis.style"],
    setting: {
      ...defaultParams,
      mutationCount: [10, 20],
      opWeights: {
        add: 0.2,
        delete: 0.2,
        edit: 2,
      },
      add: {
        type: ["Electron"],
        function: [],
        constructor: [
          "ValanceElectron",
          // "MakeSingleBond",
          // "MakeDoubleBond",
          // "MakeTripleBond",
        ],
        predicate: [],
      },
      edit: {
        type: ["Bond"],
        constructor: ["MakeSingleBond", "MakeDoubleBond", "MakeTripleBond"],
        function: [],
        predicate: [],
      },
      delete: {
        type: ["Electron"],
        constructor: [],
        predicate: [],
        function: [],
      },
    },
  },
};
