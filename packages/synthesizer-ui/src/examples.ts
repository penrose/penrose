import { SynthesizerSetting } from "@penrose/core";

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

export const presets: PresetCollection = {
  c01p01: {
    displayName: "c01p01: Collinear Points",
    prompt: "In which of the following diagrams are points B, D, E collinear?",
    substance: examples["geometry-domain"].textbook_problems["c01p01.sub"],
    domain: examples["geometry-domain"]["geometry.dsl"],
    style: examples["geometry-domain"]["euclidean.sty"],
    setting: {
      mutationCount: [1, 4],
      argOption: "existing",
      argReuse: "distinct",
      weights: {
        type: 0.1,
        predicate: 0.3,
        constructor: 0.2,
      },
      add: {
        type: [],
        function: [],
        constructor: [],
        predicate: [],
      },
      delete: {
        type: [],
        function: [],
        constructor: [],
        predicate: ["Collinear", "In"],
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
      mutationCount: [1, 4],
      argOption: "existing",
      argReuse: "distinct",
      weights: {
        type: 0.1,
        predicate: 0.3,
        constructor: 0.2,
      },
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
      mutationCount: [1, 4],
      argOption: "existing",
      argReuse: "distinct",
      weights: {
        type: 0.1,
        predicate: 0.3,
        constructor: 0.2,
      },
      add: {
        type: [],
        function: [],
        constructor: [],
        predicate: [],
      },
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
      mutationCount: [1, 4],
      argOption: "existing",
      argReuse: "distinct",
      weights: {
        type: 0.1,
        predicate: 0.3,
        constructor: 0.2,
      },
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
      "In which of the following diagrams are triangles tDEC an tDEA congruent?",
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
      mutationCount: [1, 4],
      argOption: "existing",
      argReuse: "distinct",
      weights: {
        type: 0.1,
        predicate: 0.3,
        constructor: 0.2,
      },
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
};
