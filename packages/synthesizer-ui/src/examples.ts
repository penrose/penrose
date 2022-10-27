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
  c04p01: {
    displayName: "c04p01: Congruent triangles",
    prompt:
      "Which of the following diagrams contains exactly 2 pairs of complementary angles?",
    substance: examples["geometry-domain"].textbook_problems["c04p01.sub"],
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
