import { SynthesizerSetting } from "synthesis/Synthesizer";

const setting: SynthesizerSetting = {
  mutationCount: [1, 4],
  argOption: "existing",
  argReuse: "distinct",
  weights: {
    type: 0.1,
    predicate: 0.3,
    constructor: 0.0,
  },
  add: {
    // type: "*",
    type: [],
    function: [],
    constructor: [],
    // constructor: "*",
    predicate: ["Equal"],
    // predicate: [],
  },
  delete: {
    type: [],
    function: [],
    constructor: [],
    predicate: ["IsSubset"],
  },
  edit: {
    type: [],
    function: [],
    constructor: [],
    predicate: ["IsSubset"],
    // predicate: [],
  },
};

describe("Synthesizer tests", () => {
  test("placeholder", () => {
    expect(true).toEqual(true);
  });
});
