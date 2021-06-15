import { compileDomain, compileSubstance, showError } from "index";
import { Synthesizer, SynthesizerSetting } from "synthesis/Synthesizer";

const SEED = "testSearch";

const settings: SynthesizerSetting = {
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

const domainSrc = `
type Set
type Point
type Map

constructor Singleton : Point p -> Set

function Intersection : Set a * Set b -> Set
function Union : Set a * Set b -> Set
function Subtraction : Set a * Set b -> Set
function CartesianProduct : Set a * Set b -> Set
function Difference : Set a * Set b -> Set
function Subset : Set a * Set b -> Set
function AddPoint : Point p * Set s1 -> Set

predicate Not : Prop p1
predicate From : Map f * Set domain * Set codomain
predicate Empty : Set s
predicate Intersecting : Set s1 * Set s2
predicate IsSubset : Set s1 * Set s2
predicate Equal : Set s1 * Set s2
predicate PointIn : Set s * Point p
predicate In : Point p * Set s
predicate Injection : Map m
predicate Surjection : Map m
predicate Bijection : Map m
predicate PairIn : Point * Point * Map
`;

const substanceSrc = `
Set A, B, C
IsSubset(B, A)
IsSubset(C, A)
`;

const initSynth = (
  domainSrc: string,
  substanceSrc: string,
  settings: SynthesizerSetting
): Synthesizer => {
  const envOrError = compileDomain(domainSrc);
  if (envOrError.isOk()) {
    const env = envOrError.value;
    const subRes = compileSubstance(substanceSrc, env);
    if (subRes.isOk()) {
      const subResult = subRes.value;
      return new Synthesizer(env, settings, subResult, SEED);
    } else {
      throw new Error(
        `Error when compiling the template Substance program: ${showError(
          subRes.error
        )}`
      );
    }
  } else {
    throw new Error(
      `Error when compiling the domain program:\n${showError(envOrError.error)}`
    );
  }
};

describe("Synthesizer tests", () => {
  test("next config", () => {
    const synth: Synthesizer = initSynth(domainSrc, substanceSrc, settings);
    synth.generateSubstances(10);
  });
});
