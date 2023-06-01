import { presets } from "../examples.js";
import { multipleChoiceProblem } from "./util.js";

const problems = [
  multipleChoiceProblem(presets["c01p01"], "test0", 10, {
    correct: [0],
    incorrect: [2, 7, 8],
  }),
  multipleChoiceProblem(presets["c01p10"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 3],
  }),
  multipleChoiceProblem(presets["c02p01"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 3],
  }),
  multipleChoiceProblem(presets["c03p01"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 3],
  }),
  multipleChoiceProblem(presets["c04p01"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 3],
  }),
  multipleChoiceProblem(presets["c04p12"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 3],
  }),
  multipleChoiceProblem(presets["c05p01"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 3],
  }),

  multipleChoiceProblem(presets["c05p13"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 3],
  }),
  multipleChoiceProblem(presets["c06p06"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 3],
  }),
  multipleChoiceProblem(presets["c07p10"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 3],
  }),
  multipleChoiceProblem(presets["c07p22"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 3],
  }),
  multipleChoiceProblem(presets["c08p08"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 3],
  }),
  multipleChoiceProblem(presets["c11p07"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 3],
  }),
];
export default function () {
  return <div>{problems}</div>;
}
