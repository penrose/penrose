import { presets } from "../examples";
import { multipleChoiceProblem } from "./util";

const problems = [
  multipleChoiceProblem(presets["lewis_0"], "test0", 10, {
    correct: [0],
    incorrect: [1, 6, 8],
  }),
  multipleChoiceProblem(presets["lewis_1"], "test0", 10, {
    correct: [0],
    incorrect: [3, 4, 5],
  }),
  multipleChoiceProblem(presets["lewis_2"], "test0", 10, {
    correct: [0],
    incorrect: [2, 5, 7],
  }),
  multipleChoiceProblem(presets["lewis_3"], "test0", 10, {
    correct: [0],
    incorrect: [1, 3, 5],
  }),
  multipleChoiceProblem(presets["lewis_4"], "test0", 10, {
    correct: [0],
    incorrect: [2, 6, 7],
  }),
  multipleChoiceProblem(presets["lewis_5"], "test0", 10, {
    correct: [0],
    incorrect: [2, 3, 4],
  }),
  multipleChoiceProblem(presets["lewis_6"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 3],
  }),
  multipleChoiceProblem(presets["c01p01"], "test0", 10, {
    correct: [0],
    incorrect: [1, 4, 6],
  }),
  multipleChoiceProblem(presets["c01p10"], "test0", 10, {
    correct: [0],
    incorrect: [7, 2, 3],
  }),
  multipleChoiceProblem(presets["c02p01"], "test0", 10, {
    correct: [0],
    incorrect: [4, 2, 3],
  }),
  multipleChoiceProblem(presets["c03p01"], "test0", 10, {
    correct: [0],
    incorrect: [1, 5, 3],
  }),
  multipleChoiceProblem(presets["c04p01"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 4],
  }),
  multipleChoiceProblem(presets["c04p12"], "test0", 10, {
    correct: [0, 4, 5],
    incorrect: [1],
  }),
  multipleChoiceProblem(presets["c05p01"], "test0", 10, {
    correct: [0, 1, 5, 7],
    incorrect: [],
  }),

  multipleChoiceProblem(presets["c05p13"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 7],
  }),
  multipleChoiceProblem(presets["c06p06"], "test0", 10, {
    correct: [0, 1, 2],
    incorrect: [10],
  }),
  multipleChoiceProblem(presets["c07p06"], "test0", 10, {
    correct: [0, 3],
    incorrect: [6, 10],
  }),
  multipleChoiceProblem(presets["c07p10"], "test0", 10, {
    correct: [0, 1, 2, 3],
    incorrect: [],
  }),
  multipleChoiceProblem(presets["c07p22"], "test0", 10, {
    correct: [0],
    incorrect: [5, 2, 3],
  }),
  multipleChoiceProblem(presets["c08p08"], "test0", 10, {
    correct: [0],
    incorrect: [8, 9, 3],
  }),
  multipleChoiceProblem(presets["c10p08"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 5],
  }),
  multipleChoiceProblem(presets["c11p07"], "test0", 10, {
    correct: [0],
    incorrect: [6, 7, 3],
  }),
  multipleChoiceProblem(presets["c11p25"], "test0", 10, {
    correct: [0, 1],
    incorrect: [6, 8],
  }),
  multipleChoiceProblem(presets["c12p20"], "test0", 10, {
    correct: [0],
    incorrect: [6, 7, 8],
  }),
  multipleChoiceProblem(presets["graph_0"], "test0", 10, {
    correct: [0, 8],
    incorrect: [1, 5],
  }),
  multipleChoiceProblem(presets["graph_1"], "test0", 10, {
    correct: [0],
    incorrect: [4, 2, 7],
  }),
  multipleChoiceProblem(presets["graph_2"], "test0", 10, {
    correct: [2],
    incorrect: [4, 3, 7],
  }),
  multipleChoiceProblem(presets["graph_3"], "test0", 10, {
    correct: [3],
    incorrect: [1, 4, 5],
  }),
  multipleChoiceProblem(presets["graph_4"], "test0", 10, {
    correct: [1, 3, 4, 7],
    incorrect: [],
  }),
  multipleChoiceProblem(presets["graph_5"], "test0", 10, {
    correct: [0, 1],
    incorrect: [3, 5],
  }),
  multipleChoiceProblem(presets["graph_6"], "test0", 10, {
    correct: [0],
    incorrect: [2, 4, 5],
  }),
];
export default function () {
  return <div>{problems}</div>;
}
