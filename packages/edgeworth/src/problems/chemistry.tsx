import { presets } from "../examples";
import { multipleChoiceProblem } from "./util";

const problems = [
  multipleChoiceProblem(presets["lewis_0"], "test0", 10, {
    correct: [0],
    incorrect: [1, 6, 8],
  }),
  multipleChoiceProblem(presets["lewis_1"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 3],
  }),
  multipleChoiceProblem(presets["lewis_2"], "test0", 10, {
    correct: [0],
    incorrect: [1, 3, 7],
  }),
  multipleChoiceProblem(presets["lewis_3"], "test0", 10, {
    correct: [0],
    incorrect: [3, 6, 7],
  }),
  multipleChoiceProblem(presets["lewis_4"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 3],
  }),
  multipleChoiceProblem(presets["lewis_5"], "test0", 10, {
    correct: [0],
    incorrect: [2, 3, 4],
  }),
  multipleChoiceProblem(presets["lewis_6"], "test0", 10, {
    correct: [0],
    incorrect: [1, 6, 10],
  }),
  multipleChoiceProblem(presets["graph_0"], "test0", 10, {
    correct: [0],
    incorrect: [1, 6, 8],
  }),
  multipleChoiceProblem(presets["graph_1"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 3],
  }),
  multipleChoiceProblem(presets["graph_2"], "test0", 10, {
    correct: [0],
    incorrect: [1, 3, 7],
  }),
  multipleChoiceProblem(presets["graph_3"], "test0", 10, {
    correct: [0],
    incorrect: [3, 6, 7],
  }),
  multipleChoiceProblem(presets["graph_4"], "test0", 10, {
    correct: [0],
    incorrect: [1, 2, 3],
  }),
  multipleChoiceProblem(presets["graph_5"], "test0", 10, {
    correct: [0],
    incorrect: [2, 3, 4],
  }),
  multipleChoiceProblem(presets["graph_6"], "test0", 10, {
    correct: [0],
    incorrect: [1, 6, 10],
  }),
];
export default function () {
  return <div>{problems}</div>;
}
