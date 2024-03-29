import { presets } from "../examples.js";
import { multipleChoiceProblem } from "./util.js";

export default function () {
  const problems = [
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
  return <div>{problems}</div>;
}
