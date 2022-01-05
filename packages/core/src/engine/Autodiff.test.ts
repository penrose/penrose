import { clearVisitedNodesOutput, varOf } from "engine/Autodiff";
import { EdgeAD } from "types/ad";
describe("clearVisitedNodeInput tests", () => {
  test("clears one node graph", () => {
    const var1 = varOf(1);
    var1.nodeVisited = true;
    clearVisitedNodesOutput(var1);
    expect(var1.nodeVisited).toEqual(false);
  });
  test("clears one parent and one child graph", () => {
    const var1 = varOf(1);
    const var2 = varOf(2);
    var2.nodeVisited = true;
    var1.nodeVisited = true;
    const edge1: EdgeAD = { node: var2, sensitivityNode: { tag: "Nothing" } };
    var1.children.push(edge1);
    clearVisitedNodesOutput(var1);
    expect(var1.nodeVisited).toEqual(false);
    expect(var2.nodeVisited).toEqual(false);
  });
});
