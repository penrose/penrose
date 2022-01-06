import { add, clearVisitedNodes, varOf } from "engine/Autodiff";
import { EdgeAD } from "types/ad";
describe("clearVisitedNodeInput tests", () => {
  test("clears one node graph", () => {
    const var1 = varOf(1);
    var1.nodeVisited = true;
    clearVisitedNodes([var1]);
    expect(var1.nodeVisited).toEqual(false);
  });
  test("clears addition of two numbers graph", () => {
    const var1 = varOf(1);
    const var2 = varOf(2);
    const addVar = add(var1, var2);
    addVar.nodeVisited = true;
    var1.nodeVisited = true;
    clearVisitedNodes([addVar]);
    expect(var1.nodeVisited).toEqual(false);
    expect(var2.nodeVisited).toEqual(false);
    expect(addVar.nodeVisited).toEqual(false);
  });
});
