import { varOf } from "engine/Autodiff";
import { IVarAD } from "types/ad";
import { Queue } from "./Util";

describe("Queue tests", () => {
  test("add some ints and then remove it", () => {
    const q = new Queue<Number>();
    expect(q.size).toEqual(0);
    q.enqueue(1);
    expect(q.size).toEqual(1);
    q.enqueue(2);
    expect(q.size).toEqual(2);
    expect(q.dequeue()).toEqual(1);
    expect(q.size).toEqual(1);
    expect(q.dequeue()).toEqual(2);
    expect(q.size).toEqual(0);
  });
  test("throw when empty", () => {
    const q = new Queue<Number>();
    expect(() => {
      q.dequeue();
    }).toThrowError();
  });
  test("simple object test", () => {
    const q = new Queue<IVarAD>();
    const var1 = varOf(1);
    var1.nodeVisited = false;
    q.enqueue(var1);
    const isSame = q.dequeue();
    isSame.nodeVisited = true;
    expect(var1.nodeVisited).toEqual(true);
    expect(var1.nodeVisited).toEqual(isSame.nodeVisited);
  });
});
