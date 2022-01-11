import { varOf } from "engine/Autodiff";
import { IVarAD } from "types/ad";
import { mapMap, Queue } from "./Util";

describe("mapMap tests", () => {
  test("increment value", () => {
    // https://stackoverflow.com/q/31084619
    const myMap = new Map([
      ["thing1", 1],
      ["thing2", 2],
      ["thing3", 3],
    ]);
    const newMap = mapMap(myMap, (value) => value + 1);
    expect([...newMap.keys()]).toEqual([...myMap.keys()]);
    expect(newMap.get("thing1")).toBe(2);
    expect(newMap.get("thing2")).toBe(3);
    expect(newMap.get("thing3")).toBe(4);
  });
});

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
