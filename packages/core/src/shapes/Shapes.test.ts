import { makeCanvas } from "./Samplers";
import { shapedefs } from "./Shapes";

describe("shapedefs", () => {
  test("print", () => {
    const canvas = makeCanvas(800, 700);
    console.log(
      Object.fromEntries(
        Object.entries(shapedefs).map(([name, { sampler }]) => {
          const shape: { [prop: string]: any } = sampler(canvas);
          const types = Object.fromEntries(
            Object.entries(shape).map(([prop, { tag }]) => [prop, tag])
          );
          return [name, types];
        })
      )
    );
  });
});
