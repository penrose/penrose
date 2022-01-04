import { Canvas } from "./Samplers";
import { constructors } from "./Shapes";

describe("constructors", () => {
  test("print", () => {
    const width = 800;
    const height = 700;
    const canvas: Canvas = {
      width,
      height,
      size: [width, height],
      xRange: [-width / 2, width / 2],
      yRange: [-height / 2, height / 2],
    };
    console.log(
      Object.fromEntries(
        Object.entries(constructors).map(([name, constr]) => {
          const shape: { [prop: string]: any } = constr(canvas, {});
          delete shape.shapeType;
          delete shape.bbox;
          const types = Object.fromEntries(
            Object.entries(shape).map(([prop, { tag }]) => [prop, tag])
          );
          return [name, types];
        })
      )
    );
  });
});
