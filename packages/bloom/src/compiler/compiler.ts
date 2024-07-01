import {
  Canvas,
  Num,
  SamplingContext,
  Shape,
  Var,
  simpleContext,
  uniform,
} from "@penrose/core";

export class Diagram {
  private canvas: Canvas;
  private inputs: Var[];
  private inputNames: Map<string, number>;
  private samplingContext: SamplingContext;

  constructor(canvas: Canvas, variation: string) {
    this.canvas = canvas;
    this.inputs = [];
    this.inputNames = new Map();

    const { makeInput: createVar } = simpleContext(variation);
    this.samplingContext = {
      makeInput: (meta: SamplingContext["makeInput"], name?: string) => {
        const newVar = createVar(meta);
        this.inputs.push(newVar);
        if (name !== undefined) {
          this.inputNames.set(name, this.inputs.length - 1);
        }
      },
    };
  }

  makeVarying = (name?: string): Var => {
    const newVar = this.samplingContext.makeInput(
      {
        init: { tag: "Sampled", sampler: uniform(...this.canvas.xRange) },
      },
      name,
    );
    return newVar;
  };

  makeShape = <T>(shape: Shape): Shape<Num> & { shapeType: T } => {
    this.makeShape({
      shapeType: "Circle",
      r: { tag: "FloatV", contents: 5 },
    });
  };
}
