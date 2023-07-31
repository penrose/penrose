import seedrandom from "seedrandom";
import { variable } from "../engine/Autodiff.js";
import * as ad from "../types/ad.js";
import { OptStages } from "../types/state.js";
import { ColorV, FloatV, VectorV } from "../types/value.js";
import { colorV, floatV, randFloat, vectorV } from "../utils/Util.js";

type Range = [number, number];

// NOTE: I moved `canvasSize` here from Canvas.tsx, which re-exports it, to avoid a circular import in `Style`.

// export const canvasSize: [number, number] = [800, 700];
// export const canvasXRange: Range = [-canvasSize[0] / 2, canvasSize[0] / 2];
// export const canvasYRange: Range = [-canvasSize[1] / 2, canvasSize[1] / 2];
export interface Canvas {
  width: number;
  height: number;
  size: [number, number];
  xRange: Range;
  yRange: Range;
}

export const makeCanvas = (width: number, height: number): Canvas => ({
  width,
  height,
  size: [width, height],
  xRange: [-width / 2, width / 2],
  yRange: [-height / 2, height / 2],
});

export type Sampler = (rng: seedrandom.prng) => number;

export interface Pending {
  tag: "Pending";
  pending: number; // placeholder value to use until label collection completes
}

export interface Sampled {
  tag: "Sampled";
  sampler: Sampler;
}

export interface InputMeta {
  init: Pending | Sampled;
  stages: OptStages; // can be the empty set, meaning unoptimized
}

export type InputFactory = (meta: InputMeta) => ad.Var; // NOTE: stateful!

export interface Context {
  makeInput: InputFactory;
}

/**
 * Return a simple `Context` which starts with a `seedrandom` PRNG seeded with
 * `variation`, and for each `makeInput` invocation, sets `val` by calling the
 * using the given `sampler` or placeholder `pending` value.
 */
export const simpleContext = (variation: string): Context => {
  const rng = seedrandom(variation);
  return {
    makeInput: (meta) =>
      variable(
        meta.init.tag === "Sampled"
          ? meta.init.sampler(rng)
          : meta.init.pending,
      ),
  };
};

export const uniform =
  (min: number, max: number): Sampler =>
  (rng: seedrandom.prng) =>
    randFloat(rng, min, max);

export const sampleVector = (
  { makeInput }: Context,
  canvas: Canvas,
): VectorV<ad.Num> =>
  vectorV([
    makeInput({
      init: { tag: "Sampled", sampler: uniform(...canvas.xRange) },
      stages: "All",
    }),
    makeInput({
      init: { tag: "Sampled", sampler: uniform(...canvas.yRange) },
      stages: "All",
    }),
  ]);

export const sampleWidth = (
  { makeInput }: Context,
  canvas: Canvas,
): FloatV<ad.Num> =>
  floatV(
    makeInput({
      init: { tag: "Sampled", sampler: uniform(3, canvas.width / 6) },
      stages: "All",
    }),
  );

export const sampleHeight = (
  { makeInput }: Context,
  canvas: Canvas,
): FloatV<ad.Num> =>
  floatV(
    makeInput({
      init: { tag: "Sampled", sampler: uniform(3, canvas.height / 6) },
      stages: "All",
    }),
  );

export const sampleStroke = ({ makeInput }: Context): FloatV<ad.Num> =>
  floatV(
    makeInput({
      init: { tag: "Sampled", sampler: uniform(0.5, 3) },
      stages: "All",
    }),
  );

export const sampleColor = ({ makeInput }: Context): ColorV<ad.Num> => {
  const [min, max] = [0.1, 0.9];
  return colorV({
    tag: "RGBA",
    contents: [
      makeInput({
        init: { tag: "Sampled", sampler: uniform(min, max) },
        stages: "All",
      }),
      makeInput({
        init: { tag: "Sampled", sampler: uniform(min, max) },
        stages: "All",
      }),
      makeInput({
        init: { tag: "Sampled", sampler: uniform(min, max) },
        stages: "All",
      }),
      0.5,
    ],
  });
};
