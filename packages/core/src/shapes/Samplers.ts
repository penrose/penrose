import { input } from "engine/Autodiff";
import seedrandom from "seedrandom";
import * as ad from "types/ad";
import { ColorV, FloatV, VectorV } from "types/value";
import { colorV, floatV, randFloat, vectorV } from "utils/Util";

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

export interface SamplerMeta {
  sampler: Sampler;
}

export interface PendingMeta {
  pending: number; // placeholder value to use until label collection completes
}

export type InputMeta = SamplerMeta | PendingMeta;

export type InputFactory = (meta: InputMeta) => ad.Input; // NOTE: stateful!

export interface Context {
  makeInput: InputFactory;
}

/**
 * Return a simple `Context` which starts with a `seedrandom` PRNG seeded with
 * `variation`, and for each `makeInput` invocation, sets `val` by calling the
 * using the given `sampler` or placeholder `pending` value, then increments a
 * counter for the `key` field.
 */
export const simpleContext = (variation: string): Context => {
  const rng = seedrandom(variation);
  let i = 0;
  return {
    makeInput: (meta) =>
      input({
        key: i++,
        val: "pending" in meta ? meta.pending : meta.sampler(rng),
      }),
  };
};

export const uniform = (min: number, max: number): Sampler => (
  rng: seedrandom.prng
) => randFloat(rng, min, max);

export const sampleVector = (
  { makeInput }: Context,
  canvas: Canvas
): VectorV<ad.Num> =>
  vectorV([
    makeInput({ sampler: uniform(...canvas.xRange) }),
    makeInput({ sampler: uniform(...canvas.yRange) }),
  ]);

export const sampleWidth = (
  { makeInput }: Context,
  canvas: Canvas
): FloatV<ad.Num> =>
  floatV(makeInput({ sampler: uniform(3, canvas.width / 6) }));

export const sampleHeight = (
  { makeInput }: Context,
  canvas: Canvas
): FloatV<ad.Num> =>
  floatV(makeInput({ sampler: uniform(3, canvas.height / 6) }));

export const sampleStroke = ({ makeInput }: Context): FloatV<ad.Num> =>
  floatV(makeInput({ sampler: uniform(0.5, 3) }));

export const sampleColor = ({ makeInput }: Context): ColorV<ad.Num> => {
  const [min, max] = [0.1, 0.9];
  return colorV({
    tag: "RGBA",
    contents: [
      makeInput({ sampler: uniform(min, max) }),
      makeInput({ sampler: uniform(min, max) }),
      makeInput({ sampler: uniform(min, max) }),
      0.5,
    ],
  });
};
