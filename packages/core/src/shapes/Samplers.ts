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
export type InputFactory = (sampler: Sampler) => ad.Input; // NOTE: stateful!

export interface Context {
  makeInput: InputFactory;
}

export const uniform = (min: number, max: number): Sampler => (
  rng: seedrandom.prng
) => randFloat(rng, min, max);

export const sampleVector = (
  { makeInput }: Context,
  canvas: Canvas
): VectorV<ad.Num> =>
  vectorV([
    makeInput(uniform(...canvas.xRange)),
    makeInput(uniform(...canvas.yRange)),
  ]);

export const sampleWidth = (
  { makeInput }: Context,
  canvas: Canvas
): FloatV<ad.Num> => floatV(makeInput(uniform(3, canvas.width / 6)));

export const sampleHeight = (
  { makeInput }: Context,
  canvas: Canvas
): FloatV<ad.Num> => floatV(makeInput(uniform(3, canvas.height / 6)));

export const sampleStroke = ({ makeInput }: Context): FloatV<ad.Num> =>
  floatV(makeInput(uniform(0.5, 3)));

export const sampleColor = ({ makeInput }: Context): ColorV<ad.Num> => {
  const [min, max] = [0.1, 0.9];
  return colorV({
    tag: "RGBA",
    contents: [
      makeInput(uniform(min, max)),
      makeInput(uniform(min, max)),
      makeInput(uniform(min, max)),
      0.5,
    ],
  });
};
