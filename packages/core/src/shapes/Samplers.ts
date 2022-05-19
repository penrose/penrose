import seedrandom from "seedrandom";
import * as ad from "types/ad";
import {
  BoolV,
  Color,
  ColorV,
  FloatV,
  IntV,
  ListV,
  MatrixV,
  PathCmd,
  PathDataV,
  PtListV,
  StrV,
  TupV,
  VectorV,
} from "types/value";
import { randFloat } from "utils/Util";

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

export const floatV = (contents: ad.Num): FloatV<ad.Num> => ({
  tag: "FloatV",
  contents,
});
export const intV = (contents: number): IntV => ({
  tag: "IntV",
  contents,
});
export const boolV = (contents: boolean): BoolV => ({
  tag: "BoolV",
  contents,
});
export const strV = (contents: string): StrV => ({
  tag: "StrV",
  contents,
});
export const pathDataV = (contents: PathCmd<ad.Num>[]): PathDataV<ad.Num> => ({
  tag: "PathDataV",
  contents,
});
export const ptListV = (contents: ad.Num[][]): PtListV<ad.Num> => ({
  tag: "PtListV",
  contents,
});
export const colorV = (contents: Color<ad.Num>): ColorV<ad.Num> => ({
  tag: "ColorV",
  contents,
});

export const listV = (contents: ad.Num[]): ListV<ad.Num> => ({
  tag: "ListV",
  contents,
});
export const vectorV = (contents: ad.Num[]): VectorV<ad.Num> => ({
  tag: "VectorV",
  contents,
});
export const matrixV = (contents: ad.Num[][]): MatrixV<ad.Num> => ({
  tag: "MatrixV",
  contents,
});
export const tupV = (contents: ad.Num[]): TupV<ad.Num> => ({
  tag: "TupV",
  contents,
});

export const sampleFloatIn = (
  rng: seedrandom.prng,
  min: number,
  max: number
): FloatV<ad.Num> => floatV(randFloat(rng, min, max));
export const sampleVector = (
  rng: seedrandom.prng,
  canvas: Canvas
): VectorV<ad.Num> =>
  vectorV([randFloat(rng, ...canvas.xRange), randFloat(rng, ...canvas.yRange)]);
export const sampleWidth = (
  rng: seedrandom.prng,
  canvas: Canvas
): FloatV<ad.Num> => floatV(randFloat(rng, 3, canvas.width / 6));
export const sampleZero = (): FloatV<ad.Num> => floatV(0);
export const sampleHeight = (
  rng: seedrandom.prng,
  canvas: Canvas
): FloatV<ad.Num> => floatV(randFloat(rng, 3, canvas.height / 6));
export const sampleStroke = (rng: seedrandom.prng): FloatV<ad.Num> =>
  floatV(randFloat(rng, 0.5, 3));
export const sampleColor = (rng: seedrandom.prng): ColorV<ad.Num> => {
  const [min, max] = [0.1, 0.9];
  return colorV({
    tag: "RGBA",
    contents: [
      randFloat(rng, min, max),
      randFloat(rng, min, max),
      randFloat(rng, min, max),
      0.5,
    ],
  });
};
export const sampleBlack = (): ColorV<ad.Num> =>
  colorV({ tag: "RGBA", contents: [0, 0, 0, 1] });
export const sampleNoPaint = (): ColorV<ad.Num> => colorV({ tag: "NONE" });
