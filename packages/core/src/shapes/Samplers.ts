import seedrandom from "seedrandom";
import * as ad from "types/ad";
import {
  Color,
  IBoolV,
  IColorV,
  IFileV,
  IFloatV,
  IIntV,
  IListV,
  IMatrixV,
  IPaletteV,
  IPathCmd,
  IPathDataV,
  IPtListV,
  IPtV,
  IStrV,
  IStyleV,
  ITupV,
  IVectorV,
} from "types/value";
import { randFloat } from "utils/Util";

type Range = [number, number];

// NOTE: I moved `canvasSize` here from Canvas.tsx, which re-exports it, to avoid a circular import in `Style`.

// export const canvasSize: [number, number] = [800, 700];
// export const canvasXRange: Range = [-canvasSize[0] / 2, canvasSize[0] / 2];
// export const canvasYRange: Range = [-canvasSize[1] / 2, canvasSize[1] / 2];
export interface ICanvas {
  width: number;
  height: number;
  size: [number, number];
  xRange: Range;
  yRange: Range;
}

export type Canvas = ICanvas;

export const makeCanvas = (width: number, height: number): Canvas => ({
  width,
  height,
  size: [width, height],
  xRange: [-width / 2, width / 2],
  yRange: [-height / 2, height / 2],
});

export const FloatV = (contents: ad.Num): IFloatV<ad.Num> => ({
  tag: "FloatV",
  contents,
});
export const IntV = (contents: number): IIntV => ({
  tag: "IntV",
  contents,
});
export const BoolV = (contents: boolean): IBoolV<ad.Num> => ({
  tag: "BoolV",
  contents,
});
export const StrV = (contents: string): IStrV => ({
  tag: "StrV",
  contents,
});
export const PtV = (contents: ad.Num[]): IPtV<ad.Num> => ({
  tag: "PtV",
  contents,
});
export const PathDataV = (
  contents: IPathCmd<ad.Num>[]
): IPathDataV<ad.Num> => ({
  tag: "PathDataV",
  contents,
});
export const PtListV = (contents: ad.Num[][]): IPtListV<ad.Num> => ({
  tag: "PtListV",
  contents,
});
export const ColorV = (contents: Color<ad.Num>): IColorV<ad.Num> => ({
  tag: "ColorV",
  contents,
});
export const PaletteV = (contents: Color<ad.Num>[]): IPaletteV<ad.Num> => ({
  tag: "PaletteV",
  contents,
});
export const FileV = (contents: string): IFileV<ad.Num> => ({
  tag: "FileV",
  contents,
});
export const StyleV = (contents: string): IStyleV<ad.Num> => ({
  tag: "StyleV",
  contents,
});
export const ListV = (contents: ad.Num[]): IListV<ad.Num> => ({
  tag: "ListV",
  contents,
});
export const VectorV = (contents: ad.Num[]): IVectorV<ad.Num> => ({
  tag: "VectorV",
  contents,
});
export const MatrixV = (contents: ad.Num[][]): IMatrixV<ad.Num> => ({
  tag: "MatrixV",
  contents,
});
export const TupV = (contents: ad.Num[]): ITupV<ad.Num> => ({
  tag: "TupV",
  contents,
});

export const sampleFloatIn = (
  rng: seedrandom.prng,
  min: number,
  max: number
): IFloatV<ad.Num> => FloatV(randFloat(rng, min, max));
export const sampleVector = (
  rng: seedrandom.prng,
  canvas: Canvas
): IVectorV<ad.Num> =>
  VectorV([randFloat(rng, ...canvas.xRange), randFloat(rng, ...canvas.yRange)]);
export const sampleWidth = (
  rng: seedrandom.prng,
  canvas: Canvas
): IFloatV<ad.Num> => FloatV(randFloat(rng, 3, canvas.width / 6));
export const sampleZero = (): IFloatV<ad.Num> => FloatV(0);
export const sampleHeight = (
  rng: seedrandom.prng,
  canvas: Canvas
): IFloatV<ad.Num> => FloatV(randFloat(rng, 3, canvas.height / 6));
export const sampleStroke = (rng: seedrandom.prng): IFloatV<ad.Num> =>
  FloatV(randFloat(rng, 0.5, 3));
export const sampleColor = (rng: seedrandom.prng): IColorV<ad.Num> => {
  const [min, max] = [0.1, 0.9];
  return ColorV({
    tag: "RGBA",
    contents: [
      randFloat(rng, min, max),
      randFloat(rng, min, max),
      randFloat(rng, min, max),
      0.5,
    ],
  });
};
export const sampleBlack = (): IColorV<ad.Num> =>
  ColorV({ tag: "RGBA", contents: [0, 0, 0, 1] });
export const sampleNoPaint = (): IColorV<ad.Num> => ColorV({ tag: "NONE" });
