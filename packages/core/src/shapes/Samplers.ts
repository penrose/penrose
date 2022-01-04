import { constOf } from "engine/Autodiff";
import { VarAD } from "types/ad";
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

export const FloatV = (contents: VarAD): IFloatV<VarAD> => ({
  tag: "FloatV",
  contents,
});
export const IntV = (contents: number): IIntV => ({
  tag: "IntV",
  contents,
});
export const BoolV = (contents: boolean): IBoolV<VarAD> => ({
  tag: "BoolV",
  contents,
});
export const StrV = (contents: string): IStrV => ({
  tag: "StrV",
  contents,
});
export const PtV = (contents: VarAD[]): IPtV<VarAD> => ({
  tag: "PtV",
  contents,
});
export const PathDataV = (contents: IPathCmd<VarAD>[]): IPathDataV<VarAD> => ({
  tag: "PathDataV",
  contents,
});
export const PtListV = (contents: VarAD[][]): IPtListV<VarAD> => ({
  tag: "PtListV",
  contents,
});
export const ColorV = (contents: Color<VarAD>): IColorV<VarAD> => ({
  tag: "ColorV",
  contents,
});
export const PaletteV = (contents: Color<VarAD>[]): IPaletteV<VarAD> => ({
  tag: "PaletteV",
  contents,
});
export const FileV = (contents: string): IFileV<VarAD> => ({
  tag: "FileV",
  contents,
});
export const StyleV = (contents: string): IStyleV<VarAD> => ({
  tag: "StyleV",
  contents,
});
export const ListV = (contents: VarAD[]): IListV<VarAD> => ({
  tag: "ListV",
  contents,
});
export const VectorV = (contents: VarAD[]): IVectorV<VarAD> => ({
  tag: "VectorV",
  contents,
});
export const MatrixV = (contents: VarAD[][]): IMatrixV<VarAD> => ({
  tag: "MatrixV",
  contents,
});
export const TupV = (contents: VarAD[]): ITupV<VarAD> => ({
  tag: "TupV",
  contents,
});

export const sampleFloatIn = (min: number, max: number): IFloatV<VarAD> =>
  FloatV(constOf(randFloat(min, max)));
export const sampleVector = (canvas: Canvas): IVectorV<VarAD> =>
  VectorV(
    [randFloat(...canvas.xRange), randFloat(...canvas.yRange)].map(constOf)
  );
export const sampleWidth = (canvas: Canvas): IFloatV<VarAD> =>
  FloatV(constOf(randFloat(3, canvas.width / 6)));
export const sampleZero = (): IFloatV<VarAD> => FloatV(constOf(0));
export const sampleHeight = (canvas: Canvas): IFloatV<VarAD> =>
  FloatV(constOf(randFloat(3, canvas.height / 6)));
export const sampleStroke = (): IFloatV<VarAD> =>
  FloatV(constOf(randFloat(0.5, 3)));
export const sampleColor = (): IColorV<VarAD> => {
  const [min, max] = [0.1, 0.9];
  return ColorV({
    tag: "RGBA",
    contents: [
      randFloat(min, max),
      randFloat(min, max),
      randFloat(min, max),
      0.5,
    ].map(constOf),
  });
};
export const sampleBlack = (): IColorV<VarAD> =>
  ColorV({ tag: "RGBA", contents: [0, 0, 0, 1].map(constOf) });
export const sampleNoPaint = (): IColorV<VarAD> => ColorV({ tag: "NONE" });
