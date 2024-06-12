import { PenroseState, State, Value } from "@penrose/core";
import { max, min } from "lodash";

export type InteractionCommon = {
  path: string;
};

export type Translation = {
  tag: "Translation";
  dx: number;
  dy: number;
} & InteractionCommon;

export type Scale = {
  tag: "Scale";
  sx: number;
  sy: number;
} & InteractionCommon;

export type Interaction = Translation | Scale;

const valueIsVectorNumeric = (
  val: Value.Value<number | undefined>,
): val is Value.ListV<number> | Value.TupV<number> | Value.VectorV<number> => {
  if (!(val.tag === "ListV" || val.tag === "VectorV" || val.tag === "TupV")) {
    return false;
  }

  for (const elem of val.contents) {
    if (!elem) {
      return false;
    }
  }

  return true;
};

export const getTranslatedInputsIdxs = (
  path: string,
  state: State,
): [number, number][] => {
  // TODO: cache shape names
  const shape = state.shapes.find((s) => s.name.contents === path);
  if (shape === undefined) {
    throw new Error(`No shape with path ${path}`);
  }

  switch (shape.shapeType) {
    case "Circle":
    case "Ellipse":
    case "Equation":
    case "Image":
    case "Rectangle":
    case "Text": {
      const center = state.inputIdxsByPath.get(path + ".center");
      if (
        !center ||
        center.tag !== "Val" ||
        !valueIsVectorNumeric(center.contents)
      ) {
        throw new Error(`Could not find center input indices at path ${path}`);
      }
      const [xIdx, yIdx] = center.contents.contents;
      return [[xIdx, yIdx]];
    }

    case "Line": {
      const start = state.inputIdxsByPath.get(path + ".start");
      const end = state.inputIdxsByPath.get(path + ".end");
      if (
        !start ||
        start.tag !== "Val" ||
        !valueIsVectorNumeric(start.contents)
      ) {
        throw new Error(`Could not find start input indices at path ${path}`);
      }
      if (!end || end.tag !== "Val" || !valueIsVectorNumeric(end.contents)) {
        throw new Error(`Could not find end input indices at path ${path}`);
      }
      const [startXIdx, startYIdx] = start.contents.contents;
      const [endXIdx, endYIdx] = end.contents.contents;
      return [
        [startXIdx, startYIdx],
        [endXIdx, endYIdx],
      ];
    }

    case "Polygon":
    case "Polyline": {
      const points = state.inputIdxsByPath.get(path + ".points");
      if (!points || points.tag !== "Val" || points.contents.tag !== "LListV") {
        throw new Error(`Could not find points inputs indices at path ${path}`);
      }
      const idxs: [number, number][] = [];
      for (const point of points.contents.contents) {
        const [xIdx, yIdx] = point;
        if (!xIdx || !yIdx) {
          throw new Error(
            `Untranslatable points in point list at path ${path}`,
          );
        }
        idxs.push([xIdx, yIdx]);
      }
      return idxs;
    }

    default:
      throw new Error(`Untranslatable shape type ${shape.shapeType}`);
  }
};

export const makeTranslateCallback = (
  inputIdxPairs: [number, number][],
  state: PenroseState,
): ((dx: number, dy: number, state: PenroseState) => void) => {
  const startingVals: [number, number][] = [];
  for (const [xIdx, yIdx] of inputIdxPairs) {
    startingVals.push([state.varyingValues[xIdx], state.varyingValues[yIdx]]);
  }
  return (dx: number, dy: number, state: PenroseState) => {
    for (let i = 0; i < inputIdxPairs.length; i++) {
      const [xIdx, yIdx] = inputIdxPairs[i];
      const [startX, startY] = startingVals[i];
      state.varyingValues[xIdx] = startX + dx;
      state.varyingValues[yIdx] = startY + dy;
    }
  };
};

export type RadiusScaling = {
  tag: "RadiusScaling";
  rIdx: number;
};

export type WidthHeightScaling = {
  tag: "WidthHeightScaling";
  widthIdx: number;
  heightIdx: number;
};

export type PointsScaling = {
  tag: "PointsScaling";
  pointIdxs: [number, number][];
};

export type ScalingInfo = RadiusScaling | WidthHeightScaling | PointsScaling;

const valueIsNumeric = (
  val: Value.Value<number | undefined>,
): val is Value.FloatV<number> => {
  return val.tag === "FloatV" && !!val.contents;
};

export const getScalingInfo = (path: string, state: PenroseState): ScalingInfo => {
  const shape = state.shapes.find((s) => s.name.contents === path);
  if (shape === undefined) {
    throw new Error(`No shape with path ${path}`);
  }

  switch (shape.shapeType) {
    case "Circle": {
      const r = state.inputIdxsByPath.get(path + ".r");
      if (!r || r.tag !== "Val" || !valueIsNumeric(r.contents)) {
        throw new Error(`Could not find radius input index for path ${path}`);
      }
      return {
        tag: "RadiusScaling",
        rIdx: r.contents.contents,
      };
    }

    case "Ellipse": {
      const rx = state.inputIdxsByPath.get(path + ".rx");
      const ry = state.inputIdxsByPath.get(path + ".ry");
      if (!rx || rx.tag !== "Val" || !valueIsNumeric(rx.contents)) {
        throw new Error(`Could not find rx input index for path ${path}`);
      }
      if (!ry || ry.tag !== "Val" || !valueIsNumeric(ry.contents)) {
        throw new Error(`Could not find ry input index for path ${path}`);
      }
      return {
        tag: "WidthHeightScaling",
        widthIdx: rx.contents.contents,
        heightIdx: ry.contents.contents,
      };
    }

    case "Equation":
    case "Image":
    case "Rectangle":
    case "Text": {
      const width = state.inputIdxsByPath.get(path + ".width");
      const height = state.inputIdxsByPath.get(path + ".height");
      if (!width || width.tag !== "Val" || !valueIsNumeric(width.contents)) {
        throw new Error(`Could not find width input index for path ${path}`);
      }
      if (!height || height.tag !== "Val" || !valueIsNumeric(height.contents)) {
        throw new Error(`Could not find height input index for path ${path}`);
      }
      return {
        tag: "WidthHeightScaling",
        widthIdx: width.contents.contents,
        heightIdx: height.contents.contents,
      };
    }

    case "Line":
    case "Polygon":
    case "Polyline": {
      return {
        tag: "PointsScaling",
        pointIdxs: getTranslatedInputsIdxs(path, state),
      };
    }

    default:
      throw new Error(`Unscalable shape type "${shape.shapeType}"`);
  }
};

export const getScalingInputIdxs = (info: ScalingInfo): number[] => {
  switch (info.tag) {
    case "WidthHeightScaling":
      return [info.widthIdx, info.heightIdx];

    case "RadiusScaling":
      return [info.rIdx];

    case "PointsScaling":
      return info.pointIdxs.flat();
  }
}

export const makeScaleCallback = (
  info: ScalingInfo,
  state: PenroseState,
): ((sx: number, sy: number, state: PenroseState) => void) => {
  switch (info.tag) {
    case "RadiusScaling": {
      const start = state.varyingValues[info.rIdx];
      return (sx, sy, state) => {
        state.varyingValues[info.rIdx] = start * max([sx, sy])!;
      }
    }

    case "WidthHeightScaling": {
      const start = [
        state.varyingValues[info.widthIdx],
        state.varyingValues[info.heightIdx],
      ];
      return (sx: number, sy: number, state: PenroseState) => {
        state.varyingValues[info.widthIdx] = start[0] * sx;
        state.varyingValues[info.heightIdx] = start[1] * sy;
      };
    }

    case "PointsScaling": {
      // TODO: points scaling
      throw new Error("Points scaling not implemented");
    }
  }
};
