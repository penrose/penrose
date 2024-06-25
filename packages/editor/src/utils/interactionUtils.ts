import { PenroseState, State, Value } from "@penrose/core";
import { min } from "lodash";

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

export type ChangePin = {
  tag: "ChangePin";
  active: boolean;
} & InteractionCommon;

export type Interaction = Translation | Scale | ChangePin;

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
  const shape = state.shapesByPath.get(path);
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

/**
 * Add`dx`and `dy` to `parentVaryingValues` to find offset shape values, but mutate
 * `recentVaryingValues` with these deltas added.
 * @param path
 * @param dx
 * @param dy
 * @param parentVaryingValues
 * @param recentVaryingValues
 * @param state
 */
export const translateVaryingValues = (
  path: string,
  dx: number,
  dy: number,
  parentVaryingValues: number[],
  recentVaryingValues: number[],
  state: State,
) => {
  const inputIdxPairs = getTranslatedInputsIdxs(path, state);
  for (let i = 0; i < inputIdxPairs.length; i++) {
    const [xIdx, yIdx] = inputIdxPairs[i];
    recentVaryingValues[xIdx] = parentVaryingValues[xIdx] + dx;
    recentVaryingValues[yIdx] = parentVaryingValues[yIdx] + dy;
  }
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

export const getScalingInfo = (
  path: string,
  state: PenroseState,
): ScalingInfo => {
  const shape = state.shapesByPath.get(path);
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
};

export const makeScaleCallback = (
  info: ScalingInfo,
  state: PenroseState,
): ((sx: number, sy: number, state: PenroseState) => void) => {
  switch (info.tag) {
    case "RadiusScaling": {
      const start = state.varyingValues[info.rIdx];
      return (sx, sy, state) => {
        state.varyingValues[info.rIdx] = start * min([sx, sy])!;
      };
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

export const scaleVaryingValues = (
  path: string,
  sx: number,
  sy: number,
  parentVaryingValues: number[],
  recentVaryingValues: number[],
  state: State,
) => {
  const info = getScalingInfo(path, state);
  switch (info.tag) {
    case "RadiusScaling": {
      const start = parentVaryingValues[info.rIdx];
      recentVaryingValues[info.rIdx] = start * min([sx, sy])!;
      break;
    }

    case "WidthHeightScaling": {
      const start = [
        parentVaryingValues[info.widthIdx],
        parentVaryingValues[info.heightIdx],
      ];
      recentVaryingValues[info.widthIdx] = start[0] * sx;
      recentVaryingValues[info.heightIdx] = start[1] * sy;
      break;
    }

    case "PointsScaling": {
      // TODO: points scaling
      throw new Error("Points scaling not implemented");
    }
  }
};

export const getAllInteractiveIdxs = (
  path: string,
  state: PenroseState,
): Set<number> => {
  const scaling = state.scalableShapePaths.has(path)
    ? getScalingInputIdxs(getScalingInfo(path, state))
    : [];
  const translating = state.translatableShapePaths.has(path)
    ? getTranslatedInputsIdxs(path, state).flat()
    : [];
  return new Set(scaling.concat(translating));
};
