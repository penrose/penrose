import { Result } from "true-myth";
import * as ad from "../../types/ad";
import { BadShapeParamTypeError } from "../../types/errors";
import {
  BoolV,
  ColorV,
  FloatV,
  ListV,
  LListV,
  MatrixV,
  PathDataV,
  PtListV,
  ShapeListV,
  StrV,
  TupV,
  Value,
  VectorV,
} from "../../types/value";
import { badShapeParamTypeError } from "../Error";
import { val } from "../Util";
const { err, ok } = Result;

export const checkFloatV = (
  path: string,
  value: Value<ad.Num>
): Result<FloatV<ad.Num>, BadShapeParamTypeError> => {
  if (value.tag === "FloatV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "FloatV", false));
};

export const checkBoolV = (
  path: string,
  value: Value<ad.Num>
): Result<BoolV, BadShapeParamTypeError> => {
  if (value.tag === "BoolV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "BoolV", false));
};

export const checkStrV = (
  path: string,
  value: Value<ad.Num>
): Result<StrV, BadShapeParamTypeError> => {
  if (value.tag === "StrV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "StrV", false));
};

export const checkPathDataV = (
  path: string,
  value: Value<ad.Num>
): Result<PathDataV<ad.Num>, BadShapeParamTypeError> => {
  if (value.tag === "PathDataV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "PathDataV", false));
};

export const checkPtListV = (
  path: string,
  value: Value<ad.Num>
): Result<PtListV<ad.Num>, BadShapeParamTypeError> => {
  if (value.tag === "PtListV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "PtListV", false));
};

export const checkColorV = (
  path: string,
  value: Value<ad.Num>
): Result<ColorV<ad.Num>, BadShapeParamTypeError> => {
  if (value.tag === "ColorV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "ColorV", false));
};

export const checkListV = (
  path: string,
  value: Value<ad.Num>
): Result<ListV<ad.Num>, BadShapeParamTypeError> => {
  if (value.tag === "ListV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "ListV", false));
};

export const checkVectorV = (
  path: string,
  value: Value<ad.Num>
): Result<VectorV<ad.Num>, BadShapeParamTypeError> => {
  if (value.tag === "VectorV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "VectorV", false));
};

export const checkMatrixV = (
  path: string,
  value: Value<ad.Num>
): Result<MatrixV<ad.Num>, BadShapeParamTypeError> => {
  if (value.tag === "MatrixV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "MatrixV", false));
};

export const checkTupV = (
  path: string,
  value: Value<ad.Num>
): Result<TupV<ad.Num>, BadShapeParamTypeError> => {
  if (value.tag === "TupV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "TupV", false));
};

export const checkLListV = (
  path: string,
  value: Value<ad.Num>
): Result<LListV<ad.Num>, BadShapeParamTypeError> => {
  if (value.tag === "LListV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "LListV", false));
};

export const checkShapeListV = (
  path: string,
  value: Value<ad.Num>
): Result<ShapeListV<ad.Num>, BadShapeParamTypeError> => {
  if (value.tag === "ShapeListV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "ShapeListV", false));
};
