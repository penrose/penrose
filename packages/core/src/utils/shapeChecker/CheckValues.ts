import { Result } from "true-myth";
import { err, ok } from "true-myth/result";
import * as ad from "../../types/ad";
import { StyleError } from "../../types/errors";
import {
  BoolV,
  ColorV,
  FloatV,
  GPIListV,
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

export const checkFloatV = (
  path: string,
  value: Value<ad.Num>
): Result<FloatV<ad.Num>, StyleError> => {
  if (value.tag === "FloatV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, value));
};

export const checkBoolV = (
  path: string,
  value: Value<ad.Num>
): Result<BoolV, StyleError> => {
  if (value.tag === "BoolV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, value));
};

export const checkStrV = (
  path: string,
  value: Value<ad.Num>
): Result<StrV, StyleError> => {
  if (value.tag === "StrV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, value));
};

export const checkPathDataV = (
  path: string,
  value: Value<ad.Num>
): Result<PathDataV<ad.Num>, StyleError> => {
  if (value.tag === "PathDataV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, value));
};

export const checkPtListV = (
  path: string,
  value: Value<ad.Num>
): Result<PtListV<ad.Num>, StyleError> => {
  if (value.tag === "PtListV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, value));
};

export const checkColorV = (
  path: string,
  value: Value<ad.Num>
): Result<ColorV<ad.Num>, StyleError> => {
  if (value.tag === "ColorV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, value));
};

export const checkListV = (
  path: string,
  value: Value<ad.Num>
): Result<ListV<ad.Num>, StyleError> => {
  if (value.tag === "ListV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, value));
};

export const checkVectorV = (
  path: string,
  value: Value<ad.Num>
): Result<VectorV<ad.Num>, StyleError> => {
  if (value.tag === "VectorV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, value));
};

export const checkMatrixV = (
  path: string,
  value: Value<ad.Num>
): Result<MatrixV<ad.Num>, StyleError> => {
  if (value.tag === "MatrixV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, value));
};

export const checkTupV = (
  path: string,
  value: Value<ad.Num>
): Result<TupV<ad.Num>, StyleError> => {
  if (value.tag === "TupV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, value));
};

export const checkLListV = (
  path: string,
  value: Value<ad.Num>
): Result<LListV<ad.Num>, StyleError> => {
  if (value.tag === "LListV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, value));
};

export const checkGPIListV = (
  path: string,
  value: Value<ad.Num>
): Result<GPIListV<ad.Num>, StyleError> => {
  if (value.tag === "GPIListV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, value));
};

export const checkShapeListV = (
  path: string,
  value: Value<ad.Num>
): Result<ShapeListV<ad.Num>, StyleError> => {
  if (value.tag === "ShapeListV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, value));
};
