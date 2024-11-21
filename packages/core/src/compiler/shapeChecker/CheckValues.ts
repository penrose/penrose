import { Result } from "true-myth";
import { A } from "vitest/dist/types-ad1c3f45.js";
import * as ad from "../../types/ad.js";
import { BadShapeParamTypeError } from "../../types/errors.js";
import { StylePathToUnindexedObject } from "../../types/stylePathResolution.js";
import {
  BoolV,
  ClipDataV,
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
} from "../../types/value.js";
import { badShapeParamTypeError } from "../../utils/Error.js";
import { val } from "../../utils/Util.js";
const { err, ok } = Result;

export const checkFloatV = (
  path: StylePathToUnindexedObject<A>,
  value: Value<ad.Num>,
): Result<FloatV<ad.Num>, BadShapeParamTypeError> => {
  if (value.tag === "FloatV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "FloatV", false));
};

export const checkBoolV = (
  path: StylePathToUnindexedObject<A>,
  value: Value<ad.Num>,
): Result<BoolV, BadShapeParamTypeError> => {
  if (value.tag === "BoolV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "BoolV", false));
};

export const checkStrV = (
  path: StylePathToUnindexedObject<A>,
  value: Value<ad.Num>,
): Result<StrV, BadShapeParamTypeError> => {
  if (value.tag === "StrV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "StrV", false));
};

export const checkPathDataV = (
  path: StylePathToUnindexedObject<A>,
  value: Value<ad.Num>,
): Result<PathDataV<ad.Num>, BadShapeParamTypeError> => {
  if (value.tag === "PathDataV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "PathDataV", false));
};

export const checkPtListV = (
  path: StylePathToUnindexedObject<A>,
  value: Value<ad.Num>,
): Result<PtListV<ad.Num>, BadShapeParamTypeError> => {
  if (
    value.tag === "PtListV" ||
    value.tag === "LListV" ||
    value.tag === "MatrixV"
  ) {
    return ok({ tag: "PtListV", contents: value.contents });
  }
  return err(
    badShapeParamTypeError(
      path,
      val(value),
      "PtListV (also supports LListV and MatrixV)",
      false,
    ),
  );
};

export const checkColorV = (
  path: StylePathToUnindexedObject<A>,
  value: Value<ad.Num>,
): Result<ColorV<ad.Num>, BadShapeParamTypeError> => {
  if (value.tag === "ColorV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "ColorV", false));
};

export const checkListV = (
  path: StylePathToUnindexedObject<A>,
  value: Value<ad.Num>,
): Result<ListV<ad.Num>, BadShapeParamTypeError> => {
  if (
    value.tag === "ListV" ||
    value.tag === "VectorV" ||
    value.tag === "TupV"
  ) {
    return ok({ tag: "ListV", contents: value.contents });
  }
  return err(
    badShapeParamTypeError(
      path,
      val(value),
      "ListV (also supports VectorV and TupV)",
      false,
    ),
  );
};

export const checkVectorV = (
  path: StylePathToUnindexedObject<A>,
  value: Value<ad.Num>,
): Result<VectorV<ad.Num>, BadShapeParamTypeError> => {
  if (
    value.tag === "VectorV" ||
    value.tag === "ListV" ||
    value.tag === "TupV"
  ) {
    return ok({ tag: "VectorV", contents: value.contents });
  }
  return err(
    badShapeParamTypeError(
      path,
      val(value),
      "VectorV (also supports ListV and TupV)",
      false,
    ),
  );
};

export const checkMatrixV = (
  path: StylePathToUnindexedObject<A>,
  value: Value<ad.Num>,
): Result<MatrixV<ad.Num>, BadShapeParamTypeError> => {
  if (
    value.tag === "MatrixV" ||
    value.tag === "LListV" ||
    value.tag === "PtListV"
  ) {
    return ok({ tag: "MatrixV", contents: value.contents });
  }
  return err(
    badShapeParamTypeError(
      path,
      val(value),
      "MatrixV (also supports LListV and PtListV)",
      false,
    ),
  );
};

export const checkTupV = (
  path: StylePathToUnindexedObject<A>,
  value: Value<ad.Num>,
): Result<TupV<ad.Num>, BadShapeParamTypeError> => {
  if (value.tag === "TupV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "TupV", false));
};

export const checkLListV = (
  path: StylePathToUnindexedObject<A>,
  value: Value<ad.Num>,
): Result<LListV<ad.Num>, BadShapeParamTypeError> => {
  if (
    value.tag === "LListV" ||
    value.tag === "MatrixV" ||
    value.tag === "PtListV"
  ) {
    return ok({ tag: "LListV", contents: value.contents });
  }
  return err(
    badShapeParamTypeError(
      path,
      val(value),
      "LListV (also supports MatrixV and PtListV)",
      false,
    ),
  );
};

export const checkShapeListV = (
  path: StylePathToUnindexedObject<A>,
  value: Value<ad.Num>,
): Result<ShapeListV<ad.Num>, BadShapeParamTypeError> => {
  if (value.tag === "ShapeListV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "ShapeListV", false));
};

export const checkClipDataV = (
  path: StylePathToUnindexedObject<A>,
  value: Value<ad.Num>,
): Result<ClipDataV<ad.Num>, BadShapeParamTypeError> => {
  if (value.tag === "ClipDataV") {
    return ok(value);
  }
  return err(badShapeParamTypeError(path, val(value), "ClipDataV", false));
};
