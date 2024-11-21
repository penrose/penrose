import { Result } from "true-myth";
import { A } from "vitest/dist/types-ad1c3f45.js";
import * as ad from "../../types/ad.js";
import { StyleError } from "../../types/errors.js";
import {
  Arrow,
  Center,
  Corner,
  Fill,
  Named,
  Poly,
  Rect,
  Rotate,
  Scale,
  String as StringProps,
  Stroke,
} from "../../types/shapes.js";
import { StylePathToUnindexedObject } from "../../types/stylePathResolution.js";
import { Translation } from "../../types/styleSemantics.js";
import { Value } from "../../types/value.js";
import { prettyResolvedStylePath } from "../../utils/Util.js";
import { internalMissingPathError } from "../Style.js";
import {
  checkBoolV,
  checkColorV,
  checkFloatV,
  checkPtListV,
  checkStrV,
  checkVectorV,
} from "./CheckValues.js";
const { err, ok } = Result;
const getTransProp = (
  path: StylePathToUnindexedObject<A>,
  trans: Translation,
): Value<ad.Num> => {
  const pathStr = prettyResolvedStylePath(path);
  const v = trans.symbols.get(pathStr);
  if (v === undefined || v.tag !== "Val") {
    throw internalMissingPathError(pathStr);
  }
  return v.contents;
};

export const checkProp = <T>(
  path: StylePathToUnindexedObject<A>,
  prop: string,
  trans: Translation,
  checker: (
    propPath: StylePathToUnindexedObject<A>,
    value: Value<ad.Num>,
  ) => Result<T, StyleError>,
): Result<T, StyleError> => {
  const propPath: StylePathToUnindexedObject<A> = {
    nodeType: "SyntheticStyle",
    tag: "Object",
    access: {
      tag: "Member",
      parent: path,
      name: prop,
    },
  };
  const propV = getTransProp(propPath, trans);
  return checker(propPath, propV);
};

export const checkNamed = (
  path: StylePathToUnindexedObject<A>,
  trans: Translation,
): Result<Named<ad.Num>, StyleError> => {
  const name = checkProp(path, "name", trans, checkStrV);
  if (name.isErr()) return err(name.error);

  const ensureOnCanvas = checkProp(path, "ensureOnCanvas", trans, checkBoolV);
  if (ensureOnCanvas.isErr()) return err(ensureOnCanvas.error);

  return ok({
    name: name.value,
    ensureOnCanvas: ensureOnCanvas.value,
  });
};

export const checkStroke = (
  path: StylePathToUnindexedObject<A>,
  trans: Translation,
): Result<Stroke<ad.Num>, StyleError> => {
  const strokeWidth = checkProp(path, "strokeWidth", trans, checkFloatV);
  if (strokeWidth.isErr()) return err(strokeWidth.error);

  const strokeStyle = checkProp(path, "strokeStyle", trans, checkStrV);
  if (strokeStyle.isErr()) return err(strokeStyle.error);

  const strokeColor = checkProp(path, "strokeColor", trans, checkColorV);
  if (strokeColor.isErr()) return err(strokeColor.error);

  const strokeDasharray = checkProp(path, "strokeDasharray", trans, checkStrV);
  if (strokeDasharray.isErr()) return err(strokeDasharray.error);

  return ok({
    strokeWidth: strokeWidth.value,
    strokeStyle: strokeStyle.value,
    strokeColor: strokeColor.value,
    strokeDasharray: strokeDasharray.value,
  });
};

export const checkFill = (
  path: StylePathToUnindexedObject<A>,
  trans: Translation,
): Result<Fill<ad.Num>, StyleError> => {
  const fillColor = checkProp(path, "fillColor", trans, checkColorV);
  if (fillColor.isErr()) return err(fillColor.error);

  return ok({ fillColor: fillColor.value });
};

export const checkCenter = (
  path: StylePathToUnindexedObject<A>,
  trans: Translation,
): Result<Center<ad.Num>, StyleError> => {
  const center = checkProp(path, "center", trans, checkVectorV);
  if (center.isErr()) return err(center.error);

  return ok({ center: center.value });
};

export const checkRect = (
  path: StylePathToUnindexedObject<A>,
  trans: Translation,
): Result<Rect<ad.Num>, StyleError> => {
  const width = checkProp(path, "width", trans, checkFloatV);
  if (width.isErr()) return err(width.error);

  const height = checkProp(path, "height", trans, checkFloatV);
  if (height.isErr()) return err(height.error);

  return ok({ width: width.value, height: height.value });
};

export const checkArrow = (
  path: StylePathToUnindexedObject<A>,
  trans: Translation,
): Result<Arrow<ad.Num>, StyleError> => {
  const startArrowheadSize = checkProp(
    path,
    "startArrowheadSize",
    trans,
    checkFloatV,
  );
  if (startArrowheadSize.isErr()) return err(startArrowheadSize.error);

  const endArrowheadSize = checkProp(
    path,
    "endArrowheadSize",
    trans,
    checkFloatV,
  );
  if (endArrowheadSize.isErr()) return err(endArrowheadSize.error);

  const startArrowhead = checkProp(path, "startArrowhead", trans, checkStrV);
  if (startArrowhead.isErr()) return err(startArrowhead.error);

  const endArrowhead = checkProp(path, "endArrowhead", trans, checkStrV);
  if (endArrowhead.isErr()) return err(endArrowhead.error);

  const flipStartArrowhead = checkProp(
    path,
    "flipStartArrowhead",
    trans,
    checkBoolV,
  );
  if (flipStartArrowhead.isErr()) return err(flipStartArrowhead.error);

  return ok({
    startArrowheadSize: startArrowheadSize.value,
    endArrowheadSize: endArrowheadSize.value,
    startArrowhead: startArrowhead.value,
    endArrowhead: endArrowhead.value,
    flipStartArrowhead: flipStartArrowhead.value,
  });
};

export const checkCorner = (
  path: StylePathToUnindexedObject<A>,
  trans: Translation,
): Result<Corner<ad.Num>, StyleError> => {
  const cornerRadius = checkProp(path, "cornerRadius", trans, checkFloatV);
  if (cornerRadius.isErr()) return err(cornerRadius.error);

  return ok({ cornerRadius: cornerRadius.value });
};

export const checkRotate = (
  path: StylePathToUnindexedObject<A>,
  trans: Translation,
): Result<Rotate<ad.Num>, StyleError> => {
  const rotation = checkProp(path, "rotation", trans, checkFloatV);
  if (rotation.isErr()) return err(rotation.error);

  return ok({ rotation: rotation.value });
};

export const checkScale = (
  path: StylePathToUnindexedObject<A>,
  trans: Translation,
): Result<Scale<ad.Num>, StyleError> => {
  const scale = checkProp(path, "scale", trans, checkFloatV);
  if (scale.isErr()) return err(scale.error);

  return ok({ scale: scale.value });
};

export const checkPoly = (
  path: StylePathToUnindexedObject<A>,
  trans: Translation,
): Result<Poly<ad.Num>, StyleError> => {
  const points = checkProp(path, "points", trans, checkPtListV);
  if (points.isErr()) return err(points.error);

  return ok({ points: points.value });
};

export const checkString = (
  path: StylePathToUnindexedObject<A>,
  trans: Translation,
): Result<StringProps<ad.Num>, StyleError> => {
  const string = checkProp(path, "string", trans, checkStrV);
  if (string.isErr()) return err(string.error);

  const fontSize = checkProp(path, "fontSize", trans, checkStrV);
  if (fontSize.isErr()) return err(fontSize.error);

  return ok({
    string: string.value,
    fontSize: fontSize.value,
  });
};

// Not checking shapeType since shapeType is *not* in Translation.
