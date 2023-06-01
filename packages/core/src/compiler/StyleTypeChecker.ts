import { Shape, ShapeType } from "../shapes/Shapes.js";
import * as ad from "../types/ad.js";
import { ValueShapeT, ValueType } from "../types/types.js";
import { ArgVal, Value } from "../types/value.js";

export const checkType = (
  expectedType: ValueShapeT,
  arg: ArgVal<ad.Num>
): Shape<ad.Num> | Value<ad.Num>["contents"] | undefined => {
  if (expectedType.tag === "UnionT") {
    for (const t of expectedType.types) {
      const result = checkType(t, arg);
      if (result !== undefined) return result;
    }
  } else if (expectedType.tag === "ShapeT") {
    const shape = checkShape(expectedType.type, arg);
    if (shape !== undefined) return shape;
  } else {
    const value = checkArgValAgainstValueType(expectedType.type, arg);
    if (value !== undefined) return value;
  }
  return undefined;
};

export const checkShape = (
  expectedType: ShapeType | "AnyShape",
  arg: ArgVal<ad.Num>
): Shape<ad.Num> | undefined => {
  if (expectedType === "AnyShape") {
    if (arg.tag === "ShapeVal") {
      return arg.contents;
    }
  } else {
    if (arg.tag === "ShapeVal" && arg.contents.shapeType === expectedType) {
      return arg.contents;
    }
  }
  return undefined;
};

export const checkArgValAgainstValueType = (
  expected: ValueType,
  given: ArgVal<ad.Num>
): Value<ad.Num>["contents"] | undefined => {
  if (given.tag === "ShapeVal") {
    return undefined;
  } else {
    return checkValueAgainstValueType(expected, given.contents);
  }
};

export const checkValueAgainstValueType = (
  expected: ValueType,
  given: Value<ad.Num>
): Value<ad.Num>["contents"] | undefined => {
  const { tag, contents } = given;
  if (
    expected === "Real" ||
    expected === "Unit" ||
    expected === "PosInt" ||
    expected === "Nat"
  ) {
    // Cannot really check whether something is positive of natural
    if (tag === "FloatV") return contents;
  } else if (expected === "Real2") {
    if (tag === "ListV" || tag === "VectorV" || tag === "TupV")
      if (contents.length === 2) return contents;
  } else if (expected === "Real3") {
    if (tag === "ListV" || tag === "VectorV" || tag === "TupV")
      if (contents.length === 3) return contents;
  } else if (expected === "RealN") {
    if (tag === "ListV" || tag === "VectorV" || tag === "TupV") return contents;
  } else if (expected === "Real2N") {
    if (tag === "MatrixV" || tag === "LListV" || tag === "PtListV")
      if (contents.every((row) => row.length === 2)) return contents;
  } else if (expected === "RealNM") {
    if (tag === "MatrixV" || tag === "LListV" || tag === "PtListV")
      if (contents.every((row) => row.length === contents[0].length))
        return contents;
  } else if (expected === "Color") {
    if (tag === "ColorV") return contents;
  } else if (expected === "String") {
    if (tag === "StrV") return contents;
  } else if (expected === "ColorType") {
    if (tag === "StrV" && (contents === "hsv" || contents === "rgb"))
      return contents;
  } else if (expected === "PathType") {
    if (tag === "StrV" && (contents === "open" || contents === "closed"))
      return contents;
  } else if (expected === "ShapeList") {
    if (tag === "ShapeListV") return contents;
  } else if (expected === "PathCmd") {
    if (tag === "PathDataV") return contents;
  } else {
    // type === "Boolean"
    if (tag === "BoolV") return contents;
  }

  return undefined;
};

//#endregion
