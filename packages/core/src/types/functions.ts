import { Result } from "true-myth";
import { Context } from "../shapes/Samplers";
import { Shape, ShapeType } from "../shapes/Shapes";
import * as ad from "../types/ad";
import { badArgumentError } from "../utils/Error";
import { SourceRange } from "./ast";
import { StyleError } from "./errors";
import {
  ArgValWithSourceLoc,
  ShapeT,
  UnionT,
  Value,
  ValueShapeT,
  ValueT,
  ValueType,
} from "./value";

const { ok, err } = Result;

type CompFuncBody = (context: Context, ...args: any[]) => Value<ad.Num>;
type ObjFuncBody = (...args: any[]) => ad.Num;
type ConstrFuncBody = (...args: any[]) => ad.Num;

export interface FuncArg {
  name: string;
  type: ValueShapeT;
  default?: Value<ad.Num>["contents"];
}

export interface CompFunc {
  name: string;
  funcArgs: FuncArg[];
  body: CompFuncBody;
  returns: ValueShapeT;
}

export interface ObjFunc {
  name: string;
  funcArgs: FuncArg[];
  body: ObjFuncBody;
}

export interface ConstrFunc {
  name: string;
  funcArgs: FuncArg[];
  body: ConstrFuncBody;
}

export const callCompFunc = (
  func: CompFunc,
  range: SourceRange,
  context: Context,
  args: ArgValWithSourceLoc<ad.Num>[]
): Result<Value<ad.Num>, StyleError> => {
  const vals: (Shape<ad.Num> | Value<ad.Num>["contents"])[] = [];
  for (let i = 0; i < func.funcArgs.length; i++) {
    const funcArg = func.funcArgs[i];
    const arg: ArgValWithSourceLoc<ad.Num> | undefined = args[i];
    const v = checkArg(func.name, range, funcArg, arg);
    if (v.isErr()) return err(v.error);
    vals.push(v.value);
  }
  return ok(func.body(context, ...vals));
};

export const callObjConstrFunc = (
  func: ObjFunc | ConstrFunc,
  range: SourceRange,
  args: ArgValWithSourceLoc<ad.Num>[]
): Result<ad.Num, StyleError> => {
  const vals: (Shape<ad.Num> | Value<ad.Num>["contents"])[] = [];
  for (let i = 0; i < func.funcArgs.length; i++) {
    const funcArg = func.funcArgs[i];
    const arg: ArgValWithSourceLoc<ad.Num> | undefined = args[i];
    const v = checkArg(func.name, range, funcArg, arg);
    if (v.isErr()) return err(v.error);
    vals.push(v.value);
  }
  return ok(func.body(...vals));
};

export const checkArg = (
  funcName: string,
  location: SourceRange,
  funcArg: FuncArg,
  arg: ArgValWithSourceLoc<ad.Num> | undefined
): Result<Shape<ad.Num> | Value<ad.Num>["contents"], StyleError> => {
  // If the argument is not provided
  if (!arg) {
    // But if the argument has a default value
    if (funcArg.default) {
      // Use the default value.
      return ok(funcArg.default);
    } else {
      // Otherwise report error.
      return err(badArgumentError(funcName, funcArg, location));
    }
  }

  // The argument is provided.
  return checkType(funcName, location, funcArg, funcArg.type, arg);
  /*
  const expectedType = funcArg.type;
  if (expectedType.tag === "UnionT") {
    return ok(1);
  }

  // If the argument expects any generic shape (not a specific shape)
  if (funcArg.type === "AnyShape") {
    // Check the provided argument is any shape
    if (arg.tag === "ShapeVal") {
      return ok(arg.contents);
    } else {
      return err(badArgumentError(funcName, funcArg, location, arg));
    }
  }
  // If the argument expects a specific type of shape
  if (isShapeType(funcArg.type)) {
    // Check the provided argument is also a shape and has the same shape type
    if (arg.tag === "ShapeVal" && arg.contents.shapeType === funcArg.type) {
      return ok(arg.contents);
    } else {
      return err(badArgumentError(funcName, funcArg, location, arg));
    }
  }

  // The argument now expects a value.

  // If the provided argument is a shape
  if (arg.tag === "ShapeVal") {
    return err(badArgumentError(funcName, funcArg, location, arg));
  }

  // Now the expected and provided arguments are both values.
  // Check the types of the values.
  return checkValue(funcName, location, funcArg, arg.contents, {
    start: arg.start,
    end: arg.end,
  });
  */
};

export const checkType = (
  funcName: string,
  location: SourceRange,
  funcArg: FuncArg,
  expectedType: ValueShapeT,
  arg: ArgValWithSourceLoc<ad.Num>
): Result<Shape<ad.Num> | Value<ad.Num>["contents"], StyleError> => {
  if (expectedType.tag === "UnionT") {
    for (const t of expectedType.types) {
      const result = checkType(funcName, location, funcArg, t, arg);
      if (result.isOk()) return ok(result.value);
    }
  } else if (expectedType.tag === "ShapeT") {
    const shape = checkShape(
      funcName,
      location,
      funcArg,
      expectedType.type,
      arg
    );
    if (shape.isOk()) return ok(shape.value);
  } else {
    const value = checkValue(
      funcName,
      location,
      funcArg,
      expectedType.type,
      arg
    );
    if (value.isOk()) return ok(value.value);
  }
  return err(
    badArgumentError(funcName, funcArg, location, arg, {
      start: arg.start,
      end: arg.end,
    })
  );
};

const checkShape = (
  funcName: string,
  location: SourceRange,
  funcArg: FuncArg,
  expectedType: ShapeType | "AnyShape",
  arg: ArgValWithSourceLoc<ad.Num>
): Result<Shape<ad.Num>, StyleError> => {
  if (expectedType === "AnyShape") {
    if (arg.tag === "ShapeVal") {
      return ok(arg.contents);
    }
  } else {
    if (arg.tag === "ShapeVal" && arg.contents.shapeType === expectedType) {
      return ok(arg.contents);
    }
  }

  return err(
    badArgumentError(funcName, funcArg, location, arg, {
      start: arg.start,
      end: arg.end,
    })
  );
};

export const checkValue = (
  funcName: string,
  location: SourceRange,
  funcArg: FuncArg,
  type: ValueType,
  arg: ArgValWithSourceLoc<ad.Num>
): Result<Value<ad.Num>["contents"], StyleError> => {
  if (arg.tag !== "ShapeVal") {
    const { tag, contents } = arg.contents;
    if (
      type === "Real" ||
      type === "Unit" ||
      type === "PosInt" ||
      type === "Nat"
    ) {
      // Cannot really check whether something is positive of natural
      if (tag === "FloatV") return ok(contents);
    } else if (type === "Real2") {
      if (tag === "ListV" || tag === "VectorV" || tag === "TupV")
        if (contents.length === 2) return ok(contents);
    } else if (type === "RealN") {
      if (tag === "ListV" || tag === "VectorV" || tag === "TupV")
        return ok(contents);
    } else if (type === "Real2N") {
      if (tag === "MatrixV" || tag === "LListV" || tag === "PtListV")
        if (contents.every((row) => row.length === 2)) return ok(contents);
    } else if (type === "RealNM") {
      if (tag === "MatrixV" || tag === "LListV" || tag === "PtListV")
        if (contents.every((row) => row.length === contents[0].length))
          return ok(contents);
    } else if (type === "Color") {
      if (tag === "ColorV") return ok(contents);
    } else if (type === "String") {
      if (tag === "StrV") return ok(contents);
    } else if (type === "ColorType") {
      if (tag === "StrV" && (contents === "hsv" || contents === "rgb"))
        return ok(contents);
    } else if (type === "PathType") {
      if (tag === "StrV" && (contents === "open" || contents === "closed"))
        return ok(contents);
    } else if (type === "ShapeList") {
      if (tag === "ShapeListV") return ok(contents);
    } else if (type === "PathCmd") {
      if (tag === "PathDataV") return ok(contents);
    } else {
      // type === "Boolean"
      if (tag === "BoolV") return ok(contents);
    }
  }

  return err(
    badArgumentError(funcName, funcArg, location, arg, {
      start: arg.start,
      end: arg.end,
    })
  );
};

export const valueT = (type: ValueType): ValueT => ({
  tag: "ValueT",
  type,
});

export const shapeT = (type: ShapeType | "AnyShape"): ShapeT => ({
  tag: "ShapeT",
  type,
});

export const unionT = (...types: ValueShapeT[]): UnionT => ({
  tag: "UnionT",
  types,
});
export const rectlikeT = (): UnionT =>
  unionT(
    shapeT("Equation"),
    shapeT("Image"),
    shapeT("Rectangle"),
    shapeT("Text")
  );

export const realArg = (name: string, def?: ad.Num): FuncArg => ({
  name,
  type: valueT("Real"),
  default: def,
});
export const unitArg = (name: string): FuncArg => ({
  name,
  type: valueT("Unit"),
});
export const posIntArg = (name: string): FuncArg => ({
  name,
  type: valueT("PosInt"),
});
export const natArg = (name: string): FuncArg => ({
  name,
  type: valueT("Nat"),
});
export const real2Arg = (name: string): FuncArg => ({
  name,
  type: valueT("Real2"),
});
export const realNArg = (name: string): FuncArg => ({
  name,
  type: valueT("RealN"),
});
export const real2NArg = (name: string): FuncArg => ({
  name,
  type: valueT("Real2N"),
});
export const realNMArg = (name: string): FuncArg => ({
  name,
  type: valueT("RealNM"),
});
export const colorArg = (name: string): FuncArg => ({
  name,
  type: valueT("Color"),
});
export const stringArg = (name: string): FuncArg => ({
  name,
  type: valueT("String"),
});
export const colorTypeArg = (name: string): FuncArg => ({
  name,
  type: valueT("ColorType"),
});
export const pathTypeArg = (name: string): FuncArg => ({
  name,
  type: valueT("PathType"),
});
export const booleanArg = (name: string): FuncArg => ({
  name,
  type: valueT("Boolean"),
});
export const shapeArg = (name: string, type?: ShapeType): FuncArg => ({
  name,
  type: shapeT(type ? type : "AnyShape"),
});
export const unionArg = (name: string, ...types: ValueShapeT[]): FuncArg => ({
  name,
  type: unionT(...types),
});
export const rectlikeArg = (name: string): FuncArg => ({
  name,
  type: rectlikeT(),
});

export const compFunc = (
  name: string,
  funcArgs: FuncArg[],
  body: CompFuncBody,
  returns: ValueShapeT
): CompFunc => ({
  name,
  funcArgs,
  body,
  returns,
});

export const objFunc = (
  name: string,
  funcArgs: FuncArg[],
  body: ObjFuncBody
): ObjFunc => ({
  name,
  funcArgs,
  body,
});

export const constrFunc = (
  name: string,
  funcArgs: FuncArg[],
  body: ObjFuncBody
): ConstrFunc => ({
  name,
  funcArgs,
  body,
});
