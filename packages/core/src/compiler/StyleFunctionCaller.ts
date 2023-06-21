import * as ad from "../types/ad.js";
import { SourceRange } from "../types/ast.js";
import {
  CompFunc,
  ConstrFunc,
  FuncParam,
  MayWarn,
  ObjFunc,
} from "../types/functions.js";

import { Result } from "true-myth";
import { Context } from "../shapes/Samplers.js";
import { Shape } from "../shapes/Shapes.js";
import { StyleError } from "../types/errors.js";
import { ArgValWithSourceLoc, Value } from "../types/value.js";
import {
  badArgumentTypeError,
  functionInternalError,
  missingArgumentError,
  tooManyArgumentsError,
} from "../utils/Error.js";
import { checkType } from "./StyleTypeChecker.js";

const { ok, err } = Result;

export const callCompFunc = (
  func: CompFunc,
  range: SourceRange,
  context: Context,
  args: ArgValWithSourceLoc<ad.Num>[]
): Result<MayWarn<Value<ad.Num>>, StyleError> => {
  const checkedArgs = checkArgs(func, range, args);
  if (checkedArgs.isErr()) return err(checkedArgs.error);
  try {
    const { value, warnings } = func.body(context, ...checkedArgs.value);
    return ok({
      value,
      warnings: warnings.map((w) => ({ ...w, location: range })),
    });
  } catch (e) {
    if (e instanceof Error) {
      return err(functionInternalError(func, range, e.message));
    } else {
      throw new Error("Function call resulted in exception not of Error type");
    }
  }
};

export const callObjConstrFunc = (
  func: ObjFunc | ConstrFunc,
  range: SourceRange,
  args: ArgValWithSourceLoc<ad.Num>[]
): Result<MayWarn<ad.Num>, StyleError> => {
  const checkedArgs = checkArgs(func, range, args);
  if (checkedArgs.isErr()) return err(checkedArgs.error);
  try {
    const { value, warnings } = func.body(...checkedArgs.value);
    return ok({
      value,
      warnings: warnings.map((w) => ({ ...w, location: range })),
    });
  } catch (e) {
    if (e instanceof Error) {
      return err(functionInternalError(func, range, e.message));
    } else {
      throw new Error("Function call resulted in exception not of Error type");
    }
  }
};

export const checkArgs = (
  func: ObjFunc | ConstrFunc | CompFunc,
  range: SourceRange,
  args: ArgValWithSourceLoc<ad.Num>[]
): Result<(Shape<ad.Num> | Value<ad.Num>["contents"])[], StyleError> => {
  if (args.length > func.params.length) {
    return err(tooManyArgumentsError(func, range, args.length));
  }
  const vals: (Shape<ad.Num> | Value<ad.Num>["contents"])[] = [];
  for (let i = 0; i < func.params.length; i++) {
    const funcArg = func.params[i];
    const arg: ArgValWithSourceLoc<ad.Num> | undefined = args[i];
    const v = checkArg(func.name, range, funcArg, arg);
    if (v.isErr()) return err(v.error);
    vals.push(v.value);
  }
  return ok(vals);
};

export const checkArg = (
  funcName: string,
  location: SourceRange,
  funcArg: FuncParam,
  arg: ArgValWithSourceLoc<ad.Num> | undefined
): Result<Shape<ad.Num> | Value<ad.Num>["contents"], StyleError> => {
  // If the argument is not provided
  if (!arg) {
    // But if the argument has a default value
    if (funcArg.default !== undefined) {
      // Use the default value.
      return ok(funcArg.default);
    } else {
      // Otherwise report error.
      return err(missingArgumentError(funcName, funcArg, location));
    }
  }

  // The argument is provided.
  const result = checkType(funcArg.type, arg);
  if (result !== undefined) return ok(result);
  return err(badArgumentTypeError(funcName, funcArg, arg));
};
