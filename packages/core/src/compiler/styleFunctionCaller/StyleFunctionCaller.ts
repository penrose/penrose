import * as ad from "../../types/ad";
import { SourceRange } from "../../types/ast";
import {
  CompFunc,
  ConstrFunc,
  FuncParam,
  ObjFunc,
} from "../../types/functions";

import { Result } from "true-myth";
import { Context } from "../../shapes/Samplers";
import { Shape } from "../../shapes/Shapes";
import { StyleError } from "../../types/errors";
import { ArgValWithSourceLoc, Value } from "../../types/value";
import {
  badArgumentTypeError,
  functionInternalError,
  missingArgumentError,
  tooManyArgumentsError,
} from "../../utils/Error";
import { checkType } from "../styleTypeChecker/styleTypeChecker";

const { ok, err } = Result;

export const callCompFunc = (
  func: CompFunc,
  range: SourceRange,
  context: Context,
  args: ArgValWithSourceLoc<ad.Num>[]
): Result<Value<ad.Num>, StyleError> => {
  const checkedArgs = checkArgs(func, range, args);
  if (checkedArgs.isErr()) return err(checkedArgs.error);
  try {
    return ok(func.body(context, ...checkedArgs.value));
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
): Result<ad.Num, StyleError> => {
  const checkedArgs = checkArgs(func, range, args);
  if (checkedArgs.isErr()) return err(checkedArgs.error);
  try {
    return ok(func.body(...checkedArgs.value));
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
