import * as ad from "../types/ad.js";
import { A } from "../types/ast.js";
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
import { CompApp, ConstrFn, ObjFn } from "../types/style.js";
import { Resolved, ResolvedExpr } from "../types/stylePathResolution.js";
import { ArgValWithExpr, Value } from "../types/value.js";
import {
  badArgumentTypeError,
  functionInternalError,
  missingArgumentError,
  tooManyArgumentsError,
} from "../utils/Error.js";
import { checkType } from "./StyleTypeChecker.js";

const { ok, err } = Result;

export const callCompFunc = (
  context: Context,
  callExpr: Resolved<CompApp<A>>,
  func: CompFunc,
  args: ArgValWithExpr<ad.Num>[],
): Result<MayWarn<Value<ad.Num>>, StyleError> => {
  const checkedArgs = checkArgs(callExpr, func, args);
  if (checkedArgs.isErr()) return err(checkedArgs.error);
  try {
    const { value, warnings } = func.body(context, ...checkedArgs.value);
    return ok({
      value,
      warnings: warnings.map((w) => {
        // Attach location information to the top of `BBoxApproximationWarnings`
        // since only the top stack element is resulted from the user call to Style function
        if (w.tag === "BBoxApproximationWarning") {
          w.stack[w.stack.length - 1].callExpression = callExpr;
        }
        return w;
      }),
    });
  } catch (e) {
    if (e instanceof Error) {
      return err(
        functionInternalError(
          {
            name: func.name,
            description: func.description,
            params: func.params,
          },
          callExpr,
          e.message,
        ),
      );
    } else {
      throw new Error("Function call resulted in exception not of Error type");
    }
  }
};

export const callObjConstrFunc = (
  callExpr: Resolved<ObjFn<A> | ConstrFn<A>>,
  func: ObjFunc | ConstrFunc,
  args: ArgValWithExpr<ad.Num>[],
): Result<MayWarn<ad.Num>, StyleError> => {
  const checkedArgs = checkArgs(callExpr, func, args);
  if (checkedArgs.isErr()) return err(checkedArgs.error);
  try {
    const { value, warnings } = func.body(...checkedArgs.value);
    return ok({
      value,
      warnings: warnings.map((w) => {
        // Attach location information to the top of `BBoxApproximationWarnings`
        // since only the top stack element is resulted from the user call to Style function
        if (w.tag === "BBoxApproximationWarning") {
          w.stack[w.stack.length - 1].callExpression = callExpr;
        }
        return w;
      }),
    });
  } catch (e) {
    if (e instanceof Error) {
      return err(
        functionInternalError(
          {
            name: func.name,
            description: func.description,
            params: func.params,
          },
          callExpr,
          e.message,
        ),
      );
    } else {
      throw new Error("Function call resulted in exception not of Error type");
    }
  }
};

export const checkArgs = (
  callExpr: ResolvedExpr<A>,
  func: ObjFunc | ConstrFunc | CompFunc,
  args: ArgValWithExpr<ad.Num>[],
): Result<(Shape<ad.Num> | Value<ad.Num>["contents"])[], StyleError> => {
  if (args.length > func.params.length) {
    return err(
      tooManyArgumentsError(
        {
          name: func.name,
          description: func.description,
          params: func.params,
        },
        callExpr,
        args.length,
      ),
    );
  }
  const vals: (Shape<ad.Num> | Value<ad.Num>["contents"])[] = [];
  for (let i = 0; i < func.params.length; i++) {
    const formalArg = func.params[i];
    const actualArg: ArgValWithExpr<ad.Num> | undefined = args[i];
    const v = checkArg(callExpr, func.name, formalArg, actualArg);
    if (v.isErr()) return err(v.error);
    vals.push(v.value);
  }
  return ok(vals);
};

export const checkArg = (
  callExpr: ResolvedExpr<A>,
  funcName: string,
  formalArg: FuncParam,
  actualArg: ArgValWithExpr<ad.Num> | undefined,
): Result<Shape<ad.Num> | Value<ad.Num>["contents"], StyleError> => {
  // If the argument is not provided
  if (!actualArg) {
    // But if the argument has a default value
    if (formalArg.default !== undefined) {
      // Use the default value.
      return ok(formalArg.default);
    } else {
      // Otherwise report error.
      return err(missingArgumentError(funcName, formalArg, callExpr));
    }
  }

  // The argument is provided.
  const result = checkType(formalArg.type, actualArg);
  if (result !== undefined) return ok(result);
  return err(badArgumentTypeError(funcName, formalArg, actualArg));
};
