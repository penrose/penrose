import { findShapeNames, mkPath } from "compiler/Style";
import consola, { LogLevel } from "consola";
import { checkComp, compDict } from "contrib/Functions";
import {
  dummyIdentifier,
  findExprSafe,
  insertExpr,
  isPath,
} from "engine/EngineUtils";
import { mapValues } from "lodash";
// For deep-cloning the translation
// Note: the translation should not have cycles! If it does, use the approach that `Optimizer` takes to `clone` (clearing the `ad.Num`s).
import rfdc from "rfdc";
import seedrandom from "seedrandom";
import * as ad from "types/ad";
import { A } from "types/ast";
import { ShapeAD } from "types/shape";
import { Fn, FnDone, State } from "types/state";
import { BinaryOp, Expr, Path, PropertyPath } from "types/style";
import {
  ArgVal,
  FGPI,
  FloatV,
  GPI,
  IntV,
  TagExpr,
  Translation,
  Val,
  Value,
  VectorV,
} from "types/value";
import { prettyPrintPath, zip2 } from "utils/Util";
import { ops } from "./Autodiff";
import { add, div, mul, neg, sub } from "./AutodiffFunctions";

const clone = rfdc({ proto: false, circles: false });

const log = consola.create({ level: LogLevel.Warn }).withScope("Evaluator");

// //////////////////////////////////////////////////////////////////////////////
// Evaluator
/**
 * NOTE: possible eval optimization:
 * - Analyze the comp graph first and mark all non-varying props or fields (including those that don't depend on varying vals) permanantly "Done".
 */

/**
 * Evaluate all shapes in the `State` by walking the `Translation` data structure, finding out all shape expressions (`FGPI` expressions computed by the Style compiler), and evaluating every property in each shape.
 *
 * @param s the current state, which contains the `Translation` to be evaluated
 *
 * NOTE: need to manage the random seed. In the backend we delibrately discard the new random seed within each of the opt session for consistent results.
 */
export const evalShapes = (
  rng: seedrandom.prng,
  s: State,
  xsVars: ad.Input[]
): ShapeAD[] => {
  // TODO: Evaluating the shapes for display is still done via interpretation on `ad.Num`s; not compiled

  const varyingMapList = zip2(s.varyingPaths, xsVars);

  // Insert all varying vals
  const trans = insertVaryings(clone(s.translation), varyingMapList);

  const shapePathList: [string, string][] = findShapeNames(trans);
  const shapePaths = shapePathList.map(mkPath);

  // Find out all the GPI expressions in the translation
  const shapeExprs: FGPI<ad.Num>[] = shapePaths.map(
    (p: Path<A>) => findExprSafe(trans, p) as FGPI<ad.Num>
  );

  log.info("shapePaths", shapePaths.map(prettyPrintPath));

  // Evaluate each of the shapes (note: the translation is mutated, not returned)
  const [shapesEvaled]: [ShapeAD[], Translation] = shapeExprs.reduce(
    ([currShapes, tr]: [ShapeAD[], Translation], e: FGPI<ad.Num>) =>
      evalShape(rng, e, tr, currShapes),
    [[], trans]
  );

  if (s.shapeOrdering.length < shapesEvaled.length) {
    console.error("Invalid shape layering of length", s.shapeOrdering.length);
  }

  // Sort the shapes by ordering--note the null assertion
  const sortedShapesEvaled: ShapeAD[] = s.shapeOrdering.map(
    (name) =>
      shapesEvaled.find(({ properties }) => sameName(properties.name, name))!
  );

  // Update the state with the new list of shapes
  return sortedShapesEvaled;
};

const sameName = <T>(given: Value<T>, expected: string): boolean => {
  if (given.tag !== "StrV") {
    return false;
  }
  if (typeof given.contents !== "string") {
    throw Error("expected string GPI name");
  }
  return given.contents === expected;
};

const doneFloat = (n: ad.Num): TagExpr<ad.Num> => ({
  tag: "Done",
  contents: { tag: "FloatV", contents: n },
});

/**
 * Insert a set of varying values into the translation
 *
 * @param {Translation} trans initial translation without varying values
 * @param {VaryMap} varyingMap varying paths and their values
 * @returns {Translation}
 */
export const insertVaryings = (
  trans: Translation,
  varyingMap: [Path<A>, ad.Num][]
): Translation => {
  return varyingMap.reduce(
    (tr: Translation, [path, val]: [Path<A>, ad.Num]) =>
      insertExpr(path, doneFloat(val), tr),
    trans
  );
};

/**
 * Given a list of objectives or constraints, evaluate all of their arguments and replace pure JS numbers with autodiff numbers.
 *
 * @param fns A list of function expressions
 * @param trans The current translation
 */
export const evalFns = (
  rng: seedrandom.prng,
  fns: Fn[],
  trans: Translation
): FnDone<ad.Num>[] => fns.map((f) => evalFn(rng, f, trans));

export const evalFn = (
  rng: seedrandom.prng,
  fn: Fn,
  trans: Translation
): FnDone<ad.Num> => {
  return {
    name: fn.fname,
    args: evalExprs(rng, fn.fargs, trans),
    optType: fn.optType,
  };
};

/**
 * Evaluate all properties in a shape.
 *
 * @param shapeExpr unevaluated shape expression, where all props are expressions
 * @param trans current translation (is a deep copy for one evaluation pass only; is mutated to cache evaluated expressions)
 * @param shapes current list of shapes (for folding)
 *
 */
export const evalShape = (
  rng: seedrandom.prng,
  shapeExpr: FGPI<ad.Num>,
  trans: Translation,
  shapes: ShapeAD[]
): [ShapeAD[], Translation] => {
  const [shapeType, propExprs] = shapeExpr.contents;

  // Make sure all props are evaluated to values instead of shapes
  const props = Object.fromEntries(
    Object.entries(propExprs).map(([name, prop]: [string, TagExpr<ad.Num>]): [
      string,
      Value<ad.Num>
    ] => {
      // TODO: Refactor these cases to be more concise
      switch (prop.tag) {
        case "OptEval": {
          // For display, evaluate expressions with autodiff types (incl. varying vars as AD types), then convert to numbers
          // (The tradeoff for using autodiff types is that evaluating the display step will be a little slower, but then we won't have to write two versions of all computations)
          const res: Value<ad.Num> = (evalExpr(
            rng,
            prop.contents,
            trans
          ) as Val<ad.Num>).contents;
          return [name, res];
        }
        case "Done": {
          return [name, prop.contents];
        }
        case "Pending": {
          // Pending expressions are just converted because they get converted back to numbers later
          return [name, prop.contents];
        }
      }
    })
  );

  const shape: ShapeAD = { shapeType, properties: props };

  return [[...shapes, shape], trans];
};

/**
 * Evaluate a list of expressions.
 *
 * @param es a list of expressions
 * @param trans current translation (is a deep copy for one evaluation pass only; is mutated to cache evaluated expressions)
 *
 */
export const evalExprs = (
  rng: seedrandom.prng,
  es: Expr<A>[],
  trans: Translation,
  optDebugInfo?: ad.OptDebugInfo
): ArgVal<ad.Num>[] => es.map((e) => evalExpr(rng, e, trans));

function toFloatVal(a: ArgVal<ad.Num>): ad.Num {
  if (a.tag === "Val") {
    const res = a.contents;
    if (res.tag === "FloatV") {
      return res.contents;
    } else if (res.tag === "IntV") {
      return res.contents;
    } else {
      console.log("res", res);
      throw Error("Expected floating type in list");
    }
  } else {
    console.log("res", a);
    throw Error("Expected value (non-GPI) type in list");
  }
}

function toVecVal<T>(a: ArgVal<T>): T[] {
  if (a.tag === "Val") {
    const res = a.contents;
    if (res.tag === "VectorV") {
      return res.contents;
    } else {
      console.log("res", res);
      throw Error("Expected vector type in list");
    }
  } else {
    console.log("res", a);
    throw Error("Expected value (non-GPI) type in list");
  }
}

/**
 * Evaluate the input expression to a value.
 *
 * @param e the expression to be evaluated.
 * @param trans the `Translation` so far (is a deep copy for one evaluation pass only; is mutated to cache evaluated expressions)
 *
 * NOTE: This implementation needs the `Done` status of the values for optimizing evaluation and breaking cycles
 * TODO: maybe use a more OOP approach to encode current value and done status
 * TODO: break cycles; optimize lookup
 */
export const evalExpr = (
  rng: seedrandom.prng,
  e: Expr<A>,
  trans: Translation
): ArgVal<ad.Num> => {
  // console.log("evalExpr", e);

  switch (e.tag) {
    // COMBAK: Deal with ints
    // case "IntLit": {
    //   return { tag: "Val", contents: { tag: "IntV", contents: e.contents } };
    // }

    case "StringLit": {
      return { tag: "Val", contents: { tag: "StrV", contents: e.contents } };
    }

    case "BoolLit": {
      return { tag: "Val", contents: { tag: "BoolV", contents: e.contents } };
    }

    case "VaryAD": {
      // This ad.Num had already been converted to an Expr as it was inserted in the translation during accesspath initialization; they are only generated by `floatValToExpr`. Now we just convert it back to a value.
      return { tag: "Val", contents: { tag: "FloatV", contents: e.contents } };
    }

    case "Vary": {
      console.error("expr", e, "trans", trans);
      throw new Error("encountered an unsubstituted varying value");
    }

    case "VaryInit": {
      console.error("expr", e, "trans", trans);
      throw new Error("encountered an unsubstituted varying (with init) value");
    }

    case "Fix": {
      const val = e.contents;

      // Don't convert to ad.Num if it's already been converted
      return {
        tag: "Val",
        // Fixed number is stored in translation as number, made differentiable when encountered
        contents: {
          tag: "FloatV",
          contents: val,
        },
      };
    }

    case "UOp": {
      const [uOp, expr] = [e.op, e.arg];
      // TODO: use the type system to narrow down Value to Float and Int?
      const arg = evalExpr(rng, expr, trans).contents;
      return {
        tag: "Val",
        // HACK: coerce the type for now to let the compiler finish
        contents: evalUOp(uOp, arg as FloatV<ad.Num> | IntV),
      };
    }

    case "BinOp": {
      const [binOp, e1, e2] = [e.op, e.left, e.right];

      const [val1, val2] = evalExprs(rng, [e1, e2], trans);

      const res = evalBinOp(
        binOp,
        val1.contents as Value<ad.Num>,
        val2.contents as Value<ad.Num>
      );

      return {
        tag: "Val",
        // HACK: coerce the type for now to let the compiler finish
        contents: res,
      };
    }

    case "Tuple": {
      const argVals = evalExprs(rng, e.contents, trans);
      if (argVals.length !== 2) {
        console.log(argVals);
        throw Error("Expected tuple of length 2");
      }
      return {
        tag: "Val",
        contents: {
          tag: "TupV",
          contents: [toFloatVal(argVals[0]), toFloatVal(argVals[1])],
        },
      };
    }
    case "List": {
      const argVals = evalExprs(rng, e.contents, trans);

      // The below code makes a type assumption about the whole list, based on the first elements
      // Is there a better way to implement parametric lists in typescript?
      if (!argVals[0]) {
        // Empty list
        return {
          tag: "Val",
          contents: {
            tag: "ListV",
            contents: [],
          },
        };
      }

      if (argVals[0].tag === "Val") {
        // List contains floats
        if (argVals[0].contents.tag === "FloatV") {
          return {
            tag: "Val",
            contents: {
              tag: "ListV",
              contents: argVals.map(toFloatVal),
            },
          };
        } else if (argVals[0].contents.tag === "VectorV") {
          // List contains vectors
          return {
            tag: "Val",
            contents: {
              tag: "LListV", // NOTE: The type has changed from ListV to LListV! That's because ListV's `T` is "not parametric enough" to represent a list of elements
              contents: argVals.map(toVecVal),
            },
          };
        } else {
          throw Error("unknown tag");
        }
      } else {
        console.error("list elems", argVals);
        throw Error("unsupported element in list");
      }
    }

    case "ListAccess": {
      throw Error("List access expression not (yet) supported");
    }

    case "Vector": {
      const argVals = evalExprs(rng, e.contents, trans);

      // Matrices are parsed as a list of vectors, so when we encounter vectors as elements, we assume it's a matrix, and convert it to a matrix of lists
      if (argVals[0].tag !== "Val") {
        throw Error("expected val");
      }
      if (argVals[0].contents.tag === "VectorV") {
        return {
          tag: "Val",
          contents: {
            tag: "MatrixV",
            contents: argVals.map(toVecVal),
          },
        };
      }

      // Otherwise return vec (list of nums)
      return {
        tag: "Val",
        contents: {
          tag: "VectorV",
          contents: argVals.map(toFloatVal),
        },
      };
    }

    case "Matrix": {
      // This tag is here, but actually, matrices are parsed as lists of vectors, so this case is never hit
      console.log("matrix e", e);
      throw new Error(`cannot evaluate expression of type ${e.tag}`);
    }

    case "VectorAccess": {
      // COMBAK: Deprecate Vector/MatrixAccess
      // throw Error("deprecated");

      const [e1, e2] = e.contents;

      const v1 = resolvePath(rng, e1, trans);
      const v2 = evalExpr(rng, e2, trans);

      if (v1.tag !== "Val") {
        throw Error("expected val");
      }
      if (v2.tag !== "Val") {
        throw Error("expected val");
      }

      // COMBAK: Do float to int conversion in a more principled way. For now, convert float to int on demand
      if (v2.contents.tag === "FloatV") {
        // COMBAK: (ISSUE): Indices should not have "Fix"
        const iObj: ad.Num = v2.contents.contents;
        if (typeof iObj !== "number") {
          throw Error("only constant vector element indices are supported");
        }
        v2.contents = {
          tag: "IntV",
          contents: Math.floor(iObj),
        };
      }

      if (v2.contents.tag !== "IntV") {
        throw Error("expected int");
      }

      const i: number = v2.contents.contents;

      // LList access (since any expr with brackets is parsed as a vector access
      if (v1.contents.tag === "LListV") {
        const llist = v1.contents.contents;
        if (i < 0 || i > llist.length) throw Error("access out of bounds");
        return { tag: "Val", contents: { tag: "VectorV", contents: llist[i] } };
      }

      // Vector access
      if (v1.contents.tag !== "VectorV") {
        console.log("results", v1, v2);
        throw Error("expected Vector");
      }
      const vec = v1.contents.contents;
      if (i < 0 || i > vec.length) throw Error("access out of bounds");

      return {
        tag: "Val",
        contents: { tag: "FloatV", contents: vec[i] },
      };
    }

    case "MatrixAccess": {
      // COMBAK: Deprecate Vector/MatrixAccess
      // throw Error("deprecated");

      const [e1, e2] = e.contents;
      const v1 = resolvePath(rng, e1, trans);
      const v2s = evalExprs(rng, e2, trans);

      const indices: number[] = v2s.map((v2) => {
        if (v2.tag !== "Val") {
          throw Error("expected val");
        }
        if (v2.contents.tag !== "IntV") {
          throw Error("expected int");
        }
        return v2.contents.contents;
      });

      if (v1.tag !== "Val") {
        throw Error("expected val");
      }

      if (v1.contents.tag !== "MatrixV") {
        throw Error("expected Matrix");
      }

      // m[i][j] <-- m's ith row, jth column
      const mat = v1.contents.contents;
      // TODO: Currently only supports 2D matrices
      if (!indices.length || indices.length !== 2) {
        throw Error("expected 2 indices to access matrix");
      }

      const [i, j] = indices;
      if (i < 0 || i > mat.length - 1) throw Error("`i` access out of bounds");
      const vec = mat[i];
      if (j < 0 || j > vec.length - 1) throw Error("`j` access out of bounds");

      return {
        tag: "Val",
        contents: { tag: "FloatV", contents: vec[j] },
      };
    }

    case "CompApp": {
      const [fnName, argExprs] = [e.name.value, e.args];

      // eval all args
      const args = evalExprs(rng, argExprs, trans);
      const argValues = args.map((a) => argValue(a));
      checkComp(fnName, args);

      // retrieve comp function from a global dict and call the function
      return { tag: "Val", contents: compDict[fnName]({ rng }, ...argValues) };
    }

    default: {
      if (isPath(e)) {
        return resolvePath(rng, e, trans);
      }

      throw new Error(`cannot evaluate expression of type ${e.tag}`);
    }
  }
};

/**
 * Given a path to a field or property, resolve to a fully evaluated GPI (where all props are evaluated) or an evaluated value.
 *
 * @param path path to a field (GPI or Expr) or a property (Expr only)
 * @param trans current translation (is a deep copy for one evaluation pass only; is mutated to cache evaluated expressions)
 *
 * Looks up varying vars first
 */
export const resolvePath = (
  rng: seedrandom.prng,
  path: Path<A>,
  trans: Translation,
  optDebugInfo?: ad.OptDebugInfo
): ArgVal<ad.Num> => {
  // NOTE: a VectorAccess or MatrixAccess to varying variables isn't looked up in the varying paths (since `VectorAccess`, etc. in `evalExpr` don't call `resolvePath`; it works because varying vars are inserted into the translation (see `evalEnergyOn`)
  if (path.tag === "AccessPath") {
    // Evaluate it as Vector or Matrix access as appropriate (COMBAK: Deprecate Vector/Matrix access on second pass, and also remove Vector/Matrix conversion to accesspath for derivative debugging computations)
    if (path.indices.length === 1) {
      // Vector
      const e: Expr<A> = {
        ...path,
        tag: "VectorAccess",
        contents: [path.path, path.indices[0]],
      };
      return evalExpr(rng, e, trans);
    } else if (path.indices.length === 2) {
      // Matrix
      const e: Expr<A> = {
        ...path,
        tag: "MatrixAccess",
        contents: [path.path, path.indices],
      };
      return evalExpr(rng, e, trans);
    } else throw Error("unsupported number of indices in AccessPath");
  }

  if (path.tag === "InternalLocalVar" || path.tag === "LocalVar") {
    throw Error("should not encounter local var in evaluation");
  }

  const gpiOrExpr = findExprSafe(trans, path);

  switch (gpiOrExpr.tag) {
    case "FGPI": {
      const [type, props] = gpiOrExpr.contents;

      // Evaluate GPI (i.e. each property path in GPI -- NOT necessarily the path's expression)
      const evaledProps = mapValues(props, (p, propName) => {
        const propertyPath: PropertyPath<A> = {
          ...path,
          tag: "PropertyPath",
          name: path.name,
          field: path.field,
          property: dummyIdentifier(propName, "SyntheticStyle"),
        };

        if (p.tag === "OptEval") {
          // Evaluate each property path and cache the results (so, e.g. the next lookup just returns a Value)
          // `resolve path A.val.x = f(z, y)` ===> `f(z, y) evaluates to c` ===>
          // `set A.val.x = r` ===> `next lookup of A.val.x yields c instead of computing f(z, y)`
          const val: Value<ad.Num> = (evalExpr(
            rng,
            propertyPath,
            trans
          ) as Val<ad.Num>).contents;
          insertExpr(propertyPath, { tag: "Done", contents: val }, trans);
          return val;
        } else {
          return p.contents;
        }
      });

      // No need to cache evaluated GPI as each of its individual properties should have been cached on evaluation
      return {
        tag: "GPI",
        contents: [type, evaledProps] as GPI<ad.Num>["contents"],
      };
    }

    // Otherwise, either evaluate or return the expression
    default: {
      const expr: TagExpr<ad.Num> = gpiOrExpr;

      if (expr.tag === "OptEval") {
        // Evaluate the expression and cache the results (so, e.g. the next lookup just returns a Value)
        const res: ArgVal<ad.Num> = evalExpr(rng, expr.contents, trans);

        if (res.tag === "Val") {
          insertExpr(path, { tag: "Done", contents: res.contents }, trans);
          return res;
        } else if (res.tag === "GPI") {
          throw Error(
            "Field expression evaluated to GPI when this case was eliminated"
          );
        } else {
          throw Error("Unknown tag");
        }
      } else if (expr.tag === "Done" || expr.tag === "Pending") {
        // Already done, just return results of lookup -- this is a cache hit
        return { tag: "Val", contents: expr.contents };
      } else {
        throw Error("Unexpected tag");
      }
    }
  }
};

// HACK: remove the type wrapper for the argument
export const argValue = (
  e: ArgVal<ad.Num>
): (GPI<ad.Num> | Value<ad.Num>)["contents"] => {
  switch (e.tag) {
    case "GPI": // strip the `GPI` tag
      return e.contents;
    case "Val": // strip both `Val` and type annotation like `FloatV`
      return e.contents.contents;
  }
};

export const intToFloat = (v: IntV): FloatV<ad.Num> => {
  return { tag: "FloatV", contents: v.contents };
};

/**
 * Evaluate a binary operation such as +, -, *, /, or ^.
 *
 * @param op a binary operater
 * @param arg the argument, must be float or int
 */
export const evalBinOp = (
  op: BinaryOp,
  v1: Value<ad.Num>,
  v2: Value<ad.Num>
): Value<ad.Num> => {
  // Promote int to float
  if (v1.tag === "IntV" && v2.tag === "FloatV") {
    return evalBinOp(op, intToFloat(v1), v2);
  } else if (v1.tag === "FloatV" && v2.tag === "IntV") {
    return evalBinOp(op, v1, intToFloat(v2));
  }

  // NOTE: need to explicitly check the types so the compiler will understand
  if (v1.tag === "FloatV" && v2.tag === "FloatV") {
    let res;

    switch (op) {
      case "BPlus": {
        res = add(v1.contents, v2.contents);
        break;
      }

      case "BMinus": {
        res = sub(v1.contents, v2.contents);
        break;
      }

      case "Multiply": {
        res = mul(v1.contents, v2.contents);
        break;
      }

      case "Divide": {
        res = div(v1.contents, v2.contents);
        break;
      }

      case "Exp": {
        throw Error("Pow op unimplemented");
        // res = v1.contents.powStrict(v2.contents);
        break;
      }
    }

    return { tag: "FloatV", contents: res };
  } else if (v1.tag === "IntV" && v2.tag === "IntV") {
    let res;

    switch (op) {
      case "BPlus": {
        res = v1.contents + v2.contents;
        break;
      }

      case "BMinus": {
        res = v1.contents - v2.contents;
        break;
      }

      case "Multiply": {
        res = v1.contents * v2.contents;
        break;
      }

      case "Divide": {
        res = v1.contents / v2.contents;
        return { tag: "FloatV", contents: res };
      }

      case "Exp": {
        res = Math.pow(v1.contents, v2.contents);
        break;
      }
    }

    return { tag: "IntV", contents: res };
  } else if (v1.tag === "VectorV" && v2.tag === "VectorV") {
    let res: ad.Num[] | undefined;

    switch (op) {
      case "BPlus": {
        res = ops.vadd(v1.contents, v2.contents);
        break;
      }

      case "BMinus": {
        res = ops.vsub(v1.contents, v2.contents);
        break;
      }
    }

    return { tag: "VectorV", contents: res! };
  } else if (v1.tag === "FloatV" && v2.tag === "VectorV") {
    let res: ad.Num[] | undefined;

    switch (op) {
      case "Multiply": {
        res = ops.vmul(v1.contents, v2.contents);
        break;
      }
    }
    return { tag: "VectorV", contents: res! };
  } else if (v1.tag === "VectorV" && v2.tag === "FloatV") {
    let res: ad.Num[] | undefined;

    switch (op) {
      case "Divide": {
        res = ops.vdiv(v1.contents, v2.contents);
        break;
      }

      case "Multiply": {
        res = ops.vmul(v2.contents, v1.contents);
        break;
      }
    }

    return { tag: "VectorV", contents: res! };
  } else if (v1.tag === "StrV" && v2.tag === "StrV") {
    switch (op) {
      case "BPlus":
        return { tag: "StrV", contents: v1.contents + v2.contents };
    }
  } else {
    throw new Error(
      `the types of two operands to ${op} are not supported: ${v1.tag}, ${v2.tag}`
    );
  }

  return v1; // TODO hack
};

/**
 * Evaluate an unary operation such as + and -.
 *
 * @param op an unary operater
 * @param arg the argument, must be float or int
 */
export const evalUOp = (
  op: "UMinus", // this line will cause a type error if the UnaryOp type changes
  arg: FloatV<ad.Num> | IntV | VectorV<ad.Num>
): Value<ad.Num> => {
  switch (arg.tag) {
    case "FloatV": {
      return { ...arg, contents: neg(arg.contents) };
    }
    case "IntV": {
      return { ...arg, contents: -arg.contents };
    }
    case "VectorV": {
      return { ...arg, contents: ops.vneg(arg.contents) };
    }
  }
};

// Generate a map from paths to values, where the key is the JSON stringified version of the path
export function genPathMap<T>(
  paths: Path<A>[],
  vals: T[] // TODO: Distinguish between ad.Num variables and constants?
): Map<string, T> {
  if (!paths || !vals) {
    return new Map(); // Empty, e.g. when the state is decoded, there is no gradient
  }

  if (vals.length !== paths.length) {
    console.log(paths, vals);
    throw new Error(
      "Different numbers of varying vars vs. paths: " +
        paths.length +
        ", " +
        vals.length
    );
  }
  const res = new Map();
  paths.forEach((path, index) => res.set(prettyPrintPath(path), vals[index]));
  return res;
}
