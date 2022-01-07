import consola, { LogLevel } from "consola";
import { checkComp, compDict } from "contrib/Functions";
import {
  dummyIdentifier,
  findExprSafe,
  insertExpr,
  isPath,
  valueAutodiffToNumber,
} from "engine/EngineUtils";
import { mapValues, zip } from "lodash";
// For deep-cloning the translation
// Note: the translation should not have cycles! If it does, use the approach that `Optimizer` takes to `clone` (clearing the VarADs).
import rfdc from "rfdc";
import { VarAD, OptDebugInfo, IVarAD } from "types/ad";
import { Identifier, SourceLoc } from "types/ast";
import {
  IFGPI,
  Translation,
  TagExpr,
  IVal,
  ArgVal,
  IFloatV,
  IIntV,
  ILListV,
  GPI,
  IVectorV,
} from "types/value";
import { Shape, ShapeAD } from "types/shape";
import { Value } from "types/value";
import { State, Fn, VaryMap, FnDone } from "types/state";
import { Path, Expr, IPropertyPath, BinaryOp, UnaryOp } from "types/style";
import { floatVal, prettyPrintPath } from "utils/OtherUtils";
import {
  add,
  constOf,
  constOfIf,
  differentiable,
  div,
  mul,
  neg,
  numOf,
  ops,
  sub,
} from "./Autodiff";

const clone = rfdc({ proto: false, circles: false });

const log = consola.create({ level: LogLevel.Warn }).withScope("Evaluator");

// //////////////////////////////////////////////////////////////////////////////
// Evaluator
/**
 * NOTE: possible eval optimization:
 * - Analyze the comp graph first and mark all non-varying props or fields (including those that don't depend on varying vals) permanantly "Done".
 */

// COMBAK: Import the dummy functions from Style -> EngineUtils
const dummySourceLoc = (): SourceLoc => {
  return { line: -1, col: -1 };
};

/**
 * Evaluate all shapes in the `State` by walking the `Translation` data structure, finding out all shape expressions (`FGPI` expressions computed by the Style compiler), and evaluating every property in each shape.
 *
 * @param s the current state, which contains the `Translation` to be evaluated
 *
 * NOTE: need to manage the random seed. In the backend we delibrately discard the new random seed within each of the opt session for consistent results.
 */
export const evalShapes = (s: State): ShapeAD[] => {
  // Update the stale varyingMap from the translation
  // TODO: Evaluating the shapes for display is still done via interpretation on VarADs; not compiled

  const varyingValuesDiff = s.varyingValues.map(differentiable);
  s.varyingMap = genPathMap(s.varyingPaths, varyingValuesDiff);

  const varyingMapList = zip(s.varyingPaths, varyingValuesDiff) as [
    Path,
    VarAD
  ][];

  const optDebugInfo = {
    gradient: genPathMap(s.varyingPaths, s.params.lastGradient),
    gradientPreconditioned: genPathMap(
      s.varyingPaths,
      s.params.lastGradientPreconditioned
    ),
  };

  // Insert all varying vals
  const transWithVarying = insertVaryings(s.translation, varyingMapList);

  // Clone translation to use in this top-level call, because it mutates the translation while interpreting the energy function in order to cache/reuse VarAD (computation) results
  const trans = clone(transWithVarying);

  // Find out all the GPI expressions in the translation
  const shapeExprs: IFGPI<VarAD>[] = s.shapePaths.map(
    (p: Path) => findExprSafe(trans, p) as IFGPI<VarAD>
  );

  log.info("shapePaths", s.shapePaths.map(prettyPrintPath));

  // Evaluate each of the shapes (note: the translation is mutated, not returned)
  const [shapesEvaled, transEvaled]: [
    ShapeAD[],
    Translation
  ] = shapeExprs.reduce(
    ([currShapes, tr]: [ShapeAD[], Translation], e: IFGPI<VarAD>) =>
      evalShape(e, tr, s.varyingMap, currShapes, optDebugInfo),
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

const doneFloat = (n: VarAD): TagExpr<VarAD> => ({
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
  varyingMap: [Path, VarAD][]
): Translation => {
  return varyingMap.reduce(
    (tr: Translation, [path, val]: [Path, VarAD]) =>
      insertExpr(path, doneFloat(val), tr),
    trans
  );
};

/**
 * Given a list of objectives or constraints, evaluate all of their arguments and replace pure JS numbers with autodiff numbers.
 *
 * @param fns A list of function expressions
 * @param trans The current translation
 * @param varyingMap Varying paths and values
 */
export const evalFns = (
  fns: Fn[],
  trans: Translation,
  varyingMap: VaryMap<VarAD>
): FnDone<VarAD>[] => fns.map((f) => evalFn(f, trans, varyingMap));

export const evalFn = (
  fn: Fn,
  trans: Translation,
  varyingMap: VaryMap<VarAD>
): FnDone<VarAD> => {
  const noOptDebugInfo = {
    gradient: new Map(),
    gradientPreconditioned: new Map(),
  };

  return {
    name: fn.fname,
    args: evalExprs(fn.fargs, trans, varyingMap, noOptDebugInfo),
    optType: fn.optType,
  };
};

/**
 * Evaluate all properties in a shape.
 *
 * @param shapeExpr unevaluated shape expression, where all props are expressions
 * @param trans current translation (is a deep copy for one evaluation pass only; is mutated to cache evaluated expressions)
 * @param varyingVars varying variables and their values
 * @param shapes current list of shapes (for folding)
 *
 */
export const evalShape = (
  shapeExpr: IFGPI<VarAD>,
  trans: Translation,
  varyingVars: VaryMap,
  shapes: ShapeAD[],
  optDebugInfo: OptDebugInfo
): [ShapeAD[], Translation] => {
  const [shapeType, propExprs] = shapeExpr.contents;

  // Make sure all props are evaluated to values instead of shapes
  const props = mapValues(
    propExprs,
    (prop: TagExpr<VarAD>): Value<VarAD> => {
      // TODO: Refactor these cases to be more concise
      if (prop.tag === "OptEval") {
        // For display, evaluate expressions with autodiff types (incl. varying vars as AD types), then convert to numbers
        // (The tradeoff for using autodiff types is that evaluating the display step will be a little slower, but then we won't have to write two versions of all computations)
        const res: Value<VarAD> = (evalExpr(
          prop.contents,
          trans,
          varyingVars,
          optDebugInfo
        ) as IVal<VarAD>).contents;
        return res;
      } else if (prop.tag === "Done") {
        return prop.contents;
      } else if (prop.tag === "Pending") {
        // Pending expressions are just converted because they get converted back to numbers later
        return prop.contents;
      } else {
        throw Error("unknown tag");
      }
    }
  );

  const shape: ShapeAD = { shapeType, properties: props };

  return [[...shapes, shape], trans];
};

/**
 * Evaluate a list of expressions.
 *
 * @param es a list of expressions
 * @param trans current translation (is a deep copy for one evaluation pass only; is mutated to cache evaluated expressions)
 * @param varyingVars varying variables and their values
 *
 */
export const evalExprs = (
  es: Expr[],
  trans: Translation,
  varyingVars?: VaryMap<VarAD>,
  optDebugInfo?: OptDebugInfo
): ArgVal<VarAD>[] =>
  es.map((e) => evalExpr(e, trans, varyingVars, optDebugInfo));

function toFloatVal(a: ArgVal<VarAD>): VarAD {
  if (a.tag === "Val") {
    const res = a.contents;
    if (res.tag === "FloatV") {
      return res.contents;
    } else if (res.tag === "IntV") {
      return constOf(res.contents);
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
 * @param varyingVars pairs of (path, value) for all optimized/"varying" values.
 *
 * NOTE: This implementation needs the `Done` status of the values for optimizing evaluation and breaking cycles
 * TODO: maybe use a more OOP approach to encode current value and done status
 * TODO: break cycles; optimize lookup
 */
export const evalExpr = (
  e: Expr,
  trans: Translation,
  varyingVars?: VaryMap<VarAD>,
  optDebugInfo?: OptDebugInfo
): ArgVal<VarAD> => {
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
      // This VarAD had already been converted to an Expr as it was inserted in the translation during accesspath initialization; they are only generated by `floatValToExpr`. Now we just convert it back to a value.
      return { tag: "Val", contents: { tag: "FloatV", contents: e.contents } };
    }

    case "Vary": {
      console.error("expr", e, "trans", trans, "varyingVars", varyingVars);
      throw new Error("encountered an unsubstituted varying value");
    }

    case "VaryInit": {
      console.error("expr", e, "trans", trans, "varyingVars", varyingVars);
      throw new Error("encountered an unsubstituted varying (with init) value");
    }

    case "Fix": {
      const val = e.contents;

      // Don't convert to VarAD if it's already been converted
      return {
        tag: "Val",
        // Fixed number is stored in translation as number, made differentiable when encountered
        contents: {
          tag: "FloatV",
          contents: constOfIf(val),
        },
      };
    }

    case "UOp": {
      const [uOp, expr] = [e.op, e.arg];
      // TODO: use the type system to narrow down Value to Float and Int?
      const arg = evalExpr(expr, trans, varyingVars, optDebugInfo).contents;
      return {
        tag: "Val",
        // HACK: coerce the type for now to let the compiler finish
        contents: evalUOp(uOp, arg as IFloatV<VarAD> | IIntV),
      };
    }

    case "BinOp": {
      const [binOp, e1, e2] = [e.op, e.left, e.right];

      const [val1, val2] = evalExprs(
        [e1, e2],
        trans,
        varyingVars,
        optDebugInfo
      );

      const res = evalBinOp(
        binOp,
        val1.contents as Value<VarAD>,
        val2.contents as Value<VarAD>
      );

      return {
        tag: "Val",
        // HACK: coerce the type for now to let the compiler finish
        contents: res,
      };
    }

    case "Tuple": {
      const argVals = evalExprs(e.contents, trans, varyingVars, optDebugInfo);
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
      const argVals = evalExprs(e.contents, trans, varyingVars, optDebugInfo);

      // The below code makes a type assumption about the whole list, based on the first elements
      // Is there a better way to implement parametric lists in typescript?
      if (!argVals[0]) {
        // Empty list
        return {
          tag: "Val",
          contents: {
            tag: "ListV",
            contents: [] as VarAD[],
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
            } as ILListV<VarAD>,
          };
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
      const argVals = evalExprs(e.contents, trans, varyingVars, optDebugInfo);

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

      const v1 = resolvePath(e1, trans, varyingVars, optDebugInfo);
      const v2 = evalExpr(e2, trans, varyingVars, optDebugInfo);

      if (v1.tag !== "Val") {
        throw Error("expected val");
      }
      if (v2.tag !== "Val") {
        throw Error("expected val");
      }

      // COMBAK: Do float to int conversion in a more principled way. For now, convert float to int on demand
      if (v2.contents.tag === "FloatV") {
        // COMBAK: (ISSUE): Indices should not have "Fix"
        const iObj: IVarAD = v2.contents.contents;
        v2.contents = {
          tag: "IntV",
          contents: Math.floor(numOf(iObj)),
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
      const v1 = resolvePath(e1, trans, varyingVars, optDebugInfo);
      const v2s = evalExprs(e2, trans, varyingVars, optDebugInfo);

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

      if (fnName === "derivative" || fnName === "derivativePreconditioned") {
        // Special function: don't look up the path's value, but its gradient's value

        if (argExprs.length !== 1) {
          throw Error(
            `expected 1 argument to ${fnName}; got ${argExprs.length}`
          );
        }

        let p = argExprs[0]; // special function can only have one argument

        // Vector and matrix accesses are the only way to refer to an anon varying var
        // p.tag !== "EPath"
        if (
          !isPath(p) &&
          p.tag !== "VectorAccess" &&
          p.tag !== "MatrixAccess"
        ) {
          throw Error(`expected 1 path as argument to ${fnName}; got ${p.tag}`);
        }

        if (p.tag === "VectorAccess") {
          p = {
            // convert to AccessPath schema
            nodeType: "SyntheticStyle",
            children: [],
            tag: "AccessPath",
            // contents: [p.contents[0], [p.contents[1].contents]],
            path: p.contents[0],
            indices: [p.contents[1]],
          };
        } else if (p.tag === "MatrixAccess") {
          p = {
            // convert to AccessPath schema
            nodeType: "SyntheticStyle",
            children: [],
            tag: "AccessPath",
            path: p.contents[0],
            indices: p.contents[1],
          };
        }

        return {
          tag: "Val",
          contents: compDict[fnName](
            optDebugInfo as OptDebugInfo,
            prettyPrintPath(p) // COMBAK: Test that derivatives still work
          ),
        };
      }

      // eval all args
      const args = evalExprs(argExprs, trans, varyingVars, optDebugInfo);
      const argValues = args.map((a) => argValue(a));
      checkComp(fnName, args);

      // retrieve comp function from a global dict and call the function
      return { tag: "Val", contents: compDict[fnName](...argValues) };
    }

    default: {
      if (isPath(e)) {
        return resolvePath(e, trans, varyingVars, optDebugInfo);
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
 * @param varyingMap list of varying variables and their values
 *
 * Looks up varying vars first
 */
export const resolvePath = (
  path: Path,
  trans: Translation,
  varyingMap?: VaryMap<VarAD>,
  optDebugInfo?: OptDebugInfo
): ArgVal<VarAD> => {
  // HACK: this is a temporary way to consistently compare paths. We will need to make varymap much more efficient
  let varyingVal;
  if (varyingMap) {
    varyingVal = varyingMap.get(prettyPrintPath(path));
  }

  if (varyingVal) {
    return floatVal(varyingVal);
  } else {
    // NOTE: a VectorAccess or MatrixAccess to varying variables isn't looked up in the varying paths (since `VectorAccess`, etc. in `evalExpr` don't call `resolvePath`; it works because varying vars are inserted into the translation (see `evalEnergyOn`)
    // NOTE: varyingMap includes vars of form AccessPath but not Vector/Matrix access
    if (path.tag === "AccessPath") {
      // Evaluate it as Vector or Matrix access as appropriate (COMBAK: Deprecate Vector/Matrix access on second pass, and also remove Vector/Matrix conversion to accesspath for derivative debugging computations)

      // console.log("path", path);
      // console.log("trans", trans);
      // console.log("varyingMap", varyingMap);

      if (path.indices.length === 1) {
        // Vector

        // if (path.path.tag === "FieldPath") {
        //   if (path.path.field.value === "markerLine") {
        //     debugger;
        //   }
        // }

        const e: Expr = {
          ...path,
          tag: "VectorAccess",
          contents: [path.path, path.indices[0]],
        };
        return evalExpr(e, trans, varyingMap, optDebugInfo);
      } else if (path.indices.length === 2) {
        // Matrix
        const e: Expr = {
          ...path,
          tag: "MatrixAccess",
          contents: [path.path, path.indices],
        };
        return evalExpr(e, trans, varyingMap, optDebugInfo);
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
          const propertyPath: IPropertyPath = {
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
            const propertyPathExpr = propertyPath as Path;
            const val: Value<VarAD> = (evalExpr(
              propertyPathExpr,
              trans,
              varyingMap,
              optDebugInfo
            ) as IVal<VarAD>).contents;
            const transNew = insertExpr(
              propertyPath,
              { tag: "Done", contents: val },
              trans
            );
            return val;
          } else {
            // Look up in varyingMap to see if there is a fresh value
            let varyingVal;
            if (varyingMap) {
              varyingVal = varyingMap.get(prettyPrintPath(propertyPath));
            }
            if (varyingVal) {
              return { tag: "FloatV", contents: varyingVal };
            } else {
              return p.contents;
            }
          }
        });

        // No need to cache evaluated GPI as each of its individual properties should have been cached on evaluation
        return {
          tag: "GPI",
          contents: [type, evaledProps] as GPI<VarAD>,
        };
      }

      // Otherwise, either evaluate or return the expression
      default: {
        const expr: TagExpr<VarAD> = gpiOrExpr;

        if (expr.tag === "OptEval") {
          // Evaluate the expression and cache the results (so, e.g. the next lookup just returns a Value)
          const res: ArgVal<VarAD> = evalExpr(
            expr.contents,
            trans,
            varyingMap,
            optDebugInfo
          );

          if (res.tag === "Val") {
            const transNew = insertExpr(
              path,
              { tag: "Done", contents: res.contents },
              trans
            );
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
  }
};

// HACK: remove the type wrapper for the argument
export const argValue = (e: ArgVal<VarAD>) => {
  switch (e.tag) {
    case "GPI": // strip the `GPI` tag
      return e.contents;
    case "Val": // strip both `Val` and type annotation like `FloatV`
      return e.contents.contents;
  }
};

export const intToFloat = (v: IIntV): IFloatV<VarAD> => {
  return { tag: "FloatV", contents: constOf(v.contents) };
};

/**
 * Evaluate a binary operation such as +, -, *, /, or ^.
 *
 * @param op a binary operater
 * @param arg the argument, must be float or int
 */
export const evalBinOp = (
  op: BinaryOp,
  v1: Value<VarAD>,
  v2: Value<VarAD>
): Value<VarAD> => {
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
    const returnType = "IntV";
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
        return { tag: "FloatV", contents: constOf(res) };
      }

      case "Exp": {
        res = Math.pow(v1.contents, v2.contents);
        break;
      }
    }

    return { tag: "IntV", contents: res };
  } else if (v1.tag === "VectorV" && v2.tag === "VectorV") {
    let res;

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

    return { tag: "VectorV", contents: res as VarAD[] };
  } else if (v1.tag === "FloatV" && v2.tag === "VectorV") {
    let res;

    switch (op) {
      case "Multiply": {
        res = ops.vmul(v1.contents, v2.contents);
        break;
      }
    }
    return { tag: "VectorV", contents: res as VarAD[] };
  } else if (v1.tag === "VectorV" && v2.tag === "FloatV") {
    let res;

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

    return { tag: "VectorV", contents: res as VarAD[] };
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
  op: UnaryOp,
  arg: IFloatV<VarAD> | IIntV | IVectorV<VarAD>
): Value<VarAD> => {
  if (arg.tag === "FloatV") {
    switch (op) {
      case "UMinus":
        return { ...arg, contents: neg(arg.contents) };
    }
  } else if (arg.tag === "IntV") {
    switch (op) {
      case "UMinus":
        return { ...arg, contents: -arg.contents };
    }
  } else if (arg.tag === "VectorV") {
    switch (op) {
      case "UMinus":
        return { ...arg, contents: ops.vneg(arg.contents) };
    }
  } else {
    throw Error("unary op undefined on type ${arg.tag}, op ${op}");
  }
};

// Generate a map from paths to values, where the key is the JSON stringified version of the path
export function genPathMap<T>(
  paths: Path[],
  vals: T[] // TODO: Distinguish between VarAD variables and constants?
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

  // console.log("gen path map", res);
  // throw Error("TODO");
  return res;
}
