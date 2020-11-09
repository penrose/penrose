import { mapValues, values, pickBy, concat, zip } from "lodash";
import { mapMap } from "utils/Util";
import { valueAutodiffToNumber, mapTranslation } from "engine/EngineUtils";
import { floatVal } from "utils/OtherUtils";
import seedrandom from "seedrandom";

import { compDict, checkComp } from "contrib/Functions";

import {
  varOf,
  constOf,
  differentiable,
  numOf,
  add,
  addN,
  mul,
  sub,
  div,
  max,
  min,
  sin,
  cos,
  neg,
  squared,
  sqrt,
  inverse,
  absVal,
  gt,
  lt,
  ifCond,
  ops
} from "./Autodiff";

// For deep-cloning the translation
// Note: the translation should not have cycles! If it does, use the approach that `Optimizer` takes to `clone` (clearing the VarADs).
const clone = require('rfdc')({ proto: false, circles: false });

////////////////////////////////////////////////////////////////////////////////
// Evaluator
/**
 * NOTE: possible eval optimization:
 * - Analyze the comp graph first and mark all non-varying props or fields (including those that don't depend on varying vals) permanantly "Done".
 */

/**
 * Evaluate all shapes in the `State` by walking the `Translation` data structure, finding out all shape expressions (`FGPI` expressions computed by the Style compiler), and evaluating every property in each shape.
 * @param s the current state, which contains the `Translation` to be evaluated
 *
 * NOTE: need to manage the random seed. In the backend we delibrately discard the new random seed within each of the opt session for consistent results.
 */
export const evalShapes = (s: State): State => {
  console.log("state", s); // COMBAK: revert

  // Update the stale varyingMap from the translation
  // TODO: Evaluating the shapes for display is still done via interpretation on VarADs; not compiled
  const varyingValuesDiff = s.varyingValues.map(differentiable);
  s.varyingMap = genVaryMap(s.varyingPaths, varyingValuesDiff);
  const varyingMapList = zip(s.varyingPaths, varyingValuesDiff) as [Path, VarAD][];

  // Insert all varying vals
  const transWithVarying = insertVaryings(s.translation, varyingMapList);

  // Clone translation to use in this top-level call, because it mutates the translation while interpreting the energy function in order to cache/reuse VarAD (computation) results
  const trans = clone(transWithVarying);

  // Find out all the GPI expressions in the translation
  const shapeExprs = s.shapePaths.map(
    (p: Path) => findExpr(trans, p) as IFGPI<VarAD>
  );

  // Evaluate each of the shapes (note: the translation is mutated, not returned)
  const [shapesEvaled, transEvaled] = shapeExprs.reduce(
    ([currShapes, tr]: [Shape[], Translation], e: IFGPI<VarAD>) =>
      evalShape(e, tr, s.varyingMap, currShapes),
    [[], trans]
  );

  // Sort the shapes by ordering--note the null assertion
  const sortedShapesEvaled = s.shapeOrdering.map((name) =>
    shapesEvaled.find(({ properties }) => properties.name.contents === name)!);

  // Update the state with the new list of shapes
  // (This is a shallow copy of the state btw, not a deep copy)
  return { ...s, shapes: sortedShapesEvaled };
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

  console.error("insertVaryings", trans, varyingMap); // COMBAK remove

  return varyingMap.reduce(
    (tr: Translation, [path, val]: [Path, VarAD]) =>
      insertExpr(path, doneFloat(val), tr),
    trans
  );
};

/**
 * Given a list of objectives or constraints, evaluate all of their arguments and replace pure JS numbers with autodiff numbers.
 * @param fns A list of function expressions
 * @param trans The current translation
 * @param varyingMap Varying paths and values
 */
export const evalFns = (
  fns: Fn[],
  trans: Translation,
  varyingMap: VaryMap<VarAD>
): FnDone<VarAD>[] => fns.map((f) => evalFn(f, trans, varyingMap));

const evalFn = (
  fn: Fn,
  trans: Translation,
  varyingMap: VaryMap<VarAD>
): FnDone<VarAD> => {
  return {
    name: fn.fname,
    args: evalExprs(fn.fargs, trans, varyingMap) as ArgVal<VarAD>[],
    optType: fn.optType,
  };
};

/**
 * Evaluate all properties in a shape.
 * @param shapeExpr unevaluated shape expression, where all props are expressions
 * @param trans current translation (is a deep copy for one evaluation pass only; is mutated to cache evaluated expressions)
 * @param varyingVars varying variables and their values
 * @param shapes current list of shapes (for folding)
 *
 */
export const evalShape = (
  shapeExpr: IFGPI<VarAD>, // <number>?
  trans: Translation,
  varyingVars: VaryMap,
  shapes: Shape[]
): [Shape[], Translation] => {

  const [shapeType, propExprs] = shapeExpr.contents;

  // Make sure all props are evaluated to values instead of shapes
  const props = mapValues(propExprs, (prop: TagExpr<VarAD>): Value<number> => {

    // TODO: Refactor these cases to be more concise
    if (prop.tag === "OptEval") {
      // For display, evaluate expressions with autodiff types (incl. varying vars as AD types), then convert to numbers
      // (The tradeoff for using autodiff types is that evaluating the display step will be a little slower, but then we won't have to write two versions of all computations)
      const res: Value<VarAD> = (evalExpr(prop.contents, trans, varyingVars) as IVal<VarAD>).contents;
      const resDisplay: Value<number> = valueAutodiffToNumber(res);
      return resDisplay;
    } else if (prop.tag === "Done") {
      return valueAutodiffToNumber(prop.contents);
    } else { // Pending expressions are just converted because they get converted back to numbers later
      return valueAutodiffToNumber(prop.contents);
    }
  });

  const shape: Shape = { shapeType, properties: props };

  return [[...shapes, shape], trans];
};

/**
 * Evaluate a list of expressions.
 * @param es a list of expressions
 * @param trans current translation (is a deep copy for one evaluation pass only; is mutated to cache evaluated expressions)
 * @param varyingVars varying variables and their values
 *
 */
export const evalExprs = (
  es: Expr[],
  trans: Translation,
  varyingVars?: VaryMap<VarAD>
): ArgVal<VarAD>[] =>
  es.map((e) => evalExpr(e, trans, varyingVars));

function toFloatVal<T>(a: ArgVal<T>): T {
  if (a.tag === "Val") {
    const res = a.contents;
    if (res.tag === "FloatV") {
      return res.contents;
    } else {
      console.log("res", res);
      throw Error("Expected floating type in list");
    }
  } else {
    console.log("res", a);
    throw Error("Expected value (non-GPI) type in list");
  }
};

/**
 * Evaluate the input expression to a value.
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
  varyingVars?: VaryMap<VarAD>
): ArgVal<VarAD> => {

  console.log("evalExpr", e);

  switch (e.tag) {
    case "IntLit": {
      return { tag: "Val", contents: { tag: "IntV", contents: e.contents } }
    }

    case "StringLit": {
      return { tag: "Val", contents: { tag: "StrV", contents: e.contents } }
    }

    case "BoolLit": {
      return { tag: "Val", contents: { tag: "BoolV", contents: e.contents } }
    }

    case "AFloat": {
      if (e.contents.tag === "Vary") {
        console.error("expr", e, "trans", trans, "varyingVars", varyingVars);
        throw new Error("encountered an unsubstituted varying value");
      } else {
        const val = e.contents.contents;

        return {
          tag: "Val",
          // Fixed number is stored in translation as number, made differentiable when encountered
          contents: {
            tag: "FloatV",
            contents: constOf(val),
          },
        };
      }
    }

    case "Tuple": {
      const [e1, e2] = e.contents;
      const [val1, val2] = evalExprs([e1, e2], trans, varyingVars);

      // TODO: Is there a neater way to do this check? (`checkListElemType` in GenOptProblem.hs)
      if (val1.tag === "Val" && val2.tag === "Val") {
        if (val1.contents.tag === "FloatV" && val2.contents.tag === "FloatV") {
          return { // Value<number | VarAD>
            tag: "Val",
            contents: {
              tag: "TupV",
              contents: [val1.contents.contents,
              val2.contents.contents]
            }
          }
        } else {
          throw Error("Tuple needs to contain two Float elements");
        }
      } else {
        throw Error("Tuple needs to evaluate to two values (no GPI allowed)");
      }
    }

    case "UOp": {
      const {
        contents: [uOp, expr],
      } = e as IUOp;
      // TODO: use the type system to narrow down Value to Float and Int?
      const arg = evalExpr(expr, trans, varyingVars).contents;
      return {
        tag: "Val",
        // HACK: coerce the type for now to let the compiler finish
        contents: evalUOp(uOp, arg as IFloatV<VarAD> | IIntV<VarAD>),
      }
    }

    case "BinOp": {
      const [binOp, e1, e2] = e.contents;
      const [val1, val2] = evalExprs([e1, e2], trans, varyingVars);
      return {
        tag: "Val",
        // HACK: coerce the type for now to let the compiler finish
        contents: evalBinOp(
          binOp,
          val1.contents as Value<VarAD>,
          val2.contents as Value<VarAD>
        ),
      };
    };
    case "EPath":
      return resolvePath(e.contents, trans, varyingVars);
    case "CompApp": {
      const [fnName, argExprs] = e.contents;
      // eval all args
      const args = evalExprs(argExprs, trans, varyingVars) as ArgVal<VarAD>[];
      const argValues = args.map((a) => argValue(a));
      checkComp(fnName, args);
      // retrieve comp function from a global dict and call the function
      return { tag: "Val", contents: compDict[fnName](...argValues) };
    }

    case "Tuple": {
      const argVals = evalExprs(e.contents, trans, varyingVars);
      if (argVals.length !== 2) {
        console.log(argVals);
        throw Error("Expected tuple of length 2");
      }
      return {
        tag: "Val",
        contents: {
          tag: "TupV",
          contents: [toFloatVal(argVals[0]), toFloatVal(argVals[1])]
        }
      };
    };

    case "List": {
      const argVals = evalExprs(e.contents, trans, varyingVars);
      return {
        tag: "Val",
        contents: {
          tag: "ListV",
          contents: argVals.map(toFloatVal)
        }
      };
    };

    case "ListAccess": {
      throw Error("List access expression not (yet) supported");
    };

    case "Vector": {
      const argVals = evalExprs(e.contents, trans, varyingVars);
      return {
        tag: "Val",
        contents: {
          tag: "VectorV",
          contents: argVals.map(toFloatVal)
        }
      };
    }

    case "Matrix": {
      throw new Error(`cannot evaluate expression of type ${e.tag}`);
    }

    case "VectorAccess": {
      throw new Error(`cannot evaluate expression of type ${e.tag}`);
    }

    case "MatrixAccess": {
      throw new Error(`cannot evaluate expression of type ${e.tag}`);
    }

    default: {
      throw new Error(`cannot evaluate expression of type ${e.tag}`);
    }
  }
};

/**
 * Given a path to a field or property, resolve to a fully evaluated GPI (where all props are evaluated) or an evaluated value.
 * @param path path to a field (GPI or Expr) or a property (Expr only)
 * @param trans current translation (is a deep copy for one evaluation pass only; is mutated to cache evaluated expressions)
 * @param varyingMap list of varying variables and their values
 *
 * Looks up varying vars first
 */
export const resolvePath = (
  path: Path,
  trans: Translation,
  varyingMap?: VaryMap<VarAD>
): ArgVal<VarAD> => {
  // HACK: this is a temporary way to consistently compare paths. We will need to make varymap much more efficient
  let varyingVal = varyingMap?.get(JSON.stringify(path));

  if (varyingVal) {
    return floatVal(varyingVal);
  } else {

    if (path.tag === "AccessPath") {
      throw Error("TODO");
    }

    const gpiOrExpr = findExpr(trans, path);

    switch (gpiOrExpr.tag) {
      case "FGPI": {
        const [type, props] = gpiOrExpr.contents;

        // Evaluate GPI (i.e. each property path in GPI -- NOT necessarily the path's expression)
        const evaledProps = mapValues(props, (p, propName) => {
          const propertyPath: IPropertyPath = {
            tag: "PropertyPath",
            contents: concat(path.contents, propName) as [BindingForm, string, string],
          };

          if (p.tag === "OptEval") {
            // Evaluate each property path and cache the results (so, e.g. the next lookup just returns a Value)
            // `resolve path A.val.x = f(z, y)` ===> `f(z, y) evaluates to c` ===> 
            // `set A.val.x = r` ===> `next lookup of A.val.x yields c instead of computing f(z, y)`
            const propertyPathExpr = { tag: "EPath", contents: propertyPath } as IEPath;
            const val: Value<VarAD> = (evalExpr(propertyPathExpr, trans, varyingMap) as IVal<VarAD>).contents;
            const transNew = insertExpr(propertyPath, { tag: "Done", contents: val }, trans);
            return val;
          } else {
            // Look up in varyingMap to see if there is a fresh value
            varyingVal = varyingMap?.get(JSON.stringify(propertyPath));
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
          const res: ArgVal<VarAD> = evalExpr(expr.contents, trans, varyingMap);

          if (res.tag === "Val") {
            const transNew = insertExpr(path, { tag: "Done", contents: res.contents }, trans);
            return res;
          } else if (res.tag === "GPI") {
            throw Error("Field expression evaluated to GPI when this case was eliminated");
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

/**
 * Evaluate a binary operation such as +, -, *, /, or ^.
 * @param op a binary operater
 * @param arg the argument, must be float or int
 */
export const evalBinOp = (
  op: BinaryOp,
  v1: Value<VarAD>,
  v2: Value<VarAD>
): Value<VarAD> => {

  let returnType: "FloatV" | "IntV";
  // TODO: deal with Int ops/conversion for binops
  // res = returnType === "IntV" ? Math.floor(res) : res;

  // NOTE: need to explicitly check the types so the compiler will understand
  if (v1.tag === "FloatV" && v2.tag === "FloatV") {
    let res;

    switch (op) {
      case "BPlus":
        res = add(v1.contents, v2.contents);
        break;

      case "BMinus":
        res = sub(v1.contents, v2.contents);
        break;

      case "Multiply":
        res = mul(v1.contents, v2.contents);
        break;

      case "Divide":
        res = div(v1.contents, v2.contents);
        break;

      case "Exp":
        throw Error("Pow op unimplemented");
        // res = v1.contents.powStrict(v2.contents);
        break;
    }

    returnType = "FloatV";
    return { tag: returnType, contents: res };
  }

  else if (v1.tag === "IntV" && v2.tag === "IntV") {
    returnType = "IntV";
  }
  else {
    throw new Error(`the types of two operands to ${op} must match: ${v1.tag}, ${v2.tag}`);
  }

  return v1; // TODO hack
};

/**
 * Evaluate an unary operation such as + and -.
 * @param op an unary operater
 * @param arg the argument, must be float or int
 */
export const evalUOp = (
  op: UnaryOp,
  arg: IFloatV<VarAD> | IIntV<VarAD>
): Value<VarAD> => {

  if (arg.tag === "FloatV") {
    switch (op) {
      case "UPlus":
        throw new Error("unary plus is undefined");
      case "UMinus":
        return { ...arg, contents: neg(arg.contents) };
    }
  } else { // IntV
    switch (op) {
      case "UPlus":
        throw new Error("unary plus is undefined");
      case "UMinus":
        return { ...arg, contents: -arg.contents };
    }
  }

};

/**
 * Finds an expression in a translation given a field or property path.
 * @param trans - a translation from `State`
 * @param path - a path to an expression
 * @returns an expression
 *
 * TODO: the optional type here exist because GPI is not an expression in Style yet. It's not the most sustainable pattern w.r.t to our current way to typecasting the result using `as`.
 */
export const findExpr = (
  trans: Translation,
  path: Path
): TagExpr<VarAD> | IFGPI<VarAD> => {
  let name, field, prop;

  switch (path.tag) {
    case "FieldPath":
      [name, field] = path.contents;
      // Type cast to field expression
      const fieldExpr = trans.trMap[name.contents][field];

      if (!fieldExpr) {
        throw Error(`Could not find field '${JSON.stringify(path)}' in translation`);
      }

      switch (fieldExpr.tag) {
        case "FGPI":
          return fieldExpr;
        case "FExpr":
          return fieldExpr.contents;
      }

    case "PropertyPath":
      [name, field, prop] = path.contents;
      // Type cast to FGPI and get the properties
      const gpi = trans.trMap[name.contents][field];

      if (!gpi) {
        throw Error(`Could not find GPI '${JSON.stringify(path)}' in translation`);
      }

      switch (gpi.tag) {
        case "FExpr":
          throw new Error("field path leads to an expression, not a GPI");
        case "FGPI":
          const [, propDict] = gpi.contents;
          return propDict[prop];
      }

    case "AccessPath":
      throw Error("TODO");

  }
};

const floatValToExpr = (e: Value<VarAD>): Expr => {
  if (e.tag !== "FloatV") {
    throw Error("expected to insert vector elem of type float");
  }
  return {
    tag: "AFloat", contents: { tag: "Fix", contents: e.contents }
  };
};

/**
 * Insert an expression into the translation (mutating it), returning a reference to the mutated translation for convenience
 * @param path path to a field or property
 * @param expr new expression
 * @param initTrans initial translation
 *
 */
export const insertExpr = (
  path: Path,
  expr: TagExpr<VarAD>,
  initTrans: Translation
): Translation => {
  const trans = initTrans;
  let name, field, prop;
  switch (path.tag) {
    case "FieldPath": {
      [name, field] = path.contents;
      // NOTE: this will overwrite existing expressions
      trans.trMap[name.contents][field] = { tag: "FExpr", contents: expr };
      return trans;
    } case "PropertyPath": {
      // TODO: why do I need to typecast this path? Maybe arrays are not checked properly in TS?
      [name, field, prop] = (path as IPropertyPath).contents;
      const gpi = trans.trMap[name.contents][field] as IFGPI<VarAD>;
      const [, properties] = gpi.contents;
      properties[prop] = expr;
      return trans;
    } case "AccessPath": {
      const [innerPath, indices] = path.contents;

      switch (innerPath.tag) {
        case "FieldPath": { // a.x[0] = e
          [name, field] = innerPath.contents;
          const res = trans.trMap[name.contents][field];
          if (res.tag !== "FExpr") { throw Error("did not expect GPI in vector access"); }
          const res2 = res.contents;
          if (res2.tag !== "OptEval") { throw Error("expected OptEval"); }
          const res3 = res2.contents;
          if (res3.tag !== "Vector") { throw Error("expected Vector"); }
          const res4 = res3.contents;
          res4[indices[0]] = floatValToExpr(expr.contents);
          return trans;
        }

        case "PropertyPath": { // a.x.y[0] = e
          [name, field, prop] = (innerPath as IPropertyPath).contents;
          const gpi = trans.trMap[name.contents][field] as IFGPI<VarAD>;
          const [, properties] = gpi.contents;
          const res = properties[prop];
          if (res.tag !== "OptEval") { throw Error("expected OptEval"); }
          const res2 = res.contents;
          if (res2.tag !== "Vector") { throw Error("expected Vector"); }
          const res3 = res2.contents;
          res3[indices[0]] = floatValToExpr(expr.contents);

          return trans;
        }

        default:
          throw Error("should not have nested AccessPath in AccessPath");
      }
    }
  }
};

/**
 * Gives types to a serialized `State`
 * @param json plain object encoding `State` of the diagram
 */
export const decodeState = (json: any): State => {
  // Find out the values of varying variables
  const state = {
    ...json,
    varyingValues: json.varyingState,
    varyingState: json.varyingState,
    // translation: decodeTranslation(json.transr),
    translation: json.transr,
    shapes: json.shapesr.map(([n, props]: any) => {
      return { shapeType: n, properties: props };
    }),
    varyingMap: genVaryMap(json.varyingPaths, json.varyingState),
    params: json.paramsr,
  };
  // cache energy function
  // state.overallObjective = evalEnergyOn(state);
  seedrandom(json.rng, { global: true });
  delete state.shapesr;
  delete state.transr;
  delete state.paramsr;
  // delete state.varyingState;
  return state as State;
};

/**
 * Serialize the state to match the backend format
 * NOTE: only called on resample now
 * @param state typed `State` object
 */
export const encodeState = (state: State): any => {
  const json = {
    ...state,
    varyingState: state.varyingValues,
    paramsr: state.params, // TODO: careful about the list of variables
    transr: mapTranslation(numOf, state.translation), // Only send numbers to backend
    // NOTE: clean up all additional props and turn objects into lists
    shapesr: state.shapes
      .map(values)
      .map(([n, props]) => [n, pickBy(props, (p: any) => !p.omit)]),
  } as any;
  delete json.varyingMap;
  delete json.translation;
  delete json.varyingValues;
  delete json.shapes;
  delete json.params;
  json.paramsr.optStatus = { tag: "NewIter" };
  return json;
};

export const genVaryMap = (
  varyingPaths: Path[],
  varyingValues: VarAD[] // TODO: Distinguish between VarAD variables and constants?
) => {
  if (varyingValues.length !== varyingPaths.length) {
    console.log(varyingPaths, varyingValues);
    throw new Error("Different numbers of varying vars vs. paths: " + varyingPaths.length + ", " + varyingValues.length);
  }
  const res = new Map();
  varyingPaths.forEach((path, index) =>
    res.set(JSON.stringify(path), varyingValues[index])
  );
  return res;
};

////////////////////////////////////////////////////////////////////////////////
// Types

////////////////////////////////////////////////////////////////////////////////
// Unused functions

/**
 * Gives types to a serialized `Translation`
 * @param json plain object encoding `Translation`
 */
// const decodeTranslation = (json: any): Translation => {
//   const decodeFieldExpr = (expr: any): FieldExpr<number> => {
//     switch (expr.tag) {
//       case "FGPI":
//         const [shapeType, properties] = expr.contents;
//         return {
//           tag: "FGPI",
//           contents: [shapeType],
//         };
//       case "FExpr":
//         return expr;
//       default:
//         throw new Error(`error decoding field expression ${expr}`);
//     }
//   };

//   const trans = new Map(
//     Object.entries(json.trMap).map(([subName, es]: any) => [
//       subName,
//       new Map(
//         Object.entries(es).map(([fieldName, e]: any) => [
//           fieldName,
//           decodeFieldExpr(e),
//         ])
//       ) as FieldDict<number>,
//     ])
//   ) as TransDict<number>;

//   return {
//     warnings: json.warnings,
//     compGraph: trans,
//   };
// };
