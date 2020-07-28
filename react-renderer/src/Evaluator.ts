import {
  values,
  pickBy,
  range,
  map,
  mapKeys,
  concat,
  PropertyPath,
  zip,
} from "lodash";
import { mapValues } from "lodash";
import { dist, randFloat } from "./Util";
import seedrandom from "seedrandom";
import { Tensor, Variable, scalar, pad2d, stack } from "@tensorflow/tfjs";
import { sc, scalarValue, differentiable, evalEnergyOn } from "./Optimizer";

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
export const evalTranslation = (s: State): State => {
  // Update the stale varyingMap from the translation. TODO: Where is the right place to do this?
  s.varyingMap = genVaryMap(s.varyingPaths, s.varyingValues);
  const varyingMapList = zip(s.varyingPaths, s.varyingValues) as [Path, number][];

  // Insert all varying vals
  const trans = insertVaryings(s.translation, varyingMapList);

  // Find out all the GPI expressions in the translation
  const shapeExprs = s.shapePaths.map(
    (p: Path) => findExpr(trans, p) as IFGPI<number>
  );

  // Evaluate each of the shapes
  const [shapesEvaled, transEvaled] = shapeExprs.reduce(
    ([currShapes, tr]: [Shape[], Translation], e: IFGPI<number>) =>
      evalShape(e, tr, s.varyingMap, currShapes),
    [[], trans]
  );

  // Sort the shapes by ordering - note the null assertion
  const sortedShapesEvaled = s.shapeOrdering.map((name) =>
    shapesEvaled.find(({ properties }) => properties.name.contents === name)!);

  // Update the state with the new list of shapes and translation
  // TODO: check how deep of a copy this is by, say, changing varyingValue of the returned state and see if the argument changes
  return { ...s, shapes: sortedShapesEvaled, translation: transEvaled };
};

const doneFloat = (n: number): TagExpr<number> => ({
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
  varyingMap: [Path, number][]
): Translation => {
  return varyingMap.reduce(
    (tr: Translation, [path, val]: [Path, number]) =>
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
  varyingMap: VaryMap<Tensor>
): FnDone<Tensor>[] => fns.map((f) => evalFn(f, trans, varyingMap));

const evalFn = (
  fn: Fn,
  trans: Translation,
  varyingMap: VaryMap<Tensor>
): FnDone<Tensor> => {
  return {
    name: fn.fname,
    args: evalExprs(fn.fargs, trans, varyingMap, true) as ArgVal<Tensor>[],
    optType: fn.optType,
  };
};

/**
 * Static dictionary of computation functions
 * TODO: consider using `Dictionary` type so all runtime lookups are type-safe, like here https://codeburst.io/five-tips-i-wish-i-knew-when-i-started-with-typescript-c9e8609029db
 * TODO: think about user extension of computation dict and evaluation of functions in there
 */
const compDict = {
  rgba: (r: number, g: number, b: number, a: number): IColorV<number> => {
    return {
      tag: "ColorV",
      contents: {
        tag: "RGBA",
        contents: [r, g, b, a],
      },
    };
  },

  hsva: (h: number, s: number, v: number, a: number): IColorV<number> => {
    return {
      tag: "ColorV",
      contents: {
        tag: "HSVA",
        contents: [h, s, v, a],
      },
    };
  },

  cos: (d: number): IFloatV<number> => {
    return { tag: "FloatV", contents: Math.cos((d * Math.PI) / 180) };
  },

  sin: (d: number): IFloatV<number> => {
    return { tag: "FloatV", contents: Math.sin((d * Math.PI) / 180) };
  },

  lineLength: ([type, props]: [string, any]) => {
    const [p1, p2] = arrowPts(props);
    return { tag: "FloatV", contents: dist(p1, p2) }; // TODO: Shouldn't this be written in terms of tf.js?
  },

  len: ([type, props]: [string, any]) => {
    const [p1, p2] = arrowPts(props);
    return { tag: "FloatV", contents: dist(p1, p2) };
  },

  orientedSquare: (arr1: any, arr2: any, pt: any, len: number) => {
    console.log("orientedSquare", arr1, arr2, pt, len);

    return { tag: "PathDataV", contents: [] };
    // throw Error("test");
  },

  dot: (v: any, w: any) => {
    const [tv, tw] = [stack(v), stack(w)];
    return { tag: "FloatV", contents: tv.dot(tw) };
  },

  sampleColor: (alpha: number, colorType: string) => {
    if (colorType === "rgb") {
      const rgb = range(3).map((_) => randFloat(0.1, 0.9));
      return {
        tag: "ColorV",
        contents: {
          tag: "RGBA",
          contents: [...rgb, alpha],
        },
      };
    } else if (colorType === "hsv") {
      const h = randFloat(0, 360);
      return {
        tag: "ColorV",
        contents: {
          tag: "HSVA",
          contents: [h, 100, 80, alpha], // HACK: for the color to look good
        },
      };
    } else throw new Error("unknown color type");
  },

};

const arrowPts = ({ startX, startY, endX, endY }: Properties) =>
  [
    [startX.contents, startY.contents],
    [endX.contents, endY.contents],
  ] as [[number, number], [number, number]];

const checkComp = (fn: string, args: ArgVal<number>[]) => {
  if (!compDict[fn]) throw new Error(`Computation function "${fn}" not found`);
};

/**
 * Evaluate all properties in a shape.
 * @param shapeExpr unevaluated shape expression, where all props are expressions
 * @param trans current translation
 * @param varyingVars varying variables and their values
 * @param shapes current list of shapes (for folding)
 *
 * TODO: update trans
 */
export const evalShape = (
  shapeExpr: IFGPI<number>,
  trans: Translation,
  varyingVars: VaryMap,
  shapes: Shape[]
): [Shape[], Translation] => {

  const [shapeType, propExprs] = shapeExpr.contents;
  // Make sure all props are evaluated to values instead of shapes
  const props = mapValues(propExprs, (prop: TagExpr<number>) =>
    prop.tag === "OptEval"
      ? (evalExpr(prop.contents, trans, varyingVars, false) as IVal<number>).contents
      : prop.contents
  );

  const shape: Shape = { shapeType, properties: props };

  return [[...shapes, shape], trans];
};

/**
 * Evaluate a list of expressions.
 * @param es a list of expressions
 * @param trans current translation
 * @param varyingVars varying variables and their values
 *
 * TODO: cache translation in intermediate steps
 */
export const evalExprs = (
  es: Expr[],
  trans: Translation,
  varyingVars?: VaryMap<number | Tensor>,
  autodiff = false
): ArgVal<number | Tensor>[] =>
  es.map((e) => evalExpr(e, trans, varyingVars, autodiff));

/**
 * Evaluate the input expression to a value.
 * @param e the expression to be evaluated.
 * @param trans the `Translation` so far
 * @param varyingVars pairs of (path, value) for all optimized/"varying" values.
 *
 * NOTE: This implementation needs the `Done` status of the values for optimizing evaluation and breaking cycles
 * TODO: maybe use a more OOP approach to encode current value and done status
 * TODO: deal with translation update
 * TODO: break cycles; optimize lookup
 */
export const evalExpr = (
  e: Expr,
  trans: Translation,
  varyingVars?: VaryMap<number | Tensor>,
  autodiff = false
): ArgVal<number | Tensor> => {

  switch (e.tag) {
    case "IntLit": {
      return { tag: "Val", contents: { tag: "IntV", contents: e.contents } }
    } break;

    case "StringLit": {
      return { tag: "Val", contents: { tag: "StrV", contents: e.contents } }
    } break;

    case "BoolLit": {
      return { tag: "Val", contents: { tag: "BoolV", contents: e.contents } }
    } break
      ;
    case "AFloat": {
      if (e.contents.tag === "Vary") {
        throw new Error("encountered an unsubstituted varying value");
      } else {
        const val = e.contents.contents;
        return {
          tag: "Val",
          contents: {
            tag: "FloatV",
            contents: autodiff ? differentiable(val) : val,
          },
        };
      }
    } break;

    case "Tuple": {
      const [e1, e2] = e.contents;
      const [val1, val2] = evalExprs([e1, e2], trans, varyingVars, autodiff);

      // TODO: Is there a neater way to do this check? (`checkListElemType` in GenOptProblem.hs)
      if (val1.tag === "Val" && val2.tag === "Val") {
        if (val1.contents.tag === "FloatV" && val2.contents.tag === "FloatV") {
          return { // Value<number | Tensor>
            tag: "Val",
            contents:
            {
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
    } break;

    case "UOp": {
      const {
        contents: [uOp, expr],
      } = e as IUOp;
      // TODO: use the type system to narrow down Value to Float and Int?
      const arg = evalExpr(expr, trans, varyingVars, autodiff).contents;
      return {
        tag: "Val",
        // HACK: coerce the type for now to let the compiler finish
        contents: evalUOp(uOp, arg as IFloatV<number> | IIntV<number>),
      }
    } break;

    case "BinOp": {
      const [binOp, e1, e2] = e.contents;
      const [val1, val2] = evalExprs([e1, e2], trans, varyingVars, autodiff);
      return {
        tag: "Val",
        // HACK: coerce the type for now to let the compiler finish
        contents: evalBinOp(
          binOp,
          val1.contents as Value<number | Tensor>,
          val2.contents as Value<number | Tensor>
        ),
      }
    } break;

    case "EPath": {
      return resolvePath(e.contents, trans, varyingVars, autodiff);
    } break;

    case "CompApp": {
      const [fnName, argExprs] = e.contents;
      // eval all args
      // TODO: how should computations be written? TF numbers?
      const args = evalExprs(argExprs, trans, varyingVars, autodiff) as ArgVal<number>[];
      const argValues = args.map((a) => argValue(a));
      checkComp(fnName, args);
      // retrieve comp function from a global dict and call the function
      return { tag: "Val", contents: compDict[fnName](...argValues) };
    } break;

    default: {
      throw new Error(`cannot evaluate expression of type ${e.tag}`);
    }
  }
};

const differentiableValue = (v: Value<number | Tensor>): Value<Tensor> => {
  if ((v.tag === "FloatV" || v.tag === "IntV") && typeof v.contents === "number") {
    return { ...v, contents: differentiable(v.contents) } as
      | IFloatV<Tensor>
      | IIntV<Tensor>;
  } else return v as Value<Tensor>;
};

/**
 * Given a path to a field or property, resolve to a fully evaluated GPI (where all props are evaluated) or an evaluated value.
 * @param path path to a field (GPI or Expr) or a property (Expr only)
 * @param trans current computational graph
 * @param varyingMap list of varying variables and their values
 *
 * TODO: lookup varying vars first?
 * TODO: cache done values somewhere, maybe by mutating the translation?
 */
export const resolvePath = (
  path: Path,
  trans: Translation,
  varyingMap?: VaryMap<number | Tensor>,
  autodiff = false
): ArgVal<number | Tensor> => {
  const floatVal = (v: number | Tensor): ArgVal<Tensor | number> => ({
    tag: "Val",
    contents: {
      tag: "FloatV",
      contents: v,
    },
  });
  // HACK: this is a temporary way to consistently compare paths. We will need to make varymap much more efficient
  let varyingVal = varyingMap?.get(JSON.stringify(path));
  if (varyingVal) {
    // When we look up a varying value, it will always be an autodiff-type, so convert autodiff-types to numbers if autodiff is off (in case a rendered value depends on some arithmetic that relies on a varying value)
    if (!autodiff) { return floatVal(sc(varyingVal)); }
    return floatVal(varyingVal);
  } else {
    const gpiOrExpr = findExpr(trans, path);
    switch (gpiOrExpr.tag) {
      case "FGPI":
        const [type, props] = gpiOrExpr.contents;
        // TODO: cache results
        const evaledProps = mapValues(props, (p, propName) => {
          if (p.tag === "OptEval") {
            return (evalExpr(p.contents, trans, varyingMap, autodiff) as IVal<
              number | Tensor
            >).contents;
          } else {
            const propPath: IPropertyPath = {
              tag: "PropertyPath",
              contents: concat(path.contents, propName) as [
                BindingForm,
                string,
                string
              ], // TODO: check if this is true
            };
            varyingVal = varyingMap?.get(JSON.stringify(propPath));
            if (varyingVal) {
              return { tag: "FloatV", contents: varyingVal };
            } else {
              return autodiff ? differentiableValue(p.contents) : p.contents;
            }
          }
        });
        return {
          tag: "GPI",
          contents: [type, evaledProps] as GPI<number | Tensor>,
        };
      default:
        const expr: TagExpr<number> = gpiOrExpr;
        if (expr.tag === "OptEval") {
          return evalExpr(expr.contents, trans, varyingMap, autodiff);
        } else {
          return { tag: "Val", contents: expr.contents }
        };
    }
  }
};

// HACX: remove the type wrapper for the argument
export const argValue = (e: ArgVal<number | Tensor>) => {
  switch (e.tag) {
    case "GPI": // strip the `GPI` tag
      return e.contents;
    case "Val": // strip both `Val` and type annotation like `FloatV`
      return e.contents.contents;
  }
};

const bothTensors = (v1: IFloatV<number | Tensor>, v2: IFloatV<number | Tensor>): boolean =>
  typeof v1.contents === "number" && typeof v2.contents === "number";

/**
 * Evaluate a binary operation such as +, -, *, /, or ^.
 * @param op a binary operater
 * @param arg the argument, must be float or int
 */
export const evalBinOp = (
  op: BinaryOp,
  v1: Value<number | Tensor>,
  v2: Value<number | Tensor>
): Value<number | Tensor> => {

  let returnType: "FloatV" | "IntV";
  // TODO: deal with Int ops/conversion for binops
  // res = returnType === "IntV" ? Math.floor(res) : res;

  // NOTE: need to explicitly check the types so the compiler will understand
  if (v1.tag === "FloatV" && v2.tag === "FloatV") {
    let res;

    switch (op) {
      case "BPlus":
        if (typeof v1.contents === "number" && typeof v2.contents === "number") {
          res = v1.contents + v2.contents;
        } else if (!(typeof v1.contents === "number") && !(typeof v2.contents === "number")) {
          res = v1.contents.addStrict(v2.contents);
        } else {
          throw Error("Types don't match for v1, v2");
        }
        break;

      case "BMinus":
        if (typeof v1.contents === "number" && typeof v2.contents === "number") {
          res = v1.contents - v2.contents;
        } else if (!(typeof v1.contents === "number") && !(typeof v2.contents === "number")) {
          res = v1.contents.subStrict(v2.contents);
        } else {
          console.error("v1, v2", v1, v2);
          throw Error("Types don't match for v1, v2");
        }
        break;

      case "Multiply":
        if (typeof v1.contents === "number" && typeof v2.contents === "number") {
          res = v1.contents * v2.contents;
        } else if (!(typeof v1.contents === "number") && !(typeof v2.contents === "number")) {
          res = v1.contents.mulStrict(v2.contents);
        } else {
          throw Error("Types don't match for v1, v2");
        }
        break;

      case "Divide":
        if (typeof v1.contents === "number" && typeof v2.contents === "number") {
          if (v2.contents === 0) throw new Error("divided by zero");
          res = v1.contents / v2.contents;
        } else if (!(typeof v1.contents === "number") && !(typeof v2.contents === "number")) {
          // two tensors
          res = v1.contents.divStrict(v2.contents);
        } else {
          throw Error("Types don't match for v1, v2");
        }
        break;

      case "Exp":
        if (typeof v1.contents === "number" && typeof v2.contents === "number") {
          if (v2.contents === 0) throw new Error("divided by zero");
          res = Math.pow(v1.contents, v2.contents);
        } else if (!(typeof v1.contents === "number") && !(typeof v2.contents === "number")) {
          // two tensors
          res = v1.contents.powStrict(v2.contents);
        } else {
          throw Error("Types don't match for v1, v2");
        }
        break;
    }

    returnType = "FloatV";
    return { tag: returnType, contents: res };
  }

  else if (v1.tag === "IntV" && v2.tag === "IntV") {
    returnType = "IntV";
  }
  else {
    throw new Error(`the types of two operands to ${op} must match`);
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
  arg: IFloatV<number | Tensor> | IIntV<number>
): Value<number | Tensor> => {

  if (arg.tag === "FloatV") {
    switch (op) {
      case "UPlus":
        throw new Error("unary plus is undefined");
      case "UMinus":
        if (typeof arg.contents === "number") {
          return { ...arg, contents: -arg.contents };
        } else {
          return { ...arg, contents: arg.contents.neg() }; // tensor
        }
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
): TagExpr<number> | IFGPI<number> => {
  let name, field, prop;
  switch (path.tag) {
    case "FieldPath":
      [name, field] = path.contents;
      // Type cast to field expression
      const fieldExpr = trans.trMap[name.contents][field];
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
      switch (gpi.tag) {
        case "FExpr":
          throw new Error("field path leads to an expression, not a GPI");
        case "FGPI":
          const [, propDict] = gpi.contents;
          return propDict[prop];
      }
  }
};

/**
 * Insert an expression into the translation an return a new one.
 * @param path path to a field or property
 * @param expr new expression
 * @param initTrans initial translation
 *
 * TODO: make sure this function is a deep enough copy of `initTrans`
 */
// TODO: Is it inefficient (space/time) to copy the whole translation every time an expression is inserted?
export const insertExpr = (
  path: Path,
  expr: TagExpr<number>,
  initTrans: Translation
): Translation => {
  const trans = { ...initTrans };
  let name, field, prop;
  switch (path.tag) {
    case "FieldPath":
      [name, field] = path.contents;
      // NOTE: this will overwrite existing expressions
      trans.trMap[name.contents][field] = { tag: "FExpr", contents: expr };
      return trans;
    case "PropertyPath":
      // TODO: why do I need to typecast this path? Maybe arrays are not checked properly in TS?
      [name, field, prop] = (path as IPropertyPath).contents;
      const gpi = trans.trMap[name.contents][field] as IFGPI<number>;
      const [, properties] = gpi.contents;
      properties[prop] = expr;
      return trans;
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
    varyingState: json.varyingState.map(differentiable),
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
    transr: state.translation,
    // NOTE: clean up all additional props and turn objects into lists
    shapesr: state.shapes
      .map(values)
      .map(([n, props]) => [n, pickBy(props, (p: any) => !p.omit)]),
  };
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
  varyingValues: number[] | Variable[]
) => {
  if (varyingValues.length !== varyingPaths.length) {
    console.log(varyingPaths, varyingValues);
    throw new Error("Different numbers of varying vars vs. paths");
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
