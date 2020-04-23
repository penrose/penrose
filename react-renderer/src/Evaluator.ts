import { zip } from "lodash";
import _ from "lodash";

////////////////////////////////////////////////////////////////////////////////
// Evaluator

// TODO: consider using `Dictionary` type so all runtime lookups are type-safe, like here https://codeburst.io/five-tips-i-wish-i-knew-when-i-started-with-typescript-c9e8609029db
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
};

export const evalShape = (
  e: IFGPI<number>,
  trans: Translation,
  varyingVars: [[Path, number]],
  shapes: Shape[]
): [Shape[], Translation] => {
  // TODO: finish
  return [[...shapes], trans];
};

// TODO: cache translation in intermediate steps
export const evalExprs = (
  es: Expr[],
  trans: Translation,
  varyingVars: VaryMap
): ArgVal<number>[] => es.map((e) => evalExpr(e, trans, varyingVars));

/**
 * Evaluate the input expression to a `Done` value.
 * TODO: return type; break cycles; optimize lookup
 * NOTE: This implementation needs the `Done` status of the values for optimizing evaluation and breaking cycles
 * TODO: maybe use a more OOP approach to encode current value and done status
 * TODO: deal with translation update
 * @param e the expression to be evaluated.
 * @param trans the `Translation` so far
 * @param varyingVars pairs of (path, value) for all optimized/"varying" values.
 */
export const evalExpr = (
  e: Expr,
  trans: Translation,
  varyingVars: VaryMap
): ArgVal<number> => {
  // TODO: all of below are TagExpr specific. Figure out where best to strip the tag away
  // If the expression is `Pending`, just return
  // if (e.tag === "Done") return e; // TODO: `done` is overloaded here, figure out exactly what it means for the current impl vs. scott's algorithm
  // if (e.tag === "Pending") return e;
  // Else pattern match on the type of expression and execute evaluation accordingly

  switch (e.tag) {
    case "IntLit":
      return { tag: "Val", contents: { tag: "IntV", contents: e.contents } };
    case "StringLit":
      return { tag: "Val", contents: { tag: "StrV", contents: e.contents } };
    case "BoolLit":
      return { tag: "Val", contents: { tag: "BoolV", contents: e.contents } };
    case "AFloat":
      if (e.contents.tag === "Vary") {
        throw new Error("encountered an unsubstituted varying value");
      } else {
        return {
          tag: "Val",
          contents: { tag: "FloatV", contents: e.contents.contents },
        };
      }
    case "UOp":
      const {
        contents: [uOp, expr],
      } = e as IUOp;
      // TODO: use the type system to narrow down Value to Float and Int?
      const arg = evalExpr(expr, trans, varyingVars).contents;
      return {
        tag: "Val",
        // HACK: coerce the type for now to let the compiler finish
        contents: evalUOp(uOp, arg as IFloatV<number> | IIntV<number>),
      };
    case "BinOp":
      const [binOp, e1, e2] = e.contents;
      const [val1, val2] = evalExprs([e1, e2], trans, varyingVars);
      return {
        tag: "Val",
        // HACK: coerce the type for now to let the compiler finish
        contents: evalBinOp(
          binOp,
          val1.contents as Value<number>,
          val2.contents as Value<number>
        ),
      };
    case "EPath":
      return resolvePath(e.contents, trans, varyingVars);
    case "CompApp":
      const [fnName, argExprs] = e.contents;
      // eval all args
      const args = evalExprs(argExprs, trans, varyingVars);
      const argValues = args.map((a) => argValue(a));
      // retrieve comp function from a global dict and call the function
      return compDict[fnName](...argValues);
    default:
      // TODO: error
      return { tag: "Val", contents: { tag: "FloatV", contents: NaN } };
  }
  // Simplest implementation: look up all necessary arguments recursively
};

/**
 * Given a path to a field or property, resolve to a fully evaluated GPI (where all props are evaluated) or an evaluated value.
 * TODO: lookup varying vars first?
 * TODO: cache done values somewhere, maybe by mutating the translation?
 * @param path path to a field (GPI or Expr) or a property (Expr only)
 * @param trans current computational graph
 * @param varyingVars list of varying variables and their values
 */
export const resolvePath = (
  path: Path,
  trans: Translation,
  varyingVars: VaryMap
): ArgVal<number> => {
  const gpiOrExpr = findExpr(trans, path);
  switch (gpiOrExpr.tag) {
    case "FGPI":
      const [type, props] = gpiOrExpr.contents;
      // TODO: cache results
      const evaledProps = _.mapValues(props, (p) =>
        p.tag === "OptEval"
          ? evalExpr(p.contents, trans, varyingVars)
          : p.contents
      );
      return { tag: "GPI", contents: [type, evaledProps] as GPI<number> };
    default:
      const expr: TagExpr<number> = gpiOrExpr;
      // TODO: cache results
      if (expr.tag === "OptEval") {
        return evalExpr(expr.contents, trans, varyingVars);
      } else return { tag: "Val", contents: expr.contents };
  }
};

// HACX: remove the type wrapper for the argument
const argValue = (e: ArgVal<number>) => {
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
  v1: Value<number>,
  v2: Value<number>
): Value<number> => {
  let returnType: "FloatV" | "IntV";
  // NOTE: need to explicitly check the types so the compiler will understand
  if (v1.tag === "FloatV" && v2.tag === "FloatV") returnType = "FloatV";
  else if (v1.tag === "IntV" && v2.tag === "IntV") returnType = "IntV";
  else throw new Error(`the types of two operands to ${op} must match`);

  switch (op) {
    case "BPlus":
      return { tag: returnType, contents: v1.contents + v2.contents };
    case "BMinus":
      return { tag: returnType, contents: v1.contents - v2.contents };
    case "Multiply":
      return { tag: returnType, contents: v1.contents * v2.contents };
    case "Divide":
      if (v2.contents === 0) throw new Error("divided by zero");
      const res = v1.contents / v2.contents;
      return {
        tag: returnType,
        contents: returnType === "IntV" ? Math.floor(res) : res,
      };
    case "Exp":
      return { tag: returnType, contents: Math.pow(v1.contents, v2.contents) };
  }
};

/**
 * Evaluate an unary operation such as + and -.
 * @param op an unary operater
 * @param arg the argument, must be float or int
 */
export const evalUOp = (
  op: UnaryOp,
  arg: IFloatV<number> | IIntV<number>
): Value<number> => {
  switch (op) {
    case "UPlus":
      throw new Error("unary plus is undefined");
    case "UMinus":
      return { ...arg, contents: -arg.contents };
  }
};

// TODO: return type
/**
 * Evaluate all shapes in the `State` by walking the `Translation` data structure, finding out all shape expressions (`FGPI` expressions computed by the Style compiler), and evaluating every property in each shape.
 * @param s the current state, which contains the `Translation` to be evaluated
 * NOTE: need to manage the random seed. In the backend we delibrately discard the new random seed within each of the opt session for consistent results.
 */
export const evalTranslation = (s: State) => {
  // Find out all the GPI expressions in the translation
  const shapeExprs = s.shapePaths.map((p: any) => findExpr(s.translation, p));

  // TODO: make sure the types are okay, i.e. all shape exprs are GPIs (via type assertion?)

  // Evaluate each of the shapes
  // const [
  //   shapes,
  //   trans,
  // ] = shapeExprs.reduce(([shapes, tr]: [Shape[], Translation], e: any) =>
  //   evalShape(e, s.translation, varyMap)
  // );

  // Update the state with the new list of shapes and translation
  // return { shapes: shapes, translation: trans, ...s};
};

/**
 * Finds an expression in a translation given a field or property path.
 * NOTE: assumes that the path points to an
 * @param trans - a translation from `State`
 * @param path - a path to an expression
 * @returns an expression
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
 * Gives types to a serialized `State`
 * @param json plain object encoding `State` of the diagram
 */
export const decodeState = (json: any): State => {
  // Find out the values of varying variables
  const state = {
    varyingPaths: json.varyingPaths as Path[],
    shapePaths: json.shapePaths,
    varyingValues: json.varyingState,
    // translation: decodeTranslation(json.transr),
    translation: json.transr,
    shapes: json.shapesr,
    varyingMap: zip(json.varyingPaths, json.varyingValues),
  };
  return state as State;
};

////////////////////////////////////////////////////////////////////////////////
// Types

type State = IState; // TODO
type Translation = ITrans<number>;
type VaryMap = [Path, number][];

interface IState {
  varyingPaths: Path[];
  shapePaths: Path[];
  varyingValues: number[];
  translation: Translation;
  shapes: Shape[];
  varyingMap: VaryMap;
}

// interface IGeneratedName { tag: "gen", contents: string }
// interface ISubstanceName { tag: "sub", contents: string }

// type Name = IGeneratedName | ISubstanceName
// type Translation = Record<Name, FieldDict>
// type FieldDict = Record<

/* Relevant Haskell types

data Translation a = Trans
  { trMap    :: M.Map Name (FieldDict a)
  , warnings :: [Warning]
  } deriving (Show, Eq, Typeable)

data State = State
  { shapesr            :: [Shape Double]
  , shapePaths         :: [Path]
  , shapeOrdering      :: [String]
  , shapeProperties    :: [(String, Field, Property)]
  , transr             :: Translation Double
  , varyingPaths       :: [Path]
  , uninitializedPaths :: [Path]
  , pendingPaths       :: [Path]
  , varyingState       :: [Double] -- Note: NOT polymorphic
  , paramsr            :: Params
  , objFns             :: [Fn]
  , constrFns          :: [Fn]
  , rng                :: StdGen
  , selectorMatches    :: [Int]
                    --  policyFn :: Policy,
  , policyParams       :: PolicyParams
  , oConfig            :: OptConfig
  }

*/
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
