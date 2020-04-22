import { zip } from "lodash";

////////////////////////////////////////////////////////////////////////////////
// Evaluator

export const evalShape = (
  e: IFGPI<number>,
  trans: Translation,
  varyingVars: [[Path, number]],
  shapes: Shape[]
): [Shape[], Translation] => {
  // TODO: finish
  return [[...shapes], trans];
};

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
  // If the expression is `Pending`, just return
  // if (e.tag === "Done") return e; // TODO: `done` is overloaded here, figure out exactly what it means for the current impl vs. scott's algorithm
  // if (e.tag === "Pending") return e;

  // Else pattern match on the type of expression and execute evaluation accordingly

  switch (e.tag) {
    case "IntLit":
      return { tag: "Val", contents: { tag: "IntV", contents: e.contents } };
    case "UOp":
      const {
        contents: [op, expr],
      } = e as IUOp;
      const { contents: arg } = evalExpr(expr, trans, varyingVars);

      return { tag: "Val", contents: evalUOp(op, arg) };
    case "BinOp":
      // TODO:
      return { tag: "Val", contents: { tag: "FloatV", contents: NaN } };
    default:
      // TODO: error
      return { tag: "Val", contents: { tag: "FloatV", contents: NaN } };
  }
  // Simplest implementation: look up all necessary arguments recursively
};

/**
 * Evaluate an unary operation such as + and -.
 * @param op Unary operater
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
 * Finds an expression in a translation given a field or property path
 * @param trans - a translation from `State`
 * @param path - a path to an expression
 * @returns an expression
 */
export const findExpr = (trans: Translation, path: Path): TagExpr<number> => {
  let name, field, prop;
  switch (path.tag) {
    case "FieldPath":
      [name, field] = path.contents;
      // Type cast to field expression
      const fieldExpr = trans.trMap[name.contents][field] as IFExpr<number>;
      return fieldExpr.contents;
    case "PropertyPath":
      [name, field, prop] = path.contents;
      // Type cast to FGPI and get the properties
      const gpi = trans.trMap[name.contents][field] as IFGPI<number>;
      const [_, propDict] = gpi.contents;

      return propDict[prop];
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
type VaryMap = [[Path, number]];

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
//           contents: [
//             shapeType,
//             new Map(Object.entries(properties)) as PropertyDict<number>,
//           ],
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
//     trMap: trans,
//   };
// };
