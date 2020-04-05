import { zip } from "lodash";

////////////////////////////////////////////////////////////////////////////////
// Evaluator

// TODO: return type; break cycles
// const evalExpr = (
//   e: Expr,
//   trans: Translation,
//   varyingVars: [[Path, number]]
// ) : Shape =>
// {

// };

// TODO: return type
const evalTranslation = (s: State) => {
  // Find out the values of varying variables
  const varyMap = zip(s.varyingPaths, s.varyingValues) as [[Path, number]];

  // Find out all the GPI expressions in the translation
  const shapeExprs = s.shapePaths.map((p: any) => findExpr(s.translation, p));

  // TODO: make sure the types are okay, i.e. all shape exprs are GPIs (via type assertion?)

  // Evaluate each of the shapes
  // const [shapes, trans] = shapeExprs.reduce(([shapes, tr]: [Shape[], Translation], e: any) => evalExpr(e, s.translation, varyMap));

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
      const fieldExpr = trans.trMap.get(name.contents)?.get(field) as IFExpr<
        number
      >;
      return fieldExpr.contents;
    case "PropertyPath":
      [name, field, prop] = path.contents;
      // Type cast to FGPI and get the properties
      const gpi = trans.trMap.get(name.contents)?.get(field) as IFGPI<number>;

      return gpi.contents[1].get(prop)!;
  }
};

/**
 * Gives types to a serialized `State`
 * @param json plain object encoding `State` of the diagram
 */
export const decodeState = (json: any): State => {
  const state = {
    varyingPaths: json.varyingPaths as Path[],
    shapePaths: json.shapePaths,
    varyingValues: json.varyingState,
    translation: decodeTranslation(json.transr),
    shapes: json.shapesr,
  };
  return state as State;
};

/**
 * Gives types to a serialized `Translation`
 * @param json plain object encoding `Translation`
 */
const decodeTranslation = (json: any): Translation => {
  const decodeFieldExpr = (expr: any): FieldExpr<number> => {
    switch (expr.tag) {
      case "FGPI":
        const [shapeType, properties] = expr.contents;
        return {
          tag: "FGPI",
          contents: [
            shapeType,
            new Map(Object.entries(properties)) as PropertyDict<number>,
          ],
        };
      case "FExpr":
        return expr;
      default:
        throw new Error(`error decoding field expression ${expr}`);
    }
  };

  const trans = new Map(
    Object.entries(json.trMap).map(([subName, es]: any) => [
      subName,
      new Map(
        Object.entries(es).map(([fieldName, e]: any) => [
          fieldName,
          decodeFieldExpr(e),
        ])
      ) as FieldDict<number>,
    ])
  ) as TransDict<number>;

  return {
    warnings: json.warnings,
    trMap: trans,
  };
};

////////////////////////////////////////////////////////////////////////////////
// Types

type State = IState; // TODO
type Translation = ITrans<number>;

interface IState {
  varyingPaths: Path[];
  shapePaths: Path[];
  varyingValues: number[];
  translation: Translation;
  shapes: Shape[];
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
