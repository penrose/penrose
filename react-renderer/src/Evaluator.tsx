import { zip } from "lodash";

////////////////////////////////////////////////////////////////////////////////
// Evaluator

// TODO: return type; break cycles
const evalExpr = (
  e: Expr,
  varyingVars: [[Path, number]],
  trans: Translation
) => 
{

};

// TODO: return type
const evalTranslation = (s: State) => {
  // Find out the values of varying variables
  const varyMap= zip(s.varyingPaths, s.varyingValues) as [[Path, number]];
  // Find out all the GPI expressions in the translation
  const shapeExprs = s.shapePaths.map((p: any) => findExpr(s.translation, p));

  // TODO: make sure the types are okay, i.e. all shape exprs are GPIs (via type assertion?)

  // Evaluate each of the shapes
  const shapes = shapeExprs.reduce((shapes: Shape[], e: any) => evalExpr(e, s.translation, varyMap)); 

  // Update the state with the new list of shapes and translation
  return { shapes: shapes, translation: trans, ...s};
};

// TODO: complete this
const findExpr = (trans : Translation, path : Path) => {
  return;
}

export const decodeState = (json : any) : State => {
  return {
    varyingPaths: json.varyingPaths,
    shapePaths: json.shapePath,
    varyingValues: json.varyingState,
    translation: json.transr
  };
}

////////////////////////////////////////////////////////////////////////////////
// Types

type State = IState; // TODO
type Shape = any; // TODO
type Expr = any; // TODO
type Translation = any; // TODO
type Path = any; // TODO

interface IState {
  varyingPaths: Path[],
  shapePaths: Path[],
  varyingValues: number[],
  translation: Translation
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
  , shapePaths         :: [(String, Field)] -- TODO Sub name type
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
  , autostep           :: Bool -- TODO: deprecate this
  , selectorMatches    :: [Int]
                    --  policyFn :: Policy,
  , policyParams       :: PolicyParams
  , oConfig            :: OptConfig
  }

*/
////////////////////////////////////////////////////////////////////////////////
