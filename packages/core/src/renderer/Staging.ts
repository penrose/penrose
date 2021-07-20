import { parseSubstance } from "../compiler/Substance";
import {
  ApplyPredicate,
  SubExpr,
  SubPredArg,
  SubProg,
  SubStmt,
} from "../types/substance";
import { IVarAD } from "../types/ad";
import { Shape } from "../types/shape";
import { State } from "../types/state";
import { FieldExpr } from "../types/value";

// local typedefs for ease of typing expressions
type StringObjPair = [string, { [k: string]: FieldExpr<IVarAD> }];

// Internal type for now...
export type Comic = { [k: string]: string[] };

export const testcomic: Comic = {
  "1": ["A", "B"],
  "2": ["A", "B", "IsSubset_B_A"],
};

export const getTrmapKeyComic = (json: Comic, subProg: string): Comic => {
  const obj: Comic = json; // JSON.parse(fs.readFileSync(json, "utf8").toString());

  const astOk = parseSubstance(subProg);
  if (!astOk.isOk()) {
    throw new Error("Substance program could not be parsed");
  }
  const subAst = astOk.value;

  // check obj that all obj values are legal
  const panelStmtList = Object.values(obj); // flatten one level

  // checks if all the substance stmts are valid
  // will throw an error on the first stmt that is not valid.
  const newPanelStmtList = panelStmtList.map((substStmtList) => {
    return getTrMapKeys(substStmtList, subAst).filter((name) => {
      return name !== "";
    });
  });

  // otherwise they are all valid, so translate them
  for (let i = 0; i < Object.keys(obj).length; i++) {
    const key = Object.keys(obj)[i];
    obj[key] = newPanelStmtList[i];
  }

  return obj;
};

const getTrMapKeys = (stmtList: string[], subAst: SubProg): string[] => {
  return stmtList.map((stmt) => getTrMapKey(stmt, subAst));
};

const getTrMapKey = (stmt: string, subAst: SubProg): string => {
  // turn string IsSubset ( B, A )
  // into the substance 'name'
  // first parse as a substance statement

  const stmtAstOk = parseSubstance(stmt);
  if (!stmtAstOk.isOk()) {
    throw new Error(`Statement ${stmt} could not be parsed.`);
  }

  // otherwise, it can be parsed
  const stmtAst = stmtAstOk.value;

  if (stmtAst.statements.length !== 1) {
    throw new Error(`Expected single substance construct from ${stmt}.`);
  }

  // get its name
  // console.log('name', stmt)
  const name = subStmtToInternalStr(stmtAst.statements[0]);

  // the list of possible names
  const subAstNames = subAst.statements.map(subStmtToInternalStr);

  if (!subAstNames.includes(name) && name !== "") {
    throw new Error(`Statement ${stmt} is not declared in substance.`);
  }

  return name;
};

const subStmtToInternalStr = (stmt: SubStmt): string => {
  switch (stmt.tag) {
    case "Decl":
      return stmt.name.value;
    case "ApplyPredicate":
      return getPredicateKeyName(stmt);
    default:
      return ""; // return an empty str, doesn't show up in trmap
  }
};

const getPredicateKeyName = (stmt: ApplyPredicate): string => {
  let name = stmt.name.value;
  for (let arg of stmt.args) {
    name = name.concat("_").concat(getSubPredArgKeyName(arg));
  }
  return name;
};

const getSubPredArgKeyName = (arg: SubPredArg): string => {
  if (arg.tag === "ApplyPredicate") return getPredicateKeyName(arg);
  return getSubExprKeyName(arg);
};

const getSubExprKeyName = (expr: SubExpr): string => {
  //let name;
  if (expr.tag === "Identifier") {
    return expr.value;
  } else if (expr.tag === "Deconstructor") {
    return `${expr.variable.value}_${expr.field.value}`;
  } else if (expr.tag === "StringLit") {
    return expr.contents;
  } else if (expr.tag === "Func" || "ApplyConstructor" || "ApplyFunction") {
    let str = expr.name.value;
    for (let arg of expr.args) {
      str = str.concat("_").concat(getSubExprKeyName(arg));
    }
    return str;
  } else throw new Error("unknown tag");
};

export const getComicPanelStates = (state: State, comic: Comic): State[] => {
  const panels = Object.values(comic);
  return panels.map((panel) => getStateForPanel(panel, state));
};

const getStateForPanel = (panel: string[], state: State): State => {
  const newShapeList = state.shapes.filter((shape) =>
    shapeIsPartOfPanel(shape, panel, state)
  );
  return {
    ...state,
    shapes: newShapeList,
  };
};

const shapeIsPartOfPanel = (
  shape: Shape,
  panel: string[],
  state: State
): boolean => {
  return panel.some((trMapName) =>
    shapeIsPartOfTrMapObj(trMapName, shape, state)
  );
};

const shapeIsPartOfTrMapObj = (
  trMapName: string,
  shape: Shape,
  state: State
): boolean => {
  if (!state.translation.trMap[trMapName]) {
    console.log(
      `WARNING: ${trMapName} is not a valid key. This object will not be rendered.`
    );
    return false;
  } // the requested name doesn't exist

  const trmapObj = state.translation.trMap[trMapName];

  const matchingKeyEntryGPIPairs = Object.entries(trmapObj).filter(
    ([fieldName, obj]) => {
      return (
        obj.tag === "FGPI" &&
        `${trMapName}.${fieldName}` === shape.properties.name.contents
      );
    }
  );

  if (matchingKeyEntryGPIPairs.length > 1) {
    // shouldn't happen (no duplicates)
    console.log(matchingKeyEntryGPIPairs);
    throw new Error("There are duplicates");
  }

  // either it matches or it doesn't
  return matchingKeyEntryGPIPairs.length === 1;
};

/**
 * Returns a list of states, one state per diagram in a series of staged diagrams
 */
export const getListOfStagedStates = (state: State): State[] => {
  // encodes the order in which the user defines GPIs in substance as obj properties
  let objArr = Object.entries(state.translation.trMap);
  objArr = objArr.filter(hasGPIProperties);

  // each element in listOfStagedObjArrs is a list of objs to draw
  // in a panel in the comic
  // ex. for a final diagram with objects A,B,C
  // listOfStagedObjArrs = [[A], [A,B], [A,B,C]]
  const listOfStagedObjArrs = objArr.map(tabulateObjArrs);

  const getStateFromObjArr = (arr: StringObjPair[]) => {
    return getStateFromObjArrAndLocalState(arr, state);
  };

  // map each object array to a state (modify the shapelist of the og state)
  const listOfStagedStates = listOfStagedObjArrs.map(getStateFromObjArr);

  return listOfStagedStates;
};

// determines if an object has any GPI tagged properties
const hasGPIProperties = (elem: StringObjPair) => {
  const arr = elem[1];
  const objArr: [string, FieldExpr<IVarAD>][] = Object.entries(arr);
  const hasGPIAsTag = (object: [string, FieldExpr<IVarAD>]) => {
    return object[1].tag === "FGPI";
  };
  return (
    objArr.filter((elem) => {
      return hasGPIAsTag(elem);
    }).length !== 0
  );
};

// used to make list of objects for each "comic panel"
// used as the fn for .map
const tabulateObjArrs = (
  objArr: StringObjPair,
  index: number,
  arr: StringObjPair[]
): StringObjPair[] => {
  return arr.slice(0, index + 1);
};

// returns a modified state RES, where only shapes
// that belong to objects in arr are kept in RES.shape (the state's shapelist)
const getStateFromObjArrAndLocalState = (
  arr: StringObjPair[],
  state: State
): State => {
  let shapeNamesToInclude = arr.map((elem) => {
    return elem[0];
  });

  const includeShape = (shape: Shape): boolean => {
    const shapePropPathName = shape.properties.name.contents as string;
    const dotIndex = shapePropPathName.indexOf(".");
    if (dotIndex === -1) {
      throw new Error("shape property doesn't have a .");
    }
    const shapeName = shapePropPathName.slice(0, dotIndex);
    return shapeNamesToInclude.includes(shapeName);
  };

  const newShapeList = state.shapes.filter(includeShape);

  // to be cleaner, i should technically update shapeOrdering as well
  // but not modifying that doesn't seem to affect the creation of the SVG

  return {
    ...state,
    shapes: newShapeList,
  };
};
