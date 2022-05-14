import * as ad from "../types/ad";
import { Shape } from "../types/shape";
import { State } from "../types/state";
import { FieldExpr } from "../types/value";

// local typedefs for ease of typing expressions
type StringObjPair = [string, { [k: string]: FieldExpr<ad.Num> }];

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
  const objArr: [string, FieldExpr<ad.Num>][] = Object.entries(arr);
  const hasGPIAsTag = (object: [string, FieldExpr<ad.Num>]) => {
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
  const shapeNamesToInclude = arr.map((elem) => {
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
