import { IVarAD } from "../types/ad";
import { Shape } from "../types/shape";
import { State } from "../types/state";
import { FieldExpr } from "../types/value";

// filter out trmap

export const getListOfStagedStates = (state: State): State[] => {
  const trMap = state.translation.trMap;
  // console.log(trMap);
  var objArr = Object.entries(trMap);

  // filter out non-shapes (non - FGPI things)

  // objArr = objArr.filter(areLocallyDefined);

  objArr = objArr.filter(hasGPIProperties);
  // console.log(objArr)

  // each entry in test2 is a list of objs to "include" in a panel in the comic

  // now just run whatever the fuck is happening with testArr down there on each of them
  // and return a state
  // ez. map over the arr. ez.
  var listOfStagedObjArrs = objArr.map(tabulateObjArrs);

  // console.log(test2.length, objArr.length);
  // console.log(test2)

  const newState = state;

  const getStateFromObjArr = (arr: StringObjPair[]) => {
    return getStateFromObjArrAndLocalState(arr, newState);
  };

  var listOfStagedStates = listOfStagedObjArrs.map(getStateFromObjArr);

  return listOfStagedStates;

  // test2.map (what is happening down there)

  // make a list of objArr.length states (one for each state)

  // now create a list of 7 "states?"
  // pass objArr into another fxn

  // excludes the last elem
  /*
  var testArr = objArr.slice(0, objArr.length-4)
  var testArrNames = testArr.map((elem) => {return elem[0]})
  // make a newState (replace list of shapes) 
  var newShapeList : Shape[] = [];
  for (var i=0; i<state.shapes.length; i++){
    var shap = state.shapes[i];
    var shapPropPathName = shap.properties.name.contents as string;
    const dotIndex = shapPropPathName.indexOf('.');
    if (dotIndex === -1){
      throw new Error("shape property doesn't have a .");
    }
    var shapName = shapPropPathName.slice(0, dotIndex);
    // console.log(shapName);
    if (testArrNames.includes(shapName)){
      newShapeList.push(shap);
    }
  }

  // console.log(newShapeList);

  var newState = state
  newState.shapes = newShapeList;

  return newState;
 
  // console.log(testArr)
  */
};

// helpers
const areLocallyDefined = (elem: any) => {
  return !elem[0].includes("$LOCAL");
};

const hasGPIProperties = (elem: any) => {
  const arr = elem[1];
  //console.log(arr)
  const objArr: [string, any][] = Object.entries(arr);
  //console.log("here")
  //console.log(objArr)
  //console.log("sad")

  // filter over this array
  const hasGPIAsTag = (object: any) => {
    return object[1].tag === "FGPI";
  };

  return (
    objArr.filter((elem) => {
      return hasGPIAsTag(elem);
    }).length !== 0
  );
};

type StringObjPair = [string, { [k: string]: FieldExpr<IVarAD> }];
const tabulateObjArrs = (
  objArr: StringObjPair,
  index: number,
  arr: StringObjPair[]
): StringObjPair[] => {
  return arr.slice(0, index + 1);
};

// this should be nested within the same function, for state to be in scope.

// after you declare it, just call test2.map(getStateFromObjArr)
// to get a list of states
// then just render each state, ez money
const getStateFromObjArrAndLocalState = (
  arr: StringObjPair[],
  state: State
): State => {
  var testArrNames = arr.map((elem) => {
    return elem[0];
  });
  console.log("obj arr");
  console.log(arr);
  console.log("obj names");
  console.log(testArrNames);
  // make a newState (replace list of shapes)
  var newShapeList: Shape[] = [];
  console.log(state.shapes.length);
  for (var i = 0; i < state.shapes.length; i++) {
    var shap = state.shapes[i];
    var shapPropPathName = shap.properties.name.contents as string;
    const dotIndex = shapPropPathName.indexOf(".");
    if (dotIndex === -1) {
      throw new Error("shape property doesn't have a .");
    }
    var shapName = shapPropPathName.slice(0, dotIndex);
    console.log("shapeName", shapName);

    if (testArrNames.includes(shapName)) {
      newShapeList.push(shap);
    }
  }

  /*
	var newShapeOrdering : string[] = []
	for (var i=0; i<state.shapeOrdering.length; i++){
		var shapeName = state.shapeOrdering[i];
	}
	*/

  // console.log(newShapeList);

  // shallow copy, otherwise weird aliasing issues
  var newState = { ...state };

  newState.shapes = newShapeList;

  return newState;
};
