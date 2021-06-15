import {
  stateToDistanceGraph,
  createMatrix,
  assignNewColors,
  updateColors,
} from "./ColorProcesses";
import { Graph, RGB } from "./Color";
import { viridis_data } from "./ColorData";
import { State } from "types/state";

// build KNN graph
// use an adjacency list here?
const distGraphToKNNGraph = (distGraph: Graph, k: number): Graph => {
  if (k >= distGraph.length) {
    throw new Error("Warning: more neighbors requested than graph elems");
  }

  /*
  if (distGraph.length === 0){
    return []
  }
  */

  // for each node, store its k closest neighbors
  var ajlist = createMatrix(distGraph.length, k);
  //console.log("dist graph", distGraph)
  //console.log("ajlist", ajlist)
  //var ajlist : number[][] = [];
  if (typeof ajlist === "undefined") {
    throw new Error("ajlist is undefn");
  }

  for (var i = 0; i < distGraph.length; i++) {
    // get node,dist pairs

    var currRow = distGraph[i];

    var indexedRow = currRow.map((element, index) => {
      return [element, index];
    });
    //console.log("first pass")
    //console.log(indexedRow)

    // get all nodes except the current one
    indexedRow = indexedRow.filter((elemIndexPair) => {
      return i !== elemIndexPair[1];
    });
    //console.log("2nd pass")
    //console.log(indexedRow)

    indexedRow.sort((e1, e2): number => {
      return e1[0] - e2[0];
    });

    //console.log("third pass")
    //console.log(indexedRow)

    if (typeof indexedRow === "undefined") {
      throw new Error("ir undefn");
    }

    // need to update k if this is indexedRow :(
    // const maxNeighbors = Math.min(indexedRow.length, k);

    /*
    if (indexedRow.length < k){
      throw new Error("i'm sadddd")
    }
    */

    // take the first k
    for (var j = 0; j < k; j++) {
      if (typeof ajlist[i] === "undefined") {
        console.log(i, j, k);
        console.log(ajlist);
        throw new Error("kill me");
      }
      ajlist[i][j] = indexedRow[j][1]; // add the indexes in sorted order
    }
    /*
    for (var j=0; j<k; j++){
      newAjlistRow.push(indexedRow[j][1]);
    }

    ajlist.push(newAjlistRow);
    */
  }
  // console.log("ajlist")
  // console.log(ajlist)
  return ajlist; // jagged matrix? :(
};

const sampleUniformPalette = (
  numColorsRequested: number,
  palette = viridis_data
): RGB[] => {
  if (numColorsRequested === 0) return [];

  // this needs to be made more general (currently uniform sampling)
  // doesn't work if num colors requested > palette length
  const stepSize = Math.floor(palette.length / numColorsRequested);

  var rgbList: RGB[] = [];
  for (var i = 0; i < numColorsRequested; i++) {
    rgbList.push(palette[i * stepSize]);
  }
  return rgbList;
};

// assign colors using greedy method
const KNNGraphToColorList = (
  KNNGraph: Graph,
  k: number,
  palette = viridis_data
): RGB[] => {
  // find the minimum coloring???? given a graph???

  // for now just pick 3*k colors. its fine. its totally fine.
  const colorsToAssign = sampleUniformPalette(2 * k, palette);

  // greedy algo time...
  /*
  now do a greedy assignment of colors: 
  start at any node x, and assign a random color

. Then choose a random unassigned neighbor 
  and assign any color that is not equal 
  to the color at any of the assigned neighbors
  */

  // use a queue?

  // initializing
  var colorList: RGB[] = [];

  for (var i = 0; i < KNNGraph.length; i++) {
    var nodeToAssign = i; // queue // or .pop()
    var nbors = KNNGraph[nodeToAssign];

    var unavailableColorIndexes: number[] = [];
    for (var j = 0; j < nbors.length; j++) {
      var currNbor = nbors[j];
      if (currNbor < nodeToAssign) {
        // if it has been assigned
        const alreadyUsedColor = (elem: RGB) => {
          return elem === colorsToAssign[currNbor];
        };
        var alreadyUsedColorIndex = colorsToAssign.findIndex(alreadyUsedColor);
        if (alreadyUsedColorIndex === -1) {
          throw new Error(
            "Could not find index of color assigned in KNNGraphToColorList"
          );
        }
        unavailableColorIndexes.push(alreadyUsedColorIndex);
      }
    }

    var colorIndexes: number[] = [];
    for (var j = 0; j < colorsToAssign.length; j++) {
      if (!unavailableColorIndexes.includes(j)) {
        colorIndexes.push(j);
      }
    }
    if (colorIndexes.length === 0) {
      throw new Error("Not enough colors to assign");
    }
    const randomColorIndex = Math.floor(
      Math.random() * (colorIndexes.length - 1)
    );
    colorList.push(colorsToAssign[randomColorIndex]);
  }

  //console.log("colorList");
  //console.log(colorList)

  // test case
  if (
    colorList.length !==
    colorList.filter((elem) => {
      return elem !== [-1, -1, -1];
    }).length
  ) {
    throw new Error("bad");
  }

  return colorList;
};

export const updateColorsWithKNN = (state: State): State => {
  const distGraph = stateToDistanceGraph(state);
  if (distGraph.length <= 5) {
    var k = distGraph.length - 1;
  } else {
    var k = 5; // arbitrary k
  }

  if (k === 0) {
    // don't make a KNN graph.
    return updateColors(state);
  } else {
    const KNNGraph = distGraphToKNNGraph(distGraph, k);

    const colorList = KNNGraphToColorList(KNNGraph, k);
    console.log(colorList);

    const newState = assignNewColors(state, colorList);
    return newState;
  }
};
