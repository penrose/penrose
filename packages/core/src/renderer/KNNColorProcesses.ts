import {
  stateToDistanceGraph,
  createMatrix,
  assignNewColors,
  updateColors,
} from "./ColorProcesses";
import { Graph, RGB } from "./Color";
import { viridis_data, random_palette } from "./ColorData";
import { State } from "types/state";

// Adjacency List data structure type
// given adj : Ajlist,
// adj[i] is a list of the nodes that have edges connected to node i
type Ajlist = number[][];

// build KNN graph (an adjacency list)
// from a graph of the distances between objects
// assumes k > 0 (k < 0 causes some problems)
export const distGraphToKNNGraph = (distGraph: Graph, k: number): Ajlist => {
  if (k >= distGraph.length) {
    throw new Error("Warning: more neighbors requested than graph elems");
  }

  // ajlist will store the k closest neighbors for each node, in sorted order
  // first initialize the matrix
  var ajlist: Ajlist = createMatrix(distGraph.length, k);

  // fill up the adjacency list
  for (var i = 0; i < distGraph.length; i++) {
    // list of all the neighbors of node i (includes itself)
    var currRow = distGraph[i];

    //  record the index of each neighbor
    //  along with its distance away from node i
    var indexedRow = currRow.map((element, index) => {
      return [element, index];
    });

    // remove node i from the list
    // (we don't include the node itself in its list of nbors)
    indexedRow = indexedRow.filter((elemIndexPair) => {
      return i !== elemIndexPair[1];
    });

    // sort the list based on the distance each node is away from node i
    indexedRow.sort((e1, e2): number => {
      return e1[0] - e2[0];
    });

    // put the indexes of the closest k neighbors in the ajlist
    for (var j = 0; j < k; j++) {
      ajlist[i][j] = indexedRow[j][1]; // add the indexes in sorted order
    }
  }

  return ajlist;
};

// returns a list of colors from the palette, sampled uniformly
const sampleUniformPalette = (
  numColorsRequested: number,
  palette: RGB[] = viridis_data
): RGB[] => {
  // handle 0 case, to prevent division by 0 later on
  if (numColorsRequested === 0) return [];

  // this fn doesn't work if num colors requested > palette length
  if (numColorsRequested > palette.length) {
    throw new Error("More colors requested than available in palette");
  }

  const stepSize = Math.floor(palette.length / numColorsRequested);

  var rgbList: RGB[] = [];

  for (var i = 0; i < numColorsRequested; i++) {
    rgbList.push(palette[i * stepSize]);
  }

  return rgbList;
};

// takes in a KNN graph (adjacency list)
// creates a colorList, i.e. a list that maps node --> its assigned color
export const KNNGraphToColorList = (
  KNNGraph: Ajlist,
  k: number,
  palette: RGB[] = viridis_data
): RGB[] => {
  // what's the minimum number of colors needed to color a graph, given k neighbors?
  const numColorsRequested = 2 * k; // this is sufficient?

  // list of colors that can be assigned to each node
  const colorsToAssign = sampleUniformPalette(numColorsRequested, palette);

  // initialize a colorlist
  var colorList: RGB[] = [];
  for (var i = 0; i < KNNGraph.length; i++) {
    colorList.push([-1, -1, -1]);
  }

  // now assign the colors to the nodes of the graph in a greedy fashion.
  for (var node = 0; node < KNNGraph.length; node++) {
    const nodeNbors = KNNGraph[node];

    var colorsThatCannotBeUsed: RGB[] = [];
    // loop through the nbors, check if any have already been assigned a color
    for (
      var nbornodeindex = 0;
      nbornodeindex < nodeNbors.length;
      nbornodeindex++
    ) {
      const currNbor = nodeNbors[nbornodeindex];
      if (currNbor < node) {
        // then it has been assigned a color already
        // (since we assign colors to the nodes in order)

        // get the color that it has been assigned
        const alreadyUsedColor = colorList[currNbor];
        colorsThatCannotBeUsed.push(alreadyUsedColor);
      }
    }

    // get a list of the indexes that map to the already used colors
    const unavailableColorIndexes = colorsThatCannotBeUsed.map((elem) => {
      return colorsToAssign.findIndex((color) => {
        return color === elem;
      });
    });

    // now assign a color to nodeNbors (one that isn't a part of colorsThatCannotBeUsed)
    // to do this, we first create a list of indexes corresponding to the colors that
    // we CAN assign to the current node
    var availableColorIndexes: number[] = [];
    for (var i = 0; i < colorsToAssign.length; i++) {
      if (!unavailableColorIndexes.includes(i)) {
        availableColorIndexes.push(i);
      }
    }

    // now pick a random index that maps to the a viable color
    const randomColorIndexIndex = Math.floor(
      Math.random() * (availableColorIndexes.length - 1)
    );
    const randomColorIndex = availableColorIndexes[randomColorIndexIndex];

    // get its color
    const randomColor = colorsToAssign[randomColorIndex];

    // set the random color
    colorList[node] = randomColor;
  }

  return colorList;
};

// returns a new state with updated colors (overwrites existing colors)
// doesn't take into consideration uninitialized vs. initialized paths
/**@deprecated */
export const updateColorsWithKNN = (state: State): State => {
  const distGraph = stateToDistanceGraph(state);

  if (distGraph.length <= 3) {
    var k = distGraph.length - 1;
  } else {
    var k = 3; // arbitrary k
  }
  if (k === 0) {
    // don't make a KNN graph, it causes problems when k===0
    return updateColors(state); //default to the method from ColorProcesses
  } else {
    // our main method
    const KNNGraph = distGraphToKNNGraph(distGraph, k);
    const colorList = KNNGraphToColorList(KNNGraph, k, random_palette());
    const newState = assignNewColors(state, colorList);
    return newState;
  }
};
