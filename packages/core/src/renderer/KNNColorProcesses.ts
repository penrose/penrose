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
// assumes k > 0
const distGraphToKNNGraph = (distGraph: Graph, k: number): Graph => {
  if (k >= distGraph.length) {
    throw new Error("Warning: more neighbors requested than graph elems");
  }

  // for each node, store its k closest neighbors
  var ajlist = createMatrix(distGraph.length, k);

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

    // take the first k
    for (var j = 0; j < k; j++) {
      // include the 0 (?)
      if (typeof ajlist[i] === "undefined") {
        console.log(i, j, k);
        console.log(ajlist);
        throw new Error("kill me");
      }
      ajlist[i][j] = indexedRow[j][1]; // add the indexes in sorted order
    }
  }

  // this seems to be fine
  return ajlist; // jagged matrix? :(
};

const sampleUniformPalette = (
  numColorsRequested: number,
  palette = viridis_data
): RGB[] => {
  if (numColorsRequested === 0) return [];

  // doesn't work if num colors requested > palette length
  if (numColorsRequested > palette.length) {
    throw new Error("More colors requested than available in palette");
  }

  const stepSize = Math.floor(palette.length / numColorsRequested);

  var rgbList: RGB[] = [];

  for (var i = 0; i < numColorsRequested; i++) {
    rgbList.push(palette[i * stepSize]);
  }

  // the bug isn't here
  //console.log("rgb list");
  //console.log(rgbList)
  return rgbList;
};

// i believe the bug is here.
const KNNGraphToColorListAttempt2 = (
  KNNGraph: Graph,
  k: number,
  palette = viridis_data
) => {
  const numColorsRequested = 3 * k; // this is sufficient // 2k?
  const colorsToAssign = sampleUniformPalette(numColorsRequested, palette);

  var colorList: RGB[] = [];
  for (var i = 0; i < KNNGraph.length; i++) {
    colorList.push([-1, -1, -1]);
  }

  console.log("KNN");
  console.log(KNNGraph);

  console.log("k", k);

  console.log("colorsToAssign");
  console.log(colorsToAssign);

  console.log("initial colorList", colorList);

  // now assign the colors to the nodes of the graph in a greedy fashion.
  console.log("now entering the loop");
  for (var node = 0; node < KNNGraph.length; node++) {
    console.log("curr node", node);
    const nodeNbors = KNNGraph[node];
    console.log("nbors", nodeNbors);

    var colorsThatCannotBeUsed: RGB[] = [];
    // loop through the nbors, check if any have already been assigned a color
    for (
      var nbornodeindex = 0;
      nbornodeindex < nodeNbors.length;
      nbornodeindex++
    ) {
      const currNbor = nodeNbors[nbornodeindex];
      if (currNbor < node) {
        // then it has been assigned already (since we go in order)
        // get the color that it has been assigned
        const alreadyUsedColor = colorList[currNbor];
        colorsThatCannotBeUsed.push(alreadyUsedColor);
      }
    }
    console.log("used colors");
    console.log(colorsThatCannotBeUsed);

    // bug here. big bug ;-;
    const unavailableColorIndexes = colorsThatCannotBeUsed.map((elem) => {
      return colorsToAssign.findIndex((color) => {
        return color === elem;
      });
    });

    console.log("unavailable color indexes");
    console.log(unavailableColorIndexes);

    // now assign a color to nodeNbors (one that isn't a part of colorsThatCannotBeUsed)
    // to do this, we first create a list of indexes corresponding to the colors that
    // we CAN assign to the current node
    var availableColorIndexes: number[] = [];
    for (var i = 0; i < colorsToAssign.length; i++) {
      if (!unavailableColorIndexes.includes(i)) {
        availableColorIndexes.push(i);
      }
    }

    // now pick a random index from the available color indexes
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

// assign colors using greedy method
const KNNGraphToColorList = (
  KNNGraph: Graph,
  k: number,
  palette = viridis_data
): RGB[] => {
  // find the minimum coloring???? given a graph???
  const numColorsRequested = 3 * k; // this is sufficient // 2k?
  const colorsToAssign = sampleUniformPalette(numColorsRequested, palette);

  // imma run a quick test
  var test = [1];
  /* ok. in operator sucks
  if (!(1 in test)){
    throw new Error("bruh");
  }
  */
  if (!test.includes(1)) {
    // this is fine. in operator nO
    throw new Error("bruh");
  }

  // greedy algo time...
  /*
  now do a greedy assignment of colors: 
  start at any node x, and assign a random color

. Then choose a random unassigned neighbor 
  and assign any color that is not equal 
  to the color at any of the assigned neighbors
  */

  var colorList: RGB[] = [];
  for (var i = 0; i < KNNGraph.length; i++) {
    colorList.push([-1, -1, -1]);
  }
  // var assignedNodes : number[] = []

  for (var i = 0; i < KNNGraph.length; i++) {
    var nodeToAssign = i;
    var nbors = KNNGraph[nodeToAssign];

    var unavailableColorIndexes: number[] = [];
    for (var j = 0; j < nbors.length; j++) {
      var currNbor = nbors[j];

      // this part is fucked, most likely

      if (currNbor < i) {
        // if it has been assigned
        const alreadyUsedColor = (elem: RGB) => {
          // return elem === colorsToAssign[currNbor]; // bugged ...
          return colorList[currNbor] === elem;
        };
        // this is bugged...?
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
      if (!(j in unavailableColorIndexes)) {
        colorIndexes.push(j);
      }
    }
    if (colorIndexes.length === 0) {
      throw new Error("Not enough colors to assign");
    }

    const randomColorIndex = Math.floor(
      Math.random() * (colorIndexes.length - 1)
    );
    colorList[i] = colorsToAssign[colorIndexes[randomColorIndex]];
    // assignedNodes.push(nodeToAssign)
  }

  // test case
  if (
    colorList.length !==
    colorList.filter((elem) => {
      return elem[0] !== -1;
    }).length
  ) {
    throw new Error("bad");
  }

  console.log("KNNgraph");
  console.log(KNNGraph);
  console.log("colorList");
  console.log(colorList);
  return colorList;
};

export const updateColorsWithKNN = (state: State): State => {
  const distGraph = stateToDistanceGraph(state);
  // console.log("distgraph");
  // console.log(distGraph);
  if (distGraph.length <= 5) {
    var k = distGraph.length - 1;
  } else {
    var k = 5; // arbitrary k
  }
  if (k === 0) {
    // don't make a KNN graph, it causes problems
    return updateColors(state); //default to the method from ColorProcesses
  } else {
    // method
    const KNNGraph = distGraphToKNNGraph(distGraph, k);

    //const colorList = KNNGraphToColorList(KNNGraph, k);
    // console.log(colorList);

    const colorList = KNNGraphToColorListAttempt2(KNNGraph, k);

    //console.log("KNN");
    //console.log(KNNGraph);
    //console.log("colorlist");
    //console.log(colorList);

    const newState = assignNewColors(state, colorList);
    return newState;
  }
};

/*
@penrose/core:       [
@penrose/core:         0 [ 4, 3, 2, 1, 6 ],
@penrose/core:         1 [ 7, 5, 2, 3, 6 ],
@penrose/core:         2 [ 3, 4, 1, 7, 0 ],
@penrose/core:         3 [ 2, 4, 1, 7, 6 ],
@penrose/core:         4 [ 2, 3, 0, 1, 7 ],
@penrose/core:         5 [ 7, 6, 1, 3, 2 ],
@penrose/core:         6 [ 5, 7, 1, 3, 2 ],
@penrose/core:         7 [ 5, 1, 6, 3, 2 ]
@penrose/core:       ]

@penrose/core:       [
@penrose/core:         0 [ 0.20803, 0.718701, 0.472873 ],
@penrose/core:         1 [ 0.253935, 0.265254, 0.529983 ],
@penrose/core:         2 [ 0.120565, 0.596422, 0.543611 ],
@penrose/core:         3 [ 0.221989, 0.339161, 0.548752 ],
@penrose/core:         4 [ 0.221989, 0.339161, 0.548752 ],
@penrose/core:         5 [ 0.221989, 0.339161, 0.548752 ],
@penrose/core:         6 [ 0.134692, 0.658636, 0.517649 ],
@penrose/core:         7 [ 0.647257, 0.8584, 0.209861 ]
@penrose/core:       ]
*/

/*
@penrose/core:       [
@penrose/core:         0 [ 5, 6, 4, 1, 3 ],
@penrose/core:         1 [ 3, 2, 0, 5, 4 ],
@penrose/core:         2 [ 1, 3, 0, 5, 4 ],
@penrose/core:         3 [ 1, 2, 0, 5, 4 ],
@penrose/core:         4 [ 5, 6, 0, 1, 2 ],
@penrose/core:         5 [ 4, 6, 0, 1, 2 ],
@penrose/core:         6 [ 4, 5, 0, 1, 3 ]
@penrose/core:       ]

@penrose/core:       [
@penrose/core:         0 [ 0.477504, 0.821444, 0.318195 ],
@penrose/core:         1 [ 0.647257, 0.8584, 0.209861 ],
@penrose/core:         2 [ 0.139147, 0.533812, 0.555298 ],
@penrose/core:         3 [ 0.120565, 0.596422, 0.543611 ],
@penrose/core:         4 [ 0.20803, 0.718701, 0.472873 ],
@penrose/core:         5 [ 0.20803, 0.718701, 0.472873 ],
@penrose/core:         6 [ 0.20803, 0.718701, 0.472873 ]
@penrose/core:       ]


@penrose/core:       [
@penrose/core:         0 [ 1, 2, 5, 3, 4 ],
@penrose/core:         1 [ 0, 2, 5, 3, 4 ],
@penrose/core:         2 [ 1, 0, 5, 3, 4 ],
@penrose/core:         3 [ 4, 5, 0, 1, 2 ],
@penrose/core:         4 [ 3, 5, 0, 1, 2 ],
@penrose/core:         5 [ 3, 4, 0, 1, 2 ]
@penrose/core:       ]

@penrose/core:       [
@penrose/core:         0 [ 0.253935, 0.265254, 0.529983 ],
@penrose/core:         1 [ 0.277134, 0.185228, 0.489898 ],
@penrose/core:         2 [ 0.327796, 0.77398, 0.40664 ],
@penrose/core:         3 [ 0.647257, 0.8584, 0.209861 ],
@penrose/core:         4 [ 0.327796, 0.77398, 0.40664 ],
@penrose/core:         5 [ 0.20803, 0.718701, 0.472873 ]
@penrose/core:       ]


@penrose/core:       [ 0 [ 1, 2 ], 
                       1 [ 2, 0 ], 
                       2 [ 1, 0 ] ]

@penrose/core:       [
@penrose/core:         0 [ 0.196571, 0.711827, 0.479221 ],
@penrose/core:         1 [ 0.196571, 0.711827, 0.479221 ],
@penrose/core:         2 [ 0.267968, 0.223549, 0.512008 ]
@penrose/core:       ]

*/
