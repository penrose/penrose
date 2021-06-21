import { State } from "types/state";
import { Shape } from "types/shape";

import { Graph, samplePalette, is_complete_graph, RGB } from "./Color";

// initializes a matrix of all 0.s
export const createMatrix = (rows: number, cols: number): Graph => {
  var matrix: number[][] = [];
  for (var i = 0; i < rows; i++) {
    var row: number[] = [];
    for (var j = 0; j < cols; j++) {
      row.push(0);
    }
    matrix.push(row);
  }
  return matrix;
};

// euclidean distance between two vectors
const dist = (v1: number[], v2: number[]): number => {
  if (v1.length != v2.length) {
    throw new Error("Vector inputs are not of the same dimension");
  }
  var squaredSum = 0;
  for (var i = 0; i < v1.length; i++) {
    squaredSum += (v1[i] - v2[i]) * (v1[i] - v2[i]);
  }
  return Math.sqrt(squaredSum);
};

// excluding shapes that don't have a center or color attribute, or
// shapes that alredy have appropriate default colors, like text (black)
// or this includes shapes that i haven't gotten too yet
export const includeShapesOnly = (shape: Shape): boolean => {
  return !(
    shape.shapeType === "FreeformPolygon" ||
    shape.shapeType === "Polygon" ||
    shape.shapeType === "Line" ||
    shape.shapeType === "Arrow" ||
    shape.shapeType === "Path" ||
    shape.shapeType === "Text" ||
    shape.shapeType === "Image" ||
    shape.shapeType === "PathString" ||
    shape.shapeType === "Polyline"
  );
};

// given a state, generates a matrix that records the
// distance between objects.

// given a list of shapes, create a matrix that records distance between objects
// ex. graph[i][j] === distance between shape i and shape j
export const shapeListToDistanceGraph = (shapeList: Shape[]): Graph => {
  // initializing a matrix of 0.'s
  var object_graph = createMatrix(shapeList.length, shapeList.length);

  // filling in the matrix
  for (var i = 0; i < shapeList.length; i++) {
    for (var j = i + 1; j < shapeList.length; j++) {
      const shape1 = shapeList[i];
      const shape2 = shapeList[j];

      // for now, the only thing used to determine distance will be the centers
      // of each shape
      var v1 = shape1.properties.center;
      var v2 = shape2.properties.center;

      // some legality checks
      if (
        !Array.isArray(v1.contents) ||
        !Array.isArray(v2.contents) ||
        v1.contents.length !== v2.contents.length ||
        typeof v1.contents[0] !== "number" ||
        typeof v2.contents[0] !== "number"
      ) {
        throw new Error("bad center prop input: not number[]");
      }

      // calculate & set the distance
      const centerDist = dist(v1.contents as number[], v2.contents as number[]);
      object_graph[i][j] = centerDist;

      // symmetric matrix (we are creating an undirected graph)
      object_graph[j][i] = object_graph[i][j];
    }
  }

  return object_graph;
};

// generates a distance graph from a state (uses all viable shapes:
// does not distinguish between whether they have
// an initialized or uninitialized color path
/**@deprecated */
export const stateToDistanceGraph = (state: State): Graph => {
  const shapeList = state.shapes.filter(includeShapesOnly);
  return shapeListToDistanceGraph(shapeList);
};

// converts a matrix that stores distance between objects
// to one that stores "repellant energy" between objects
// ex. graph[i][j] === *how much* objs i and j should repel each other
// (repel each other in terms of color)
export const distanceGraphToEnergyGraph = (graph: Graph): Graph => {
  var newGraph = graph;
  for (var i = 0; i < graph.length; i++) {
    for (var j = i + 1; j < graph.length; j++) {
      // super hacky, uses some guidelines fron Constraints.ts (repel fxns)

      const epsilon = 20; // prevent division by 0
      const weight = 10e4; // scaling factor, since values are typically small

      // invert the distance to get a "repellant energy" value
      newGraph[i][j] = (1 / (graph[i][j] + epsilon)) * weight;

      // symmetric graph
      newGraph[j][i] = newGraph[i][j];
    }
  }
  return newGraph;
};

// create a new state with newly assigned colors to some shapes
// only shapes that satisfy includeInColorAdjustmentFn have their colors revised
// the colors are selected from colorList, in the order they appear

// ex. the first shape s for which includeInColorAdjustmentFn(s) === true
// will be assigned colorList[0] as its color

// the last shape sLast for which includeInColorAdjustmentFn(sLast) === true
// will be assigned colorList[colorList.length - 1] as its color
export const assignNewColors = (
  state: State,
  colorList: RGB[],
  includeInColorAdjustmentFn: (s: Shape) => boolean = includeShapesOnly,
  alpha: number = 0.8
): State => {
  // assumes all colors map to the order of appropriate objects in state
  var newState = state;
  var j = 0;
  for (var i = 0; i < newState.shapes.length; i++) {
    if (includeInColorAdjustmentFn(newState.shapes[i])) {
      newState.shapes[i].properties.color = {
        tag: "ColorV",
        contents: {
          tag: "RGBA",
          contents: [colorList[j][0], colorList[j][1], colorList[j][2], alpha],
        },
      };
      j += 1;
    }
  }
  return newState;
};

// find the first pair 0 dist apart, or the closest one
// this assumes that the graph matrix records *repellant energy*, so a
// greater matrix entry indicates that the nodes are closer together
// (and should repel each other more for color)
/**@deprecated */
const findTwoClosestNodes = (graph: Graph): [number, number] => {
  if (!is_complete_graph(graph) || graph.length == 0) {
    throw new Error("Invalid graph input in findTwoClosestNodes");
  } else if (graph.length == 1) {
    // hack: only the first num in the tuple is used, so second num is arbitrary
    return [0, 0];
  }

  // initialize the variable for the pair of closest nodes
  var closestNodes;
  for (var i = 0; i < graph.length; i++) {
    for (var j = i + 1; j < graph.length; j++) {
      if (typeof closestNodes === "undefined") {
        closestNodes = [i, j];
      } else if (graph[i][j] > graph[closestNodes[0]][closestNodes[1]]) {
        closestNodes = [i, j];
      }
    }
  }
  if (typeof closestNodes === "undefined") {
    throw new Error("No pair of closest nodes found in findTwoClosestNodes");
  }
  return closestNodes as [number, number];
};

// find the minimum weighted edge between two nodes
// and use one of those nodes as the "pivot" nodes to select colors from (according to edges)
// do not use: color assignment is not as effective as updateColorsWithKNN
/**@deprecated */
export const updateColors = (state: State): State => {
  const distGraph = stateToDistanceGraph(state);
  const energyGraph = distanceGraphToEnergyGraph(distGraph);
  if (energyGraph.length <= 1) {
    var colorList = samplePalette(energyGraph);
  } else {
    const [node1, node2] = findTwoClosestNodes(energyGraph);
    var colorList = samplePalette(energyGraph, node1);
  }
  const newState = assignNewColors(state, colorList);
  return newState;
};
