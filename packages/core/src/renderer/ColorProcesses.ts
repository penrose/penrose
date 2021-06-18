import { State } from "types/state";
import { Shape } from "types/shape";

import { Graph, samplePalette, is_complete_graph } from "./Color";

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

export const shapeListToDistanceGraph = (shapeList: Shape[]): Graph => {
  // initializing a matrix of 0.'s
  var object_graph = createMatrix(shapeList.length, shapeList.length);

  // filling in the matrix
  for (var i = 0; i < shapeList.length; i++) {
    for (var j = i + 1; j < shapeList.length; j++) {
      const shape1 = shapeList[i];
      const shape2 = shapeList[j];

      //for now, the only thing used will be the centers
      var v1 = shape1.properties.center;
      var v2 = shape2.properties.center;

      if (
        !Array.isArray(v1.contents) ||
        !Array.isArray(v2.contents) ||
        v1.contents.length !== v2.contents.length ||
        typeof v1.contents[0] !== "number" ||
        typeof v2.contents[0] !== "number"
      ) {
        throw new Error("bad center prop input: not number[]");
      }

      const centerDist = dist(v1.contents as number[], v2.contents as number[]);

      object_graph[i][j] = centerDist;

      // symmetric graph
      object_graph[j][i] = object_graph[i][j];
    }
  }

  return object_graph;
};

export const stateToDistanceGraph = (state: State): Graph => {
  const shapeList = state.shapes.filter(includeShapesOnly);
  return shapeListToDistanceGraph(shapeList);
};

export const distanceGraphToEnergyGraph = (graph: Graph): Graph => {
  var newGraph = graph;
  for (var i = 0; i < graph.length; i++) {
    for (var j = i + 1; j < graph.length; j++) {
      // super hacky, uses some guidelines fron Constraints.ts (repel fxns)

      const epsilon = 20;
      const weight = 10e4; // scaling factor, since values are typically small

      // invert the distance to get a "repellant energy" value
      newGraph[i][j] = (1 / (graph[i][j] + epsilon)) * weight;

      // symmetric graph
      newGraph[j][i] = newGraph[i][j];
    }
  }
  return newGraph;
};

// create a new state with newly assigned colors
// assigns the alpha of colors to be 0.5 arbitrarily
export const assignNewColors = (
  state: State,
  colorList: [number, number, number][],
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
// this assumes that the graph matrix records * repellant energy *, so a
// greater matrix entry indicates that the nodes are closer together
// (and should repel each other more for color)
const findTwoClosestNodes = (graph: Graph): [number, number] => {
  if (!is_complete_graph(graph) || graph.length == 0) {
    throw new Error("Invalid graph input in findTwoClosestNodes");
  } else if (graph.length == 1) {
    return [0, 0]; // only the first node is used, so second num is arbitrary
  }

  // init
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

// find the min distance, and use that as the pivot node.
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
