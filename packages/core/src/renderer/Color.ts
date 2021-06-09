import { hsvToRGB } from "utils/Util";
import { viridis_data } from "./ColorData";

//
export type RGB = [number, number, number];
type HSV = [number, number, number];

const value2hsv = (value: number, hue?: number, sat?: number): HSV => {
  return [
    typeof hue === "undefined" ? 0 : hue,
    typeof sat === "undefined" ? 0 : sat,
    value,
  ];
};

const value2RGB = (value: number): RGB => {
  return hsvToRGB(value2hsv(value));
};

export const sampleUniformGrayscale = (
  numColors: number,
  start?: number,
  end?: number
): RGB[] => {
  // default for grayscale
  if (typeof start === "undefined") start = 0.1;
  if (typeof end === "undefined") end = 0.9;

  const stepSize = (end - start) / numColors;

  var valueArr: number[] = [];
  for (var i = 0; i < numColors; i++) {
    valueArr.push(start + i * stepSize);
  }

  const colorList = valueArr.map(value2RGB);

  return colorList;
};

// graph library

export const sampleUniformPalette = (
  numColorsRequested: number,
  palette?: RGB[]
): RGB[] => {
  if (numColorsRequested === 0) return [];

  // default palette
  if (typeof palette === "undefined") palette = viridis_data;

  // this needs to be made more general (currently uniform sampling)
  // doesn't work if num colors requested > palette length
  const stepSize = Math.floor(palette.length / numColorsRequested);

  var rgbList: RGB[] = [];
  for (var i = 0; i < numColorsRequested; i++) {
    rgbList.push(palette[i * stepSize]);
  }

  return rgbList;
};

interface Edge {
  start_node_index: number;
  end_node_index: number;
  weight: number;
  norm_weight?: number;
}

type Graph = number[][];

/*
res > 0 if e1 > e2
res === 0 if e1 === e2
res < 0 if e1 < e2
*/
const compareEdges = (e1: Edge, e2: Edge): number => {
  return e1.weight - e2.weight;
};

const is_square_matrix = (graph: Graph): boolean => {
  for (var i = 0; i < graph.length; i++) {
    if (graph[i].length !== graph.length) {
      return false;
    }
  }
  return true;
};

export const samplePalette = (object_graph: Graph, palette?: RGB[]): RGB[] => {
  if (!is_square_matrix(object_graph)) {
    throw new Error("Invalid graph matrix");
  }

  // pick a random point
  const numObjects = object_graph.length;
  const random_node = Math.floor(Math.random() * (numObjects - 1));

  var edges_from_random_node: Edge[] = [];
  for (var i = 0; i < numObjects; i++) {
    if (i != random_node) {
      edges_from_random_node.push({
        start_node_index: random_node,
        end_node_index: i,
        weight: object_graph[random_node][i],
      });
    }
  }

  // sort edges from matrix
  // need a generic sort?
  edges_from_random_node.sort(compareEdges);

  // map edge weights to [0, 1] [normalized]
  const max_edge_index = edges_from_random_node.length - 1;
  const max_edge_weight = edges_from_random_node[max_edge_index].weight;
  const min_edge_weight = edges_from_random_node[0].weight;

  const norm_magnitude = max_edge_weight - min_edge_weight;
  for (var i = 0; i < edges_from_random_node.length; i++) {
    edges_from_random_node[i].norm_weight =
      edges_from_random_node[i].weight / norm_magnitude;
  }

  // assign colors from palette

  // initializing
  var colorList: RGB[] = [];
  for (var i = 0; i < numObjects; i++) {
    colorList.push([-1, -1, -1]);
  }

  if (typeof palette === "undefined") palette = viridis_data;

  // array
  // index = node
  // entry = color assigned
  colorList[0] = palette[0];
  for (var i = 0; i < edges_from_random_node.length; i++) {
    var curr_norm_weight = edges_from_random_node[i].norm_weight;
    if (typeof curr_norm_weight === "undefined") {
      throw new Error("norm weight not defined");
    }
    var palette_index = Math.floor(curr_norm_weight * palette.length);

    var curr_node_index = edges_from_random_node[i].end_node_index;
    colorList[curr_node_index] = palette[palette_index];
  }

  return colorList;
};
