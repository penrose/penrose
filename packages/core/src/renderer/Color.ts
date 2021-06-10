import { viridis_data } from "./ColorData";

export type RGB = [number, number, number];

/**
 * @type Adjacency matrix storing edge weights between nodes
 */
export type Graph = number[][];

/**
 * Stores information about edges
 * @property start_node_index : first node connected by the edge
 * @property end_node_index : second node connected by the edge
 * @property weight : edge weight
 * @property norm_weight? : normalized edge weight, to be initialized using info about the entire graph
 */
export interface Edge {
  start_node_index: number;
  end_node_index: number;

  weight: number;
  norm_weight?: number;
}

/**
 * Comparison fn, used for sorting a list of Edges
 * @returns res > 0 if e1 > e2; res === 0 if e1 === e2; res < 0 if e1 < e2
 */
export const compareEdges = (e1: Edge, e2: Edge): number => {
  return e1.weight - e2.weight;
};

/**
 * Checks that # of rows === # of cols within a graph (list of num lists)
 */
export const is_square_matrix = (graph: Graph): boolean => {
  for (var i = 0; i < graph.length; i++) {
    if (graph[i].length !== graph.length) {
      return false;
    }
  }
  return true;
};

/**
 * Checks that an edge exists between every pair of nodes & no self loops exist
 */
export const is_complete_graph = (graph: Graph): boolean => {
  if (!is_square_matrix(graph)) return false;
  for (var i = 0; i < graph.length; i++) {
    for (var j = 0; j < graph.length; j++) {
      // no self loops
      if (i == j && graph[i][j] !== 0) return false;
      // an edge exists between distinct nodes
      // else if (graph[i][j] === 0) return false;
      // edge weights are symmetric
      else if (i != j && graph[i][j] !== graph[j][i]) return false;
    }
  }
  return true;
}; // maybe I can add memoization to this?

/**
 * Main sampling function: assigns colors based on random node
 * @param object_graph a complete graph (nxn adjacency matrix for n objects)
 * @param palette optional param, a list of colors in the RGB space
 * @returns a list of colors, where the index === object node number
 */ export const samplePalette = (
  object_graph: Graph,
  node_to_sort_around?: number,
  palette = viridis_data
): RGB[] => {
  if (!is_complete_graph(object_graph)) {
    throw Error("Invalid graph input");
  }
  const numberOfNodes = object_graph.length;

  if (
    typeof node_to_sort_around !== "undefined" &&
    node_to_sort_around >= numberOfNodes
  ) {
    throw Error("Invalid node to sort around");
  }

  // handle 0 node case
  if (numberOfNodes === 0) return [];
  // handle 1 node case
  else if (numberOfNodes === 1) {
    // pick a random color
    const random_index = Math.floor(Math.random() * (palette.length - 1));
    return [palette[random_index]];
  }

  // pick a random node to sort edge weights using
  const random_node = Math.floor(Math.random() * (numberOfNodes - 1));
  if (typeof node_to_sort_around === "undefined")
    node_to_sort_around = random_node;

  // gather up the edge weights of the edges attached to the node
  var edges_from_selected_node: Edge[] = [];
  for (var i = 0; i < numberOfNodes; i++) {
    if (i != node_to_sort_around) {
      // no self edges
      edges_from_selected_node.push(
        // this is an Edge type object
        {
          start_node_index: node_to_sort_around,
          end_node_index: i,
          weight: object_graph[node_to_sort_around][i],
        }
      );
    }
  }

  // sort the edges
  edges_from_selected_node.sort(compareEdges);

  const max_edge_index = edges_from_selected_node.length - 1;
  const max_edge_weight = edges_from_selected_node[max_edge_index].weight;
  const min_edge_weight = edges_from_selected_node[0].weight;

  // norm weight is the difference between the max and min edge weights
  const norm_magnitude = max_edge_weight - min_edge_weight;

  // give all Edge objects a norm weight property in the range [0, 1]
  for (var i = 0; i < edges_from_selected_node.length; i++) {
    edges_from_selected_node[i].norm_weight =
      edges_from_selected_node[i].weight / norm_magnitude;
  }

  // assign colors from palette

  // initializing a RGB array of the appropriate size (length = n)
  var colorList: RGB[] = [];
  for (var i = 0; i < numberOfNodes; i++) {
    colorList.push([-1, -1, -1]);
  }

  // use a default palette if no palette was passed in
  // if (typeof palette === "undefined") palette = viridis_data;

  // assign node 0 to the first color in the palette
  colorList[0] = palette[0];

  // assign colors to the nodes connected to node 0
  for (var i = 0; i < edges_from_selected_node.length; i++) {
    var curr_norm_weight = edges_from_selected_node[i].norm_weight;

    // a check to make typescript happy (technically shouldn't happen)
    if (typeof curr_norm_weight === "undefined") {
      throw Error("Norm weight not defined in Edge");
    }

    // get the index for extracting the color from the palette
    var palette_index = Math.floor(curr_norm_weight * palette.length);

    // get the index of the node connected to node 0 at the current edge
    var curr_node_index = edges_from_selected_node[i].end_node_index;

    // set the color
    colorList[curr_node_index] = palette[palette_index];
  }

  return colorList;
};
