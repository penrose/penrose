import { Shape, ShapeAD } from "../types/shape";
import Graph from "./Graph";

export type GroupGraph = Graph<
  string, // shape name
  number // shape layer
>;

export const makeGroupGraph = <T extends Shape | ShapeAD>(
  shapes: T[]
): GroupGraph => {
  const graph: GroupGraph = new Graph();
  const nameShapeMap: { [k: string]: T } = {};
  // populate the nodes
  for (let i = 0; i < shapes.length; i++) {
    const shape = shapes[i];
    const layer = i;
    const shapeNameVal = shape.properties["name"];
    if (shapeNameVal.tag !== "StrV") {
      throw Error("Shape name is not a string");
    }
    const shapeName = shapeNameVal.contents;

    graph.setNode(shapeName, layer);
    nameShapeMap[shapeName] = shape;
  }
  // then populate the edges
  for (const [name, shape] of Object.entries(nameShapeMap)) {
    if (shape.shapeType === "Group") {
      const subGPIsVal = shape.properties["shapes"];
      if (subGPIsVal.tag !== "ShapeListV") {
        throw Error("Group content not a list of shapes");
      }
      const subGPIs = subGPIsVal.contents;
      const subNames = subGPIs.map((gpi) => {
        const subShapeNameVal = gpi.contents[1]["name"];
        if (subShapeNameVal.tag !== "StrV") {
          throw Error("Shape name is not a string");
        }
        return subShapeNameVal.contents;
      });

      for (const subName of subNames) {
        graph.setEdge({ i: name, j: subName, e: undefined });
      }
    }
  }
  return graph;
};

export const findRoot = (graph: GroupGraph, visited: Set<string>): string => {
  const roots = graph.sources();
  const nodes = roots.length > 0 ? roots : graph.nodes();
  // Find the smallest node out of those unvisited
  return nodes
    .filter((node) => !visited.has(node))
    .reduce((currMinNode, newNode) => {
      return graph.node(currMinNode) < graph.node(newNode)
        ? currMinNode
        : newNode;
    });
};
