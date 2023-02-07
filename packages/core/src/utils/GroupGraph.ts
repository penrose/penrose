import { Shape, ShapeAD } from "../types/shape";
import Graph from "./Graph";

export type GroupGraphNode<T extends Shape | ShapeAD> = {
  shape: T;
  index: number;
};

export type GroupGraph<T extends Shape | ShapeAD> = Graph<
  string,
  GroupGraphNode<T>
>;

const makeGroupGraphNode = <T extends Shape | ShapeAD>(
  shape: T,
  index: number
): [string, GroupGraphNode<T>, string[]] => {
  const shapeType = shape.shapeType;
  const shapeNameVal = shape.properties["name"];
  if (shapeNameVal.tag !== "StrV") {
    throw Error("Shape name is not a string");
  }
  const shapeName = shapeNameVal.contents;

  if (shapeType === "Group") {
    const subGPIsVal = shape.properties["shapes"];
    if (subGPIsVal.tag !== "ShapeListV") {
      throw Error("Group content not a list of shapes");
    }
    const subGPIs = subGPIsVal.contents;
    const subShapeNames = subGPIs.map((gpi) => {
      const subShapeNameVal = gpi.contents[1]["name"];
      if (subShapeNameVal.tag !== "StrV") {
        throw Error("Shape name is not a string");
      }
      return subShapeNameVal.contents;
    });
    return [
      shapeName,
      {
        shape,
        index,
      },
      subShapeNames,
    ];
  } else {
    return [
      shapeName,
      {
        shape,
        index,
      },
      [],
    ];
  }
};

export const makeGroupGraph = <T extends Shape | ShapeAD>(
  shapes: T[]
): GroupGraph<T> => {
  const graph: GroupGraph<T> = new Graph();
  const rawGraph: { [k: string]: [GroupGraphNode<T>, string[]] } = {};
  for (let i = 0; i < shapes.length; i++) {
    const [name, node, children] = makeGroupGraphNode(shapes[i], i);
    rawGraph[name] = [node, children];
  }

  for (const [name, [node, children]] of Object.entries(rawGraph)) {
    graph.setNode(name, node);
    for (const child of children) {
      graph.setNode(child, rawGraph[child][0]);
      graph.setEdge({ i: name, j: child, e: undefined });
    }
  }
  return graph;
};

export const findRoot = <T extends Shape | ShapeAD>(
  graph: GroupGraph<T>
): string => {
  const roots = graph.sources();
  if (roots.length === 0) {
    // If no root, find the node with lowest layer
    return graph.nodes().reduce((currMinNode, newNode) => {
      return graph.node(currMinNode).index < graph.node(newNode).index
        ? currMinNode
        : newNode;
    });
  } else {
    // If has root, find the root with lowest layer
    return roots.reduce((currMinRoot, newRoot) => {
      return graph.node(currMinRoot).index < graph.node(newRoot).index
        ? currMinRoot
        : newRoot;
    });
  }
};

export const findCycles = <T extends Shape | ShapeAD>(
  graph: GroupGraph<T>
): string[][] => {
  return graph.findCycles();
};
