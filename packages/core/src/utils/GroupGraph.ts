import { Shape, ShapeAD } from "../types/shape";
import Graph from "./Graph";

export type GroupGraphNode<T extends Shape | ShapeAD> = {
  shape: T;
  index: number;
  shapeType: string;
  children: string[]; // names
  parents: string[]; // names
};

export type GroupGraph<T extends Shape | ShapeAD> = {
  [name: string]: GroupGraphNode<T>;
};

const makePartialGroupGraphNode = <T extends Shape | ShapeAD>(
  shape: T,
  index: number
): [string, GroupGraphNode<T>] => {
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
        shapeType,
        children: subShapeNames,
        parents: [],
      },
    ];
  } else {
    return [
      shapeName,
      {
        shape,
        index,
        shapeType,
        children: [],
        parents: [],
      },
    ];
  }
};

export const makeGroupGraph = <T extends Shape | ShapeAD>(
  shapes: T[]
): GroupGraph<T> => {
  const tree: GroupGraph<T> = {};
  for (let i = 0; i < shapes.length; i++) {
    const shape = shapes[i];
    const [shapeName, shapeNode] = makePartialGroupGraphNode(shape, i);
    tree[shapeName] = shapeNode;
  }
  // Now populate the parents fields
  for (const [name, node] of Object.entries(tree)) {
    for (const childName of node.children) {
      tree[childName].parents.push(name);
    }
  }
  return tree;
};

export const findRoot = <T extends Shape | ShapeAD>(
  graph: GroupGraph<T>
): string => {
  let allRoots = Object.entries(graph).filter(
    ([, node]) => node.parents.length === 0
  );
  if (allRoots.length === 0) {
    allRoots = Object.entries(graph);
  }
  const smallestRoot = allRoots.reduce((currMin, newElem) => {
    return currMin[1].index < newElem[1].index ? currMin : newElem;
  });
  return smallestRoot[0];
};

export const findCycles = <T extends Shape | ShapeAD>(
  graph: GroupGraph<T>
): string[][] => {
  const rawGraph = new Graph<string>();
  for (const name of Object.keys(graph)) {
    rawGraph.setNode(name, undefined);
  }
  for (const [name, node] of Object.entries(graph)) {
    for (const child of node.children) {
      rawGraph.setEdge({ i: name, j: child, e: undefined });
    }
  }

  return rawGraph.findCycles();
};
