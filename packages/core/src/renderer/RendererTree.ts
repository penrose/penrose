import { Shape } from "types/shape";

export type RendererTreeNode = {
  shape: Shape;
  index: number;
  shapeType: string;
  children: string[]; // names
  parents: string[]; // names
};

export type RendererTree = { [name: string]: RendererTreeNode };

const makePartialRendererTreeNode = (
  shape: Shape,
  index: number
): [string, RendererTreeNode] => {
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

export const makeRendererTree = (shapes: Shape[]): RendererTree => {
  console.log(shapes);
  const tree: RendererTree = {};
  for (let i = 0; i < shapes.length; i++) {
    const shape = shapes[i];
    const [shapeName, shapeNode] = makePartialRendererTreeNode(shape, i);
    tree[shapeName] = shapeNode;
  }
  // Now populate the parents fields
  for (const [name, node] of Object.entries(tree)) {
    for (const childName of node.children) {
      tree[childName].parents.push(name);
    }
  }
  // Each node cannot have more than one parents
  for (const [name, node] of Object.entries(tree)) {
    if (node.parents.length > 1) {
      throw Error("Shape " + name + " is contained by more than one groups");
    }
  }
  return tree;
};

export const findRoot = (tree: RendererTree): string => {
  const allRoots = Object.entries(tree).filter(
    ([, node]) => node.parents.length === 0
  );
  if (allRoots.length === 0) {
    throw Error("Cannot find root");
  }
  const smallestRoot = allRoots.reduce((currMin, newElem) => {
    return currMin[1].index < newElem[1].index ? currMin : newElem;
  });
  return smallestRoot[0];
};
