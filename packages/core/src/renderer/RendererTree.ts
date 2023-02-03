import { Shape } from "types/shape";

export type RendererTreeNode = {
  shape: Shape;
  shapeType: string;
  children: string[]; // names
  parents: string[]; // names
};

export type RendererTree = { [name: string]: RendererTreeNode };

const makePartialRendererTreeNode = (
  shape: Shape
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
        shapeType,
        children: [],
        parents: [],
      },
    ];
  }
};

export const makeRendererTree = (shapes: Shape[]): RendererTree => {
  const tree: RendererTree = {};
  for (const shape of shapes) {
    const [shapeName, shapeNode] = makePartialRendererTreeNode(shape);
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
  const rootEntry = Object.entries(tree).find(
    ([, node]) => node.parents.length === 0
  );
  if (!rootEntry) {
    throw Error("The group relation is cyclic.");
  } else {
    return rootEntry[0];
  }
};
