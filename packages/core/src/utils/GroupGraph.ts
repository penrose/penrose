import { ShapeAD } from "../types/shape";
import Graph from "./Graph";
import { shapeListV } from "./Util";

export type GroupGraph = Graph<string, number>; // shape name and index

export const makeGroupGraph = (shapes: ShapeAD[]): GroupGraph => {
  const graph: GroupGraph = new Graph();
  const nameShapeMap = new Map<string, ShapeAD>();
  // populate the nodes
  for (let i = 0; i < shapes.length; i++) {
    const shape = shapes[i];
    const shapeNameVal = shape.properties["name"];
    if (shapeNameVal.tag !== "StrV") {
      throw Error("Shape name is not a string");
    }
    const shapeName = shapeNameVal.contents;

    graph.setNode(shapeName, i);
    nameShapeMap.set(shapeName, shape);
  }
  // then populate the edges
  for (const [name, shape] of nameShapeMap.entries()) {
    if (shape.shapeType === "Group") {
      const subShapesVal = shape.properties["shapes"];
      if (subShapesVal.tag !== "GPIListV") {
        throw Error("Group content not a list of shapes");
      }
      const subShapes = subShapesVal.contents;
      const subNames = subShapes.map((shape) => {
        const subShapeNameVal = shape.contents[1]["name"];
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

export const getParent = (
  graph: GroupGraph,
  node: string
): string | undefined => {
  const parents = graph.parents(node);
  if (parents.length === 0) {
    return undefined;
  } else {
    return parents[0];
  }
};

export const traverseUp = (graph: GroupGraph, node: string): string[] => {
  const path: string[] = [];
  let currNode: string | undefined = node;
  while (currNode) {
    path.push(currNode);
    currNode = getParent(graph, currNode);
  }
  return path;
};

export const findOrderedRoots = (graph: GroupGraph): string[] => {
  const roots = graph.sources();
  const nodes = roots.length > 0 ? roots : graph.nodes();
  return nodes.sort((a, b) => graph.node(a) - graph.node(b));
};

export type RenderGraph = RenderGraphNode[];
export type RenderGraphNode = ShapeAD;

export const buildRenderGraphNode = (
  name: string,
  groupGraph: GroupGraph,
  nameShapeMap: Map<string, ShapeAD>
): RenderGraphNode => {
  const shape = nameShapeMap.get(name);
  if (!shape) {
    throw new Error("Cannot find shape name in name-shape map");
  }

  // If shape is non-group, return it as the node.

  if (shape.shapeType !== "Group") {
    return shape;
  }

  // If shape is group, recursively handle all the sub-shapes.
  // And convert GPIListV to ShapeListV.

  const subGPIsVal = shape.properties["shapes"];
  if (subGPIsVal.tag !== "GPIListV") {
    throw Error("Group content not a list of shapes");
  }
  const childrenNames = subGPIsVal.contents.map((gpi) => {
    const subNameVal = gpi.contents[1]["name"];
    if (subNameVal.tag !== "StrV") {
      throw Error("Shape name is not a string");
    }
    return subNameVal.contents;
  });

  shape.properties["shapes"] = shapeListV(
    buildRenderGraph(childrenNames, groupGraph, nameShapeMap)
  );

  return shape;
};

export const buildRenderGraph = (
  roots: string[],
  groupGraph: GroupGraph,
  nameShapeMap: Map<string, ShapeAD>
): RenderGraph => {
  const orderedRoots = roots.sort(
    (a, b) => groupGraph.node(a) - groupGraph.node(b)
  );
  return orderedRoots.map((root) =>
    buildRenderGraphNode(root, groupGraph, nameShapeMap)
  );
};
