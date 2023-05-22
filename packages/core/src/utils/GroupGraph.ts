import { Shape } from "../shapes/Shapes.js";
import * as ad from "../types/ad.js";
import Graph from "./Graph.js";
import { getAdValueAsString, getValueAsShapeList, shapeListV } from "./Util.js";

export type GroupGraph = Graph<string, number>; // shape name and index

export const makeGroupGraph = (shapes: Shape<ad.Num>[]): GroupGraph => {
  const graph: GroupGraph = new Graph();
  const nameShapeMap = new Map<string, Shape<ad.Num>>();
  // populate the nodes
  for (let i = 0; i < shapes.length; i++) {
    const shape = shapes[i];
    const shapeName = getAdValueAsString(shape.name);

    graph.setNode(shapeName, i);
    nameShapeMap.set(shapeName, shape);
  }
  // then populate the edges
  for (const [name, shape] of nameShapeMap.entries()) {
    if (shape.shapeType === "Group") {
      const subShapes = shape.shapes.contents;
      const subNames = subShapes.map((subShape) => {
        return subShape.name.contents;
      });

      for (const subName of subNames) {
        graph.setEdge({ i: name, j: subName, e: undefined });
      }

      const clip = shape.clipPath.contents;
      if (clip.tag === "Clip") {
        const clipName = clip.contents.name.contents;
        graph.setEdge({ i: name, j: clipName, e: undefined });
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
export type RenderGraphNode = Shape<ad.Num>;

export const buildRenderGraphNode = (
  name: string,
  groupGraph: GroupGraph,
  nameShapeMap: Map<string, Shape<ad.Num>>
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

  const subShapes = getValueAsShapeList(shape.shapes);
  const childrenNames = subShapes.map((subShape) =>
    getAdValueAsString(subShape.name)
  );

  shape.shapes = shapeListV(
    buildRenderGraph(childrenNames, groupGraph, nameShapeMap)
  );

  return shape;
};

export const buildRenderGraph = (
  roots: string[],
  groupGraph: GroupGraph,
  nameShapeMap: Map<string, Shape<ad.Num>>
): RenderGraph => {
  const orderedRoots = roots.sort(
    (a, b) => groupGraph.node(a) - groupGraph.node(b)
  );
  return orderedRoots.map((root) =>
    buildRenderGraphNode(root, groupGraph, nameShapeMap)
  );
};
