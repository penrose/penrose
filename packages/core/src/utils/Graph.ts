import * as graphlib from "graphlib";

//#region compressed matrix

export interface NodeWithEdges<T> {
  value: T;
  children: NodeWithEdges<T>[];
}

export interface Vertex<T> {
  index: number;
  value: T;
}

export const createAdjacencyMatrix = <T>(
  nodeList: NodeWithEdges<T>[]
): boolean[][] => {
  const matrix = createSquareMatrix(nodeList.length, false);
  let i = 0;
  nodeList.forEach((node) => {
    node.children.forEach((childNode) => {
      const j = nodeList.lastIndexOf(childNode);
      matrix[i][j] = true;
    });
    i++;
  });
  return matrix;
};

export interface CompressedMatrix {
  rowIndices: Array<number>;
  cumulativeEntriesByColumn: Array<number>;
  columnIndices: Array<number>;
  cumulativeEntriesByRow: Array<number>;
}

// immutable Graph
export interface Graph<T> {
  nodeList: Vertex<T>[];
  parentsOf: (nodeIndex: number) => number[];
  childrenOf: (nodeIndex: number) => number[];
}

export class CompressedAdjacencyGraph<T> implements Graph<T> {
  nodeList: Vertex<T>[];
  cm: CompressedMatrix;

  constructor(nodeList: Vertex<T>[], cm: CompressedMatrix) {
    this.nodeList = nodeList;
    this.cm = cm;
  }
  parentsOf = (nodeIndex: number): number[] => {
    let startIndex = 0;
    if (nodeIndex > 0) {
      startIndex = this.cm.cumulativeEntriesByRow[nodeIndex - 1];
    }
    const endIndex = this.cm.cumulativeEntriesByRow[nodeIndex];
    return this.cm.columnIndices.slice(startIndex, endIndex);
  };
  childrenOf = (nodeIndex: number): number[] => {
    let startIndex = 0;
    if (nodeIndex > 0) {
      startIndex = this.cm.cumulativeEntriesByColumn[nodeIndex - 1];
    }
    const endIndex = this.cm.cumulativeEntriesByColumn[nodeIndex];
    return this.cm.rowIndices.slice(startIndex, endIndex);
  };
}

export const createCompressedMatrix = (
  adjacencyMatrix: boolean[][]
): CompressedMatrix => {
  const cm = {
    rowIndices: new Array<number>(),
    cumulativeEntriesByColumn: new Array<number>(),
    columnIndices: new Array<number>(),
    cumulativeEntriesByRow: new Array<number>(),
  };
  let cumulativeEntries = 0;
  const size = adjacencyMatrix.length;
  for (let i = 0; i < size; i++) {
    for (let j = 0; j < size; j++) {
      if (adjacencyMatrix[j][i]) {
        cm.rowIndices.push(j);
        cumulativeEntries++;
      }
    }
    cm.cumulativeEntriesByColumn.push(cumulativeEntries);
  }
  cumulativeEntries = 0;
  for (let i = 0; i < size; i++) {
    for (let j = 0; j < size; j++) {
      if (adjacencyMatrix[i][j]) {
        cm.columnIndices.push(j);
        cumulativeEntries++;
      }
    }
    cm.cumulativeEntriesByRow.push(cumulativeEntries);
  }
  return cm;
};

function createSquareMatrix(width: number, defaultValue = false): boolean[][] {
  const matrix = [];
  for (let i = 0; i < width; i++) {
    const row: boolean[] = [];
    for (let j = 0; j < width; j++) {
      row.push(defaultValue);
    }
    matrix.push(row);
  }
  return matrix;
}

//#endregion

//#region graphlib wrapper

export interface Edge<
  NodeId extends string,
  EdgeName extends string | undefined
> {
  v: NodeId;
  w: NodeId;
  name: EdgeName;
}

/**
 * A better-typed `graphlib` Graph with options `{ multigraph: true }`. Edge
 * labels and the graph label are all `undefined`, and default node labels are
 * unsupported. All edge methods provide only the signature using the `Edge`
 * object, not the positional signature. Methods which would have returned
 * `undefined` for missing node IDs instead return an empty array. A `topsort`
 * method is provided which calls `graphlib.alg.topsort`.
 */
export class Multidigraph<
  NodeId extends string,
  NodeLabel,
  EdgeName extends string | undefined
> {
  private _graph: graphlib.Graph;

  constructor() {
    this._graph = new graphlib.Graph({ multigraph: true });
  }

  setNode(name: NodeId, label: NodeLabel): this {
    this._graph.setNode(name, label);
    return this;
  }

  setNodes(names: NodeId[], label: NodeLabel): this {
    this._graph.setNodes(names, label);
    return this;
  }

  filterNodes(filter: (v: NodeId) => boolean): this {
    this._graph.filterNodes((v) => filter(v as NodeId));
    return this;
  }

  setPath(nodes: NodeId[]): this {
    this._graph.setPath(nodes);
    return this;
  }

  hasNode(name: NodeId): boolean {
    return this._graph.hasNode(name);
  }

  removeNode(name: NodeId): this {
    this._graph.removeNode(name);
    return this;
  }

  nodes(): NodeId[] {
    return this._graph.nodes() as NodeId[];
  }

  node(name: NodeId): NodeLabel {
    return this._graph.node(name);
  }

  setEdge(edge: Edge<NodeId, EdgeName>): this {
    this._graph.setEdge(edge);
    return this;
  }

  edges(): Edge<NodeId, EdgeName>[] {
    return this._graph.edges() as Edge<NodeId, EdgeName>[];
  }

  hasEdge(edge: Edge<NodeId, EdgeName>): boolean {
    return this._graph.hasEdge(edge);
  }

  removeEdge(edge: Edge<NodeId, EdgeName>): this {
    this._graph.removeEdge(edge);
    return this;
  }

  inEdges(v: NodeId, w?: NodeId): Edge<NodeId, EdgeName>[] {
    return (
      (this._graph.inEdges(v, w) as undefined | Edge<NodeId, EdgeName>[]) ?? []
    );
  }

  outEdges(v: NodeId, w?: NodeId): Edge<NodeId, EdgeName>[] {
    return (
      (this._graph.outEdges(v, w) as undefined | Edge<NodeId, EdgeName>[]) ?? []
    );
  }

  nodeEdges(v: NodeId, w?: NodeId): Edge<NodeId, EdgeName>[] {
    return (
      (this._graph.nodeEdges(v, w) as undefined | Edge<NodeId, EdgeName>[]) ??
      []
    );
  }

  predecessors(v: NodeId): NodeId[] {
    return (this._graph.predecessors(v) as undefined | NodeId[]) ?? [];
  }

  successors(v: NodeId): NodeId[] {
    return (this._graph.successors(v) as undefined | NodeId[]) ?? [];
  }

  neighbors(v: NodeId): NodeId[] {
    return (this._graph.neighbors(v) as undefined | NodeId[]) ?? [];
  }

  nodeCount(): number {
    return this._graph.nodeCount();
  }

  edgeCount(): number {
    return this._graph.edgeCount();
  }

  sources(): NodeId[] {
    return this._graph.sources() as NodeId[];
  }

  sinks(): NodeId[] {
    return this._graph.sinks() as NodeId[];
  }

  topsort(): NodeId[] {
    return graphlib.alg.topsort(this._graph) as NodeId[];
  }
}

//#endregion
