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
