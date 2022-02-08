import {
  CompressedAdjacencyGraph,
  createAdjacencyMatrix,
  createCompressedMatrix,
  NodeWithEdges,
  Vertex,
} from "./Graph";
class IntNode implements NodeWithEdges<number> {
  value: number;
  children: NodeWithEdges<number>[] = [];
  constructor(value: number) {
    this.value = value;
  }
}

describe("Adjacency matrix creation tests", () => {
  test("cycle between two nodes", () => {
    const one = new IntNode(1);
    const two = new IntNode(2);
    one.children.push(two);
    two.children.push(one);
    expect(createAdjacencyMatrix([one, two])).toEqual([
      [false, true],
      [true, false],
    ]);
  });
  test("no edges two nodes", () => {
    const one = new IntNode(1);
    const two = new IntNode(2);
    expect(createAdjacencyMatrix([one, two])).toEqual([
      [false, false],
      [false, false],
    ]);
  });
  test("six node tree", () => {
    const one = new IntNode(1);
    const two = new IntNode(2);
    const three = new IntNode(3);
    const four = new IntNode(4);
    const five = new IntNode(5);
    const six = new IntNode(6);
    one.children.push(two);
    two.children.push(three);
    three.children.push(four);
    three.children.push(five);
    three.children.push(six);
    expect(createAdjacencyMatrix([one, two, three, four, five, six])).toEqual([
      [false, true, false, false, false, false],
      [false, false, true, false, false, false],
      [false, false, false, true, true, true],
      [false, false, false, false, false, false],
      [false, false, false, false, false, false],
      [false, false, false, false, false, false],
    ]);
  });
});

describe("Compressed Adjacency Matrix tests", () => {
  test("cycle between two nodes", () => {
    const adjacencyMatrix = [
      [false, true],
      [true, false],
    ];
    const cm = createCompressedMatrix(adjacencyMatrix);
    expect(cm.rowIndices).toEqual([1, 0]);
    expect(cm.cumulativeEntriesByColumn).toEqual([1, 2]);
    expect(cm.columnIndices).toEqual([1, 0]);
    expect(cm.cumulativeEntriesByRow).toEqual([1, 2]);
  });
  test("keenan slide http://15462.courses.cs.cmu.edu/fall2021/lecture/meshes/slide_021", () => {
    const adjacencyMatrix = [
      [true, true, false],
      [false, false, true],
      [false, true, false],
    ];
    const cm = createCompressedMatrix(adjacencyMatrix);
    expect(cm.rowIndices).toEqual([0, 0, 2, 1]);
    expect(cm.cumulativeEntriesByColumn).toEqual([1, 3, 4]);
    expect(cm.columnIndices).toEqual([0, 1, 2, 1]);
    expect(cm.cumulativeEntriesByRow).toEqual([2, 3, 4]);
  });
});

describe("Graph adjacency matrix tests", () => {
  test("keenan slide http://15462.courses.cs.cmu.edu/fall2021/lecture/meshes/slide_021", () => {
    const adjacencyMatrix = [
      [true, true, false],
      [false, false, true],
      [false, true, false],
    ];
    const cm = createCompressedMatrix(adjacencyMatrix);
    const nodeList: Vertex<number>[] = [
      { index: 0, value: 1 },
      { index: 1, value: 2 },
      { index: 2, value: 3 },
    ];
    const g = new CompressedAdjacencyGraph(nodeList, cm);
    expect(g.childrenOf(0)).toEqual([0]);
    expect(g.childrenOf(1)).toEqual([0, 2]);
    expect(g.childrenOf(2)).toEqual([1]);
    expect(g.parentsOf(0)).toEqual([0, 1]);
  });
  test("one node, no edges test", () => {
    const adjacencyMatrix = [[false]];
    const cm = createCompressedMatrix(adjacencyMatrix);
    const nodeList: Vertex<number>[] = [{ index: 0, value: 1 }];
    const g = new CompressedAdjacencyGraph(nodeList, cm);
    expect(g.childrenOf(0)).toEqual([]);
  });
  test("one node, self-edge test", () => {
    const adjacencyMatrix = [[true]];
    const cm = createCompressedMatrix(adjacencyMatrix);
    const nodeList: Vertex<number>[] = [{ index: 0, value: 1 }];
    const g = new CompressedAdjacencyGraph(nodeList, cm);
    expect(g.childrenOf(0)).toEqual([0]);
    expect(g.childrenOf(0)).toEqual([0]);
  });
});
