import * as graphlib from "graphlib";

export interface Edge<NodeId extends string> {
  v: NodeId;
  w: NodeId;
}

/**
 * A better-typed `graphlib` Graph with options `{}`. Edge labels and the graph
 * label are all `undefined`, and default node labels are unsupported. All edge
 * methods provide only the signature using the `Edge` object, not the
 * positional signature. Methods which would have returned `undefined` for
 * missing node IDs instead return an empty array. A `topsort` method is
 * provided which calls `graphlib.alg.topsort`.
 */
export class Digraph<NodeId extends string, NodeLabel> {
  private _graph: graphlib.Graph;

  constructor() {
    this._graph = new graphlib.Graph({});
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

  setEdge(edge: Edge<NodeId>): this {
    this._graph.setEdge(edge);
    return this;
  }

  edges(): Edge<NodeId>[] {
    return this._graph.edges() as Edge<NodeId>[];
  }

  hasEdge(edge: Edge<NodeId>): boolean {
    return this._graph.hasEdge(edge);
  }

  removeEdge(edge: Edge<NodeId>): this {
    this._graph.removeEdge(edge);
    return this;
  }

  inEdges(v: NodeId, w?: NodeId): Edge<NodeId>[] {
    return (this._graph.inEdges(v, w) as undefined | Edge<NodeId>[]) ?? [];
  }

  outEdges(v: NodeId, w?: NodeId): Edge<NodeId>[] {
    return (this._graph.outEdges(v, w) as undefined | Edge<NodeId>[]) ?? [];
  }

  nodeEdges(v: NodeId, w?: NodeId): Edge<NodeId>[] {
    return (this._graph.nodeEdges(v, w) as undefined | Edge<NodeId>[]) ?? [];
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

export interface MultiEdge<
  NodeId extends string,
  EdgeName extends string | undefined
> extends Edge<NodeId> {
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

  setEdge(edge: MultiEdge<NodeId, EdgeName>): this {
    this._graph.setEdge(edge);
    return this;
  }

  edges(): MultiEdge<NodeId, EdgeName>[] {
    return this._graph.edges() as MultiEdge<NodeId, EdgeName>[];
  }

  hasEdge(edge: MultiEdge<NodeId, EdgeName>): boolean {
    return this._graph.hasEdge(edge);
  }

  removeEdge(edge: MultiEdge<NodeId, EdgeName>): this {
    this._graph.removeEdge(edge);
    return this;
  }

  inEdges(v: NodeId, w?: NodeId): MultiEdge<NodeId, EdgeName>[] {
    return (
      (this._graph.inEdges(v, w) as
        | undefined
        | MultiEdge<NodeId, EdgeName>[]) ?? []
    );
  }

  outEdges(v: NodeId, w?: NodeId): MultiEdge<NodeId, EdgeName>[] {
    return (
      (this._graph.outEdges(v, w) as
        | undefined
        | MultiEdge<NodeId, EdgeName>[]) ?? []
    );
  }

  nodeEdges(v: NodeId, w?: NodeId): MultiEdge<NodeId, EdgeName>[] {
    return (
      (this._graph.nodeEdges(v, w) as
        | undefined
        | MultiEdge<NodeId, EdgeName>[]) ?? []
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
