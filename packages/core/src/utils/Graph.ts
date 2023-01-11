interface Arc<I, E> {
  /** the other node */
  i: I;
  /** edge label */
  e: E;
}

interface Vert<I, L, E> {
  /** label */
  l: L;
  /** in-edges */
  p: Arc<I, E>[];
  /** out-edges */
  s: Arc<I, E>[];
}

export interface Edge<I, E> {
  i: I;
  j: I;
  e: E;
}

export default class Graph<I, L = undefined, E = undefined> {
  private g: Map<I, Vert<I, L, E>> = new Map();

  private get(i: I): Vert<I, L, E> {
    const v = this.g.get(i);
    if (v === undefined) throw Error(`node ID ${i} not found`);
    return v;
  }

  setNode(i: I, l: L): void {
    if (this.g.has(i)) throw Error(`node with ID ${i} already exists`);
    this.g.set(i, { l, p: [], s: [] });
  }

  nodes(): I[] {
    return [...this.g.keys()];
  }

  node(i: I): L {
    return this.get(i).l;
  }

  setEdge({ i, j, e }: Edge<I, E>): void {
    this.get(i).s.push({ i: j, e });
    this.get(j).p.push({ i: i, e });
  }

  inEdges(i: I): Edge<I, E>[] {
    return this.get(i).p.map(({ i: j, e }) => ({ i: j, j: i, e }));
  }

  outEdges(i: I): Edge<I, E>[] {
    return this.get(i).s.map(({ i: j, e }) => ({ i, j, e }));
  }

  nodeCount(): number {
    return this.g.size;
  }

  sources(): I[] {
    const a = [];
    for (const [i, v] of this.g) if (v.p.length === 0) a.push(i);
    return a;
  }

  topsort(): I[] {
    return []; // TODO
  }

  descendants(i: I): I[] {
    return []; // TODO
  }

  findCycles(): I[][] {
    return []; // TODO
  }
}
