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
    if (v === undefined) throw Error(`node ID not found: ${i}`);
    return v;
  }

  setNode(i: I, l: L): void {
    let v = this.g.get(i);
    if (v === undefined) v = { l, p: [], s: [] };
    else v.l = l;
    this.g.set(i, v);
  }

  hasNode(i: I): boolean {
    return this.g.has(i);
  }

  nodes(): I[] {
    return [...this.g.keys()];
  }

  node(i: I): L {
    return this.get(i).l;
  }

  setEdge({ i, j, e }: Edge<I, E>, labelMissing?: () => L): void {
    if (labelMissing === undefined) {
      labelMissing = () => {
        throw Error(`node ID not found: ${i}`);
      };
    }

    let v = this.g.get(i);
    if (v === undefined) {
      v = { l: labelMissing(), p: [], s: [] };
      this.g.set(i, v);
    }

    let w = this.g.get(j);
    if (w === undefined) {
      w = { l: labelMissing(), p: [], s: [] };
      this.g.set(j, w);
    }

    v.s.push({ i: j, e });
    w.p.push({ i: i, e });
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
    const xs = [];
    for (const [i, v] of this.g) if (v.p.length === 0) xs.push(i);
    return xs;
  }

  topsort(): I[] {
    const xs: I[] = [];
    const indegree = new Map<I, number>();
    for (const [i, v] of this.g) indegree.set(i, v.p.length);
    const stack = this.sources();
    while (stack.length > 0) {
      const i = stack.pop() as I;
      xs.push(i);
      for (const { i: j } of this.get(i).s) {
        const n = indegree.get(j)!;
        if (n === 1) stack.push(j);
        indegree.set(j, n - 1);
      }
    }
    return xs;
  }

  descendants(i: I): Set<I> {
    const xs = new Set<I>();
    const stack = [i];
    while (stack.length > 0) {
      const j = stack.pop() as I;
      xs.add(j);
      for (const { i: k } of this.get(j).s) if (!xs.has(k)) stack.push(k);
    }
    return xs;
  }

  findCycles(): I[][] {
    return []; // TODO
  }
}
