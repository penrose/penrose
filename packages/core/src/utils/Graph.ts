// `i` and `I` stand for "index" or "ID", since we use them for node keys

/** internal: half of an edge, where direction and one node are implicit */
interface Arc<I, E> {
  /** the other node */
  i: I;
  /** edge label */
  e: E;
}

/** internal: label and connectivity of a node */
interface Vert<I, L, E> {
  /** node label */
  l: L;
  /** in-edges; `p` stands for "predecessor" */
  p: Arc<I, E>[];
  /** out-edges; `s` stands for "successor" */
  s: Arc<I, E>[];
}

/** directed edge */
export interface Edge<I, E> {
  /** initial vertex */
  i: I;
  /** terminal vertex */
  j: I;
  /** edge label */
  e: E;
}

/** directed multigraph with node keys `I` and labels `L` and edge labels `E` */
export default class Graph<I, L = undefined, E = undefined> {
  private g = new Map<I, Vert<I, L, E>>();

  private get(i: I): Vert<I, L, E> {
    const v = this.g.get(i);
    if (v === undefined) throw Error(`node ID not found: ${i}`);
    return v;
  }

  /** set `i`'s label to `l`, adding `i` to the graph if not already present */
  setNode(i: I, l: L): void {
    const v = this.g.get(i);
    if (v === undefined) this.g.set(i, { l, p: [], s: [] });
    else v.l = l;
  }

  /** @returns whether `i` is in the graph */
  hasNode(i: I): boolean {
    return this.g.has(i);
  }

  /** @returns fresh array of all node keys in the graph */
  nodes(): I[] {
    return [...this.g.keys()];
  }

  /** @returns `i`'s label */
  node(i: I): L {
    return this.get(i).l;
  }

  /**
   * add an edge from `i` to `j` with label `e`
   * @param labelMissing called if `i` or `j`'s label is missing; default throws
   */
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

  /**
   * throws if `i` is absent
   * @returns fresh array of `i`'s in-edges
   */
  inEdges(i: I): Edge<I, E>[] {
    return this.get(i).p.map(({ i: j, e }) => ({ i: j, j: i, e }));
  }

  /**
   * throws if `i` is absent
   * @returns fresh array of `i`'s out-edges
   */
  outEdges(i: I): Edge<I, E>[] {
    return this.get(i).s.map(({ i: j, e }) => ({ i, j, e }));
  }

  /** @returns number of nodes in the graph */
  nodeCount(): number {
    return this.g.size;
  }

  /** @returns fresh array of all nodes that have no in-edges */
  sources(): I[] {
    const xs = [];
    for (const [i, v] of this.g) if (v.p.length === 0) xs.push(i);
    return xs;
  }

  /** @returns fresh array of all nodes that have no out-edges */
  sinks(): I[] {
    const xs = [];
    for (const [i, v] of this.g) if (v.s.length === 0) xs.push(i);
    return xs;
  }

  /**
   * throws if the graph contains a cycle
   * @returns fresh array of all nodes in topological order
   */
  topsort(): I[] {
    // we want this to be somewhat stable, so by using a stack to compute the
    // reverse topological sort and then reversing at the end, we at least
    // guarantee that a graph with no edges will just return its nodes in their
    // original insertion order
    const xs: I[] = [];
    const outdegree = new Map<I, number>();
    for (const [i, v] of this.g) outdegree.set(i, v.s.length);
    const stack = this.sinks();
    while (stack.length > 0) {
      const i = stack.pop() as I;
      xs.push(i);
      for (const { i: j } of this.get(i).p) {
        const d = outdegree.get(j)!;
        if (d === 1) stack.push(j);
        outdegree.set(j, d - 1);
      }
    }
    const m = xs.length;
    const n = this.g.size;
    if (m !== n) throw Error(`could only sort ${m} nodes out of ${n} total`);
    return xs.reverse();
  }

  /**
   * throws if `i` is absent
   * @returns fresh set of all nodes reachable from `i`, including `i`
   */
  descendants(i: I): Set<I> {
    const xs = new Set<I>();
    const stack = [i]; // depth-first search
    while (stack.length > 0) {
      const j = stack.pop() as I;
      xs.add(j);
      for (const { i: k } of this.get(j).s) if (!xs.has(k)) stack.push(k);
    }
    return xs;
  }

  /**
   * not guaranteed to be exhaustive
   * @returns empty array if acyclic, else nonempty array of cycles
   */
  findCycles(): I[][] {
    const cycles: I[][] = [];
    const unvisited = new Set(this.g.keys());
    while (unvisited.size > 0) {
      // depth-first search from an arbitrary node until we either find a cycle
      // or find everything reachable from this node
      let i: I = unvisited.values().next().value; // starting node
      const stack = [i]; // our current path from the starting node
      const succs = new Map<I, I[]>(); // allow efficient cycle detection check
      while (stack.length > 0) {
        i = stack[stack.length - 1];
        unvisited.delete(i);
        let s = succs.get(i);
        if (s === undefined) {
          // lazily populate array of successors
          s = this.get(i).s.map(({ i: j }) => j);
          succs.set(i, s);
        }
        if (s.length > 0) {
          const j = s.pop()!;
          if (succs.has(j)) {
            cycles.push([...stack.slice(stack.indexOf(j)), j]);
            break;
          }
          if (unvisited.has(j)) stack.push(j);
        } else {
          // nothing else to explore from this node; backtrack
          stack.pop();
          succs.delete(i); // `succs`'s keys should be `stack`'s elements
        }
      }
    }
    return cycles;
  }
}
