export interface AlloyInstanceSig {
  name: string;
  id: number; // remove if unused
  atoms: string[];
}

export interface AlloyInstanceRel {
  name: string;
  belongsTo: string;
  tuples: string[][];
}
export interface AlloyInstance {
  sigs: AlloyInstanceSig[];
  rels: AlloyInstanceRel[];
}
