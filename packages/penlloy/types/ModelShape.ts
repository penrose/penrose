export interface ModelSig {
  name: string;
  parent?: string;
  supersets?: string[];
}

export interface ModelRel {
  name: string;
  type: string;
  sig: string;
}

export interface ModelShape {
  sigs: ModelSig[];
  rels: ModelRel[];
}
