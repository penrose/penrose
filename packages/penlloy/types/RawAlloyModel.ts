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

export interface RawAlloyModel {
  sigs: ModelSig[];
  rels: ModelRel[];
}
