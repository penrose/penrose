export interface RawAlloyExported {
  alloy: RawAlloyInternal;
}

export interface RawAlloyInternal {
  instance: RawAlloyInstance;
  _attributes: {
    builddate: string;
  };
}

export interface RawAlloyInstance {
  sig?: RawAlloySig | RawAlloySig[];
  field?: RawAlloyField | RawAlloyField[];
}

export interface RawAlloySig {
  _attributes: {
    label: string;
    ID: string;
    builtin: "yes" | "no";
  };
  atom?: RawAlloyAtom | RawAlloyAtom[];
}

export interface RawAlloyAtom {
  _attributes: {
    label: string;
  };
}

export interface RawAlloyField {
  _attributes: {
    label: string;
    ID: string;
    parentID: string;
  };
  tuple?: RawAlloyTuple | RawAlloyTuple[];
}

export interface RawAlloyTuple {
  atom?: RawAlloyAtom | RawAlloyAtom[];
}
