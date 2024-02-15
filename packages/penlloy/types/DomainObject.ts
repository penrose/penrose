export type SingleSigName = {
  tag: "SingleSigName";
  contents: string;
};

export type ConjunctionSigName = {
  tag: "ConjunctionSigName";
  contents: string[];
};

export type SigName = SingleSigName | ConjunctionSigName;

export interface DomainSigType {
  tag: "DomainSigType";
  contents: SingleSigName;
}

export interface DomainConjunctionType {
  tag: "DomainConjunctionType";
  contents: ConjunctionSigName;
}

export type DomainType = DomainSigType | DomainConjunctionType;

export interface DomainSubType {
  tag: "DomainSubType";
  sub: SigName;
  sup: SigName;
}

export interface DomainRel {
  tag: "DomainRel";
  belongsTo: SingleSigName;
  name: string;
  argTypes: SigName[];
}

export type DomainObject = DomainType | DomainSubType | DomainRel;
