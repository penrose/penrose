export type DomainCache = {
  typeNames: string[];
  predNames: string[];
  fnNames: string[];
  consNames: string[];
};

export type SubstanceCache = {
  varNames: string[];
};

export type ShapeProperties = {
  [key: string]: string;
};

export type ShapeDefinitions = {
  [shapeName: string]: ShapeProperties;
};
