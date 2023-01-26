/**
 * Data structure for information of a program trio
 */
export interface Trio {
  gallery: boolean;
  id: string;
  substanceURI: string;
  styleURI: string;
  domainURI: string;
  substanceID: string;
  styleID: string;
  domainID: string;
  variation: string;
  name: string;
}

/**
 * Schema for the registry of working examples
 * See also: @penrose/examples/src/registry-schema.json
 */
export interface Registry {
  root: string;
  substances: { [subID: string]: { URI: string; domain: string } };
  styles: { [styID: string]: { URI: string; domain: string } };
  domains: { [domID: string]: { URI: string } };
  trios: {
    name?: string;
    substance: string;
    style: string;
    domain: string;
    variation: string;
    meta?: string;
    gallery?: boolean;
  }[];
}
