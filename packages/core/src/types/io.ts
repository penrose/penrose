/**
 * Data structure for information of a program trio
 */
export interface Trio {
  gallery: boolean;
  id: string;
  substanceURI: string;
  styleURI: string;
  domainURI: string;
  substanceName: string;
  styleName: string;
  domainName: string;
  variation: string;
  name: string;
}

/**
 * Schema for the registry of working examples
 * See also: @penrose/examples/src/registry-schema.json
 */
export interface Registry {
  root: string;
  substances: { [subID: string]: { name: string; URI: string } };
  styles: { [styID: string]: { name: string; URI: string } };
  domains: { [domID: string]: { name: string; URI: string } };
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
