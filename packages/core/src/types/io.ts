/**
 * Data structure for information of a program trio
 */
export interface Trio {
  substanceURI: string;
  styleURI: string;
  domainURI: string;
  substanceName: string;
  styleName: string;
  domainName: string;
  name: string;
}

/**
 * Schema for the registry of working examples
 */
export interface Registry {
  substances: { name: string; URI: string };
  styles: { name: string; URI: string };
  domains: { name: string; URI: string };
  trios: { substance: string; style: string; domain: string }[];
}
