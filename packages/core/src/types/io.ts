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
  variation: string;
  name: string;
  showInIDE: boolean;
}

/**
 * Schema for the registry of working examples
 */
export interface Registry {
  root: string;
  substances: { [subID: string]: { name: string; URI: string } };
  styles: { [styID: string]: { name: string; URI: string } };
  domains: { [domID: string]: { name: string; URI: string } };
  trios: {
    substance: string;
    style: string;
    domain: string;
    variation: string;
    meta?: string;
    showInIDE: boolean;
  }[];
}
