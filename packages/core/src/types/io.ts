/**
 * Data structure for information about a single program
 */
interface TrioProgram {
  name: string;
  path: string;
}
/**
 * Data structure for information of a program trio
 * See also `trio-schema.json`
 */
export interface Trio {
  substance: TrioProgram;
  style: TrioProgram;
  domain: TrioProgram;
  variation: string;
  name: string;
}

/**
 * Schema for the registry of working example trios
 */
export interface Registry {
  /**
   * Root URI to concatenate onto paths
   */
  root: string;
  trios: string[];
}
