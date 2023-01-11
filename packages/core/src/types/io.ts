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
  /**
   * Preview image URL
   */
  image?: string;
  /**
   * Provides location of trio file so that relative paths can be resolved
   */
  path?: string;
}

/**
 * Points to a trio file that resolves to type `Trio`
 */
export interface TrioFilePointer {
  path: string;
}

/**
 * Schema for the registry of working example trios
 */
export interface Registry {
  /**
   * Root URI to concatenate onto paths
   */
  root: string;
  trios: (TrioFilePointer | Trio)[];
}
