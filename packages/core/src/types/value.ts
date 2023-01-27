/**
 * The input parameters to computations/objectives/constraints in Style. It can be either an entire shape (`GPI`) or a value (`Val`).
 */
export type ArgVal<T> = GPI<T> | Val<T>;

export interface GPI<T> {
  tag: "GPI";

  /**
   * A shape (Graphical Primitive Instance, aka GPI) in penrose has a type (_e.g._ `Circle`) and a set of properties (_e.g._ `center`). Each property this a value tagged with its type.
   */
  contents: [string, { [k: string]: Value<T> }];
}

export interface Val<T> {
  tag: "Val";
  contents: Value<T>;
}

export type Field = string;
export type Name = string;
export type Property = string;
export type ShapeTypeStr = string;
export type PropID = string;

/**
 * A value in the penrose system.
 */
export type Value<T> =
  | FloatV<T>
  | BoolV
  | StrV
  | PathDataV<T>
  | PtListV<T>
  | ColorV<T>
  | ListV<T>
  | VectorV<T>
  | MatrixV<T>
  | TupV<T>
  | LListV<T>
  | ShapeListV<T>;

/** A floating point number **/
export interface FloatV<T> {
  tag: "FloatV";
  contents: T;
}

/** A boolean expression (`True` or `False`) **/
export interface BoolV {
  tag: "BoolV";
  contents: boolean;
}

/** A string literal **/
export interface StrV {
  tag: "StrV";
  contents: string;
}

/** A path, similar to an [SVG path](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) **/
export interface PathDataV<T> {
  tag: "PathDataV";
  contents: PathCmd<T>[];
}

/** A list of points **/
export interface PtListV<T> {
  tag: "PtListV";
  contents: T[][];
}

/** A color encoded in RGB or HSV **/
export interface ColorV<T> {
  tag: "ColorV";
  contents: Color<T>;
}

/** A list **/
export interface ListV<T> {
  tag: "ListV";
  contents: T[];
}

/** A vector of floating-point numbers **/
export interface VectorV<T> {
  tag: "VectorV";
  contents: T[];
}

/** A 2D matrix **/
export interface MatrixV<T> {
  tag: "MatrixV";
  contents: T[][];
}

/** A 2-tuple **/
export interface TupV<T> {
  tag: "TupV";
  contents: T[];
}

/** A list of lists **/
export interface LListV<T> {
  tag: "LListV";
  contents: T[][];
}

export type Color<T> = RGBA<T> | HSVA<T> | NoPaint;

export interface RGBA<T> {
  tag: "RGBA";
  contents: T[];
}

export interface HSVA<T> {
  tag: "HSVA";
  contents: T[];
}

export interface NoPaint {
  tag: "NONE";
}

// SVG spec types
export type SubPath<T> = ValueV<T> | CoordV<T>;

export interface PathCmd<T> {
  cmd: string;
  contents: SubPath<T>[];
}

export interface ValueV<T> {
  tag: "ValueV";
  contents: T[];
}

export interface CoordV<T> {
  tag: "CoordV";
  contents: T[];
}

export interface ShapeListV<T> {
  tag: "ShapeList";
  contents: GPI<T>[];
}
