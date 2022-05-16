import * as ad from "./ad";
import { A } from "./ast";
import { StyleError } from "./errors";
import { Expr } from "./style";

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
export type GPIMap = { [k: string]: TagExpr<ad.Num> };
export type FieldDict = { [k: string]: FieldExpr<ad.Num> };

export type StyleOptFn = [string, Expr<A>[]]; // Objective or constraint

// NOTE: To make a deep clone, use `clone` from `rfdc`
/**
 * Translation represents the computational graph compiled from a trio of Penrose programs.
 */
export type Translation = Trans<ad.Num>;

export type TrMap<T> = { [k: string]: { [k: string]: FieldExpr<T> } };

export interface Trans<T> {
  // TODO: compGraph
  trMap: TrMap<T>;
  warnings: StyleError[];
}

export type FieldExpr<T> = FExpr<T> | FGPI<T>;

export interface FExpr<T> {
  tag: "FExpr";
  contents: TagExpr<T>;
}

export interface FGPI<T> {
  tag: "FGPI";
  contents: GPIExpr<T>;
}

export type GPIProps<T> = { [k: string]: TagExpr<T> };

export type GPIExpr<T> = [string, { [k: string]: TagExpr<T> }];
export type TagExpr<T> = OptEval<T> | Done<T> | Pending<T>;

export interface OptEval<T> {
  tag: "OptEval";
  contents: Expr<A>;
}

export interface Done<T> {
  tag: "Done";
  contents: Value<T>;
}

export interface Pending<T> {
  tag: "Pending";
  contents: Value<T>;
}

/**
 * A value in the penrose system.
 */
export type Value<T> =
  | FloatV<T>
  | IntV
  | BoolV<T>
  | StrV
  | PtV<T>
  | PathDataV<T>
  | PtListV<T>
  | ColorV<T>
  | PaletteV<T>
  | FileV<T>
  | StyleV<T>
  | ListV<T>
  | VectorV<T>
  | MatrixV<T>
  | TupV<T>
  | LListV<T>
  | HMatrixV<T>;

// Unused
// interface TypePropertyPath {
//   tag: "TypePropertyPath";
// }

/** A floating point number **/
export interface FloatV<T> {
  tag: "FloatV";
  contents: T;
}

/** An integer **/
export interface IntV {
  tag: "IntV";
  contents: number;
}

/** A boolean expression (`True` or `False`) **/
export interface BoolV<T> {
  tag: "BoolV";
  contents: boolean;
}

/** A string literal **/
export interface StrV {
  tag: "StrV";
  contents: string;
}

/** A point in 2D **/
export interface PtV<T> {
  tag: "PtV";
  contents: T[];
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

/** A list of colors **/
export interface PaletteV<T> {
  tag: "PaletteV";
  contents: Color<T>[];
}

/** A color encoded in RGB or HSV **/
export interface ColorV<T> {
  tag: "ColorV";
  contents: Color<T>;
}

/** A path to a file **/
export interface FileV<T> {
  tag: "FileV";
  contents: string;
}

/** A string literal for shape styling (e.g. `dotted`) **/
export interface StyleV<T> {
  tag: "StyleV";
  contents: string;
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

/**
 * @deprecated
 */
export interface HMatrixV<T> {
  tag: "HMatrixV";
  contents: HMatrix<T>;
}

export interface HMatrix<T> {
  xScale: T;
  xSkew: T;
  ySkew: T;
  yScale: T;
  dx: T;
  dy: T;
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
