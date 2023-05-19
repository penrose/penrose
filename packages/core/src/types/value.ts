import { Shape as RealShape, Shape } from "../shapes/Shapes";
import { SourceRange } from "./ast";

/**
 * The input parameters to computations/objectives/constraints in Style.
 * It can be either an entire shape (wrapped in `ShapeVal`)
 * or a value (wrapped in `Val`).
 */
export type ArgVal<T> = ShapeVal<T> | Val<T>;

export interface Val<T> {
  tag: "Val";
  contents: Value<T>;
}

export interface ShapeVal<T> {
  tag: "ShapeVal";
  contents: RealShape<T>;
}

export type ArgValWithSourceLoc<T> = ArgVal<T> & SourceRange;

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
  | ShapeListV<T>
  | ClipDataV<T>;

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
  tag: "ShapeListV";
  contents: RealShape<T>[];
}

export interface ClipDataV<T> {
  tag: "ClipDataV";
  contents: ClipData<T>;
}

export type ClipData<T> = NoClip | Clip<T>;

export interface NoClip {
  tag: "NoClip";
}

export interface Clip<T> {
  tag: "Clip";
  contents: Shape<T>;
}
