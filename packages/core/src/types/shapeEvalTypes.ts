import { VarAD } from "./adTypes";
import { ASTNode, Identifier } from "./ASTTypes";
import { StyleError } from "./errors";
import { SubPath, Value } from "./shapeTypes";

/**
 * The input parameters to computations/objectives/constraints in Style. It can be either an entire shape (`IGPI`) or a value (`IVal`).
 */
export type ArgVal<T> = IGPI<T> | IVal<T>;

export interface IGPI<T> {
  tag: "GPI";
  contents: GPI<T>;
}

/**
 * A shape (Graphical Primitive Instance, aka GPI) in penrose has a type (_e.g._ `Circle`) and a set of properties (_e.g._ `center`). Each property this a value tagged with its type.
 */
export type GPI<T> = [string, { [k: string]: Value<T> }];

export interface IVal<T> {
  tag: "Val";
  contents: Value<T>;
}

export type Field = string;
export type Name = string;
export type Property = string;
export type FExpr = FieldExpr<VarAD>;
export type ShapeTypeStr = string;
export type PropID = string;
export type GPIMap = { [k: string]: TagExpr<VarAD> };
export type FieldDict = { [k: string]: FieldExpr<VarAD> };

export type StyleOptFn = [string, Expr[]]; // Objective or constraint

// NOTE: To make a deep clone, use `clone` from `rfdc`
/**
 * Translation represents the computational graph compiled from a trio of Penrose programs.
 */
export type Translation = ITrans<VarAD>;

export type Trans = TrMap<VarAD>;

export type TrMap<T> = { [k: string]: { [k: string]: FieldExpr<T> } };

export interface ITrans<T> {
  // TODO: compGraph
  trMap: TrMap<T>;
  warnings: StyleError[];
}

export type FieldExpr<T> = IFExpr<T> | IFGPI<T>;

export interface IFExpr<T> {
  tag: "FExpr";
  contents: TagExpr<T>;
}

export interface IFGPI<T> {
  tag: "FGPI";
  contents: GPIExpr<T>;
}

export type GPIProps<T> = { [k: string]: TagExpr<T> };

export type GPIExpr<T> = [string, { [k: string]: TagExpr<T> }];
export type TagExpr<T> = IOptEval<T> | IDone<T> | IPending<T>;

export interface IOptEval<T> {
  tag: "OptEval";
  contents: Expr;
}

export interface IDone<T> {
  tag: "Done";
  contents: Value<T>;
}

export interface IPending<T> {
  tag: "Pending";
  contents: Value<T>;
}

export type Expr =
  // | IIntLit
  | AnnoFloat
  | IStringLit
  | IBoolLit
  | Path // NOTE: changed from EPath
  | ICompApp
  | IObjFn
  | IConstrFn
  // | IAvoidFn // TODO: unimplemented
  | IBinOp
  | IUOp
  | IList
  | ITuple
  | IVector
  | IMatrix
  | IVectorAccess
  | IMatrixAccess
  | IListAccess
  | GPIDecl
  | ILayering
  | IPluginAccess;
// | IThenOp; // TODO: deprecated transformation exprs

export interface IIntLit {
  tag: "IntLit";
  contents: number;
}

// interface IAFloat {
//     tag: "AFloat";
//     contents: AnnoFloat;
// }

export interface IStringLit extends ASTNode {
  tag: "StringLit";
  contents: string;
}

export interface IBoolLit extends ASTNode {
  tag: "BoolLit";
  contents: boolean;
}

export interface ICompApp extends ASTNode {
  tag: "CompApp";
  name: Identifier;
  args: Expr[];
}

export interface IObjFn extends ASTNode {
  tag: "ObjFn";
  name: Identifier;
  args: Expr[];
}

export interface IConstrFn extends ASTNode {
  tag: "ConstrFn";
  name: Identifier;
  args: Expr[];
}

export interface IAvoidFn {
  tag: "AvoidFn";
  contents: [string, Expr[]];
}

export type BinaryOp = "BPlus" | "BMinus" | "Multiply" | "Divide" | "Exp";

// NOTE: unary + operator not parsed, as they don't change values
export type UnaryOp = "UMinus";
export interface IBinOp extends ASTNode {
  tag: "BinOp";
  op: BinaryOp;
  left: Expr;
  right: Expr;
}
export interface IUOp extends ASTNode {
  tag: "UOp";
  op: UnaryOp;
  arg: Expr;
}

export interface IList extends ASTNode {
  tag: "List";
  contents: Expr[];
}

export interface ITuple extends ASTNode {
  tag: "Tuple";
  contents: [Expr, Expr];
}

export interface IVector extends ASTNode {
  tag: "Vector";
  contents: Expr[];
}

export interface IMatrix extends ASTNode {
  tag: "Matrix";
  contents: Expr[];
}

export interface IVectorAccess extends ASTNode {
  tag: "VectorAccess";
  contents: [Path, Expr];
}

export interface IMatrixAccess extends ASTNode {
  tag: "MatrixAccess";
  contents: [Path, Expr[]];
}

export interface IListAccess extends ASTNode {
  tag: "ListAccess";
  contents: [Path, number];
}

export interface GPIDecl extends ASTNode {
  tag: "GPIDecl";
  shapeName: Identifier;
  properties: PropertyDecl[];
}

export interface ILayering extends ASTNode {
  tag: "Layering";
  below: Path;
  above: Path;
}

export interface IPluginAccess extends ASTNode {
  tag: "PluginAccess";
  contents: [string, Expr, Expr];
}

export interface IThenOp {
  tag: "ThenOp";
  contents: [Expr, Expr];
}

export type AnnoFloat = IFix | IVary | IVaryAD;

export interface IFix extends ASTNode {
  tag: "Fix";
  contents: number;
}

export interface IVary extends ASTNode {
  tag: "Vary";
}

export interface IVaryAD extends ASTNode {
  tag: "VaryAD";
  contents: VarAD;
}

export interface PropertyDecl extends ASTNode {
  tag: "PropertyDecl";
  name: Identifier;
  value: Expr;
}

// TODO: check how the evaluator/compiler should interact with ASTNode
export type Path =
  | IFieldPath
  | IPropertyPath
  | IAccessPath
  | LocalVar
  | IInternalLocalVar;
// LocalVar is only used internally by the compiler
// Unused
// | ITypePropertyPath;

export interface IFieldPath extends ASTNode {
  tag: "FieldPath";
  name: BindingForm;
  field: Identifier;
}

export interface IPropertyPath extends ASTNode {
  tag: "PropertyPath";
  name: BindingForm;
  field: Identifier;
  property: Identifier;
}

export interface IAccessPath extends ASTNode {
  tag: "AccessPath";
  path: Path;
  indices: Expr[];
}

// COMBAK: This is named inconsistently since the parser calls it `LocalVar`, should be ILocalVar
export interface LocalVar extends ASTNode {
  tag: "LocalVar";
  contents: Identifier;
}

export interface IInternalLocalVar extends ASTNode {
  // Note, better to not extend ASTNode as it's only used internally by compiler, but breaks parser otherwise
  tag: "InternalLocalVar";
  contents: string;
}

// Unused
// interface ITypePropertyPath {
//   tag: "TypePropertyPath";
// }

export type BindingForm = SubVar | StyVar;

export interface SubVar extends ASTNode {
  tag: "SubVar";
  contents: Identifier;
}

export interface StyVar extends ASTNode {
  tag: "StyVar";
  contents: Identifier;
}

/** A floating point number **/
export interface IFloatV<T> {
  tag: "FloatV";
  contents: T;
}

/** An integer **/
export interface IIntV {
  tag: "IntV";
  contents: number;
}

/** A boolean expression (`True` or `False`) **/
export interface IBoolV<T> {
  tag: "BoolV";
  contents: boolean;
}

/** A string literal **/
export interface IStrV<T> {
  tag: "StrV";
  contents: string;
}

/** A point in 2D **/
export interface IPtV<T> {
  tag: "PtV";
  contents: [T, T];
}

/** A path, similar to an [SVG path](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) **/
export interface IPathDataV<T> {
  tag: "PathDataV";
  contents: SubPath<T>[];
}

/** A list of points **/
export interface IPtListV<T> {
  tag: "PtListV";
  contents: [T, T][];
}

/** A list of colors **/
export interface IPaletteV<T> {
  tag: "PaletteV";
  contents: Color<T>[];
}

/** A color encoded in RGB or HSV **/
export interface IColorV<T> {
  tag: "ColorV";
  contents: Color<T>;
}

/** A path to a file **/
export interface IFileV<T> {
  tag: "FileV";
  contents: string;
}

/** A string literal for shape styling (e.g. `dotted`) **/
export interface IStyleV<T> {
  tag: "StyleV";
  contents: string;
}

/** A list **/
export interface IListV<T> {
  tag: "ListV";
  contents: T[];
}

/** A vector of floating-point numbers **/
export interface IVectorV<T> {
  tag: "VectorV";
  contents: T[];
}

/** A 2D matrix **/
export interface IMatrixV<T> {
  tag: "MatrixV";
  contents: T[][];
}

/** A 2-tuple **/
export interface ITupV<T> {
  tag: "TupV";
  contents: [T, T];
}

/** A list of lists **/
export interface ILListV<T> {
  tag: "LListV";
  contents: T[][];
}

/**
 * @deprecated
 */
export interface IHMatrixV<T> {
  tag: "HMatrixV";
  contents: HMatrix<T>;
}

/**
 * @deprecated
 */
export interface IPolygonV<T> {
  tag: "PolygonV";
  contents: [[T, T][][], [T, T][][], [[T, T], [T, T]], [T, T][]];
}

export type HMatrix<T> = IHMatrix<T>;

export interface IHMatrix<T> {
  xScale: T;
  xSkew: T;
  ySkew: T;
  yScale: T;
  dx: T;
  dy: T;
}

export type Color<T> = IRGBA<T> | IHSVA<T>;

export interface IRGBA<T> {
  tag: "RGBA";
  contents: [T, T, T, T];
}

export interface IHSVA<T> {
  tag: "HSVA";
  contents: [T, T, T, T];
}

export interface IPt<T> {
  tag: "Pt";
  contents: [T, T];
}

export interface ICubicBez<T> {
  tag: "CubicBez";
  contents: [[T, T], [T, T], [T, T]];
}

export interface ICubicBezJoin<T> {
  tag: "CubicBezJoin";
  contents: [[T, T], [T, T]];
}

export interface IQuadBez<T> {
  tag: "QuadBez";
  contents: [[T, T], [T, T]];
}

export interface IQuadBezJoin<T> {
  tag: "QuadBezJoin";
  contents: [T, T];
}
