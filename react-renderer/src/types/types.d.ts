/**
 * Output of label generation.
 */
interface LabelData {
  rendered: SVGSVGElement;
  width: number;
  height: number;
}

type Translation = ITrans<number>; // TODO: number type might be different
// type VaryMap<T = number> = [Path, T][];
type VaryMap<T = number> = Map<string, T>;

type FnDone<T> = IFnDone<T>;
interface IFnDone<T> {
  name: string;
  args: ArgVal<T>[];
  optType: OptType;
}

type Fn = IFn;
interface IFn {
  fname: string;
  fargs: Expr[];
  optType: OptType;
}
type OptType = "ObjFn" | "ConstrFn";

/**
 * The diagram state modeling the original Haskell types
 */
interface IState {
  varyingPaths: Path[];
  shapePaths: Path[];
  shapeProperties: any; // TODO: types
  uninitializedPaths: any; // TODO: types
  params: Params;
  objFns: Fn[];
  constrFns: Fn[];
  rng: any; // TODO: types
  selecterMatches: any; // TODO: types
  policyParams: any; // TODO: types
  oConfig: any; // TODO: types
  pendingPaths: Path[];
  varyingValues: number[];
  translation: Translation;
  shapeOrdering: string[];
  shapes: Shape[];
  varyingMap: VaryMap;
  overallObjective(...xs: Variable[]): Scalar;
}
type State = IState; // TODO

// TODO: annotate the comp graph with their derivatives
// NOTE: the point is to have a better type that allows annotation of the comp graph
// interface FieldExpr<T> {
//   tag: "FGPI" | "FExpr";
//   done: boolean;
//   value: ?;
//   contents: [string, { [k: string]: TagExpr<T> }] | TagExpr<T>;
// }
// TagExpr is either Value or Expr
interface CompNode<T> {
  status: "Done" | "Uninitialized" | "Pending";
  def: Expr | GPIExpr<T>;
  value: Value<T> | undefined;
}

type Properties = { [k: string]: Value<number> };

////////////////////////////////////////////////////////////////////////////

type Shape = IShape;

interface IShape {
  shapeType: string;
  properties: Properties;
}

type ArgVal<T> = IGPI<T> | IVal<T>;

interface IGPI<T> {
  tag: "GPI";
  contents: GPI<T>;
}

type GPI<T> = [string, { [k: string]: Value<T> }];

interface IVal<T> {
  tag: "Val";
  contents: Value<T>;
}

// type Translation<T> = ITrans<T>;

interface ITrans<T> {
  // TODO: compGraph
  trMap: { [k: string]: { [k: string]: FieldExpr<T> } };
  warnings: string[];
}

type FieldExpr<T> = IFExpr<T> | IFGPI<T>;

interface IFExpr<T> {
  tag: "FExpr";
  contents: TagExpr<T>;
}

interface IFGPI<T> {
  tag: "FGPI";
  contents: GPIExpr<T>;
}

type GPIExpr<T> = [string, { [k: string]: TagExpr<T> }];
type TagExpr<T> = IOptEval<T> | IDone<T> | IPending<T>;

interface IOptEval<T> {
  tag: "OptEval";
  contents: Expr;
}

interface IDone<T> {
  tag: "Done";
  contents: Value<T>;
}

interface IPending<T> {
  tag: "Pending";
  contents: Value<T>;
}

// interface Expr {
//   tag: string;
//   contents: any;
// }

// interface IIntLit extends Expr {
//   tag: "IntLit";
// }

type Expr =
  | IIntLit
  | IAFloat
  | IStringLit
  | IBoolLit
  | IEPath
  | ICompApp
  | IObjFn
  | IConstrFn
  | IAvoidFn
  | IBinOp
  | IUOp
  | IList
  | ITuple
  | IListAccess
  | ICtor
  | ILayering
  | IPluginAccess
  | IThenOp;

interface IIntLit {
  tag: "IntLit";
  contents: number;
}

interface IAFloat {
  tag: "AFloat";
  contents: AnnoFloat;
}

interface IStringLit {
  tag: "StringLit";
  contents: string;
}

interface IBoolLit {
  tag: "BoolLit";
  contents: boolean;
}

interface IEPath {
  tag: "EPath";
  contents: Path;
  // value
}

interface ICompApp {
  tag: "CompApp";
  contents: [string, Expr[]];
}

interface IObjFn {
  tag: "ObjFn";
  contents: [string, Expr[]];
}

interface IConstrFn {
  tag: "ConstrFn";
  contents: [string, Expr[]];
}

interface IAvoidFn {
  tag: "AvoidFn";
  contents: [string, Expr[]];
}

interface IBinOp {
  tag: "BinOp";
  contents: [BinaryOp, Expr, Expr];
}

interface IUOp {
  tag: "UOp";
  contents: [UnaryOp, Expr];
}

interface IList {
  tag: "List";
  contents: Expr[];
}

interface ITuple {
  tag: "Tuple";
  contents: [Expr, Expr];
}

interface IListAccess {
  tag: "ListAccess";
  contents: [Path, number];
}

interface ICtor {
  tag: "Ctor";
  contents: [string, PropertyDecl[]];
}

interface ILayering {
  tag: "Layering";
  contents: [Path, Path];
}

interface IPluginAccess {
  tag: "PluginAccess";
  contents: [string, Expr, Expr];
}

interface IThenOp {
  tag: "ThenOp";
  contents: [Expr, Expr];
}

type AnnoFloat = IFix | IVary;

interface IFix {
  tag: "Fix";
  contents: number;
}

interface IVary {
  tag: "Vary";
}

type BinaryOp = "BPlus" | "BMinus" | "Multiply" | "Divide" | "Exp";

type UnaryOp = "UPlus" | "UMinus";

type PropertyDecl = IPropertyDecl;

type IPropertyDecl = [string, Expr];

type Path = IFieldPath | IPropertyPath;

interface IFieldPath {
  tag: "FieldPath";
  contents: [BindingForm, string];
}

interface IPropertyPath {
  tag: "PropertyPath";
  contents: [BindingForm, string, string];
}

type Var = IVarConst;

type IVarConst = string;

type BindingForm = IBSubVar | IBStyVar;

interface IBSubVar {
  tag: "BSubVar";
  contents: Var;
}

interface IBStyVar {
  tag: "BStyVar";
  contents: StyVar;
}

type StyVar = IStyVar;

type IStyVar = string;

type Value<T> =
  | IFloatV<T>
  | IIntV<T>
  | IBoolV<T>
  | IStrV<T>
  | IPtV<T>
  | IPathDataV<T>
  | IPtListV<T>
  | IPaletteV<T>
  | IColorV<T>
  | IFileV<T>
  | IStyleV<T>
  | IListV<T>
  | ITupV<T>
  | ILListV<T>
  | IHMatrixV<T>
  | IPolygonV<T>;

interface IFloatV<T> {
  tag: "FloatV";
  contents: T;
}

interface IIntV<T> {
  tag: "IntV";
  contents: number;
}

interface IBoolV<T> {
  tag: "BoolV";
  contents: boolean;
}

interface IStrV<T> {
  tag: "StrV";
  contents: string;
}

interface IPtV<T> {
  tag: "PtV";
  contents: [T, T];
}

interface IPathDataV<T> {
  tag: "PathDataV";
  contents: SubPath<T>[];
}

interface IPtListV<T> {
  tag: "PtListV";
  contents: [T, T][];
}

interface IPaletteV<T> {
  tag: "PaletteV";
  contents: Color[];
}

interface IColorV<T> {
  tag: "ColorV";
  contents: Color;
}

interface IFileV<T> {
  tag: "FileV";
  contents: string;
}

interface IStyleV<T> {
  tag: "StyleV";
  contents: string;
}

interface IListV<T> {
  tag: "ListV";
  contents: T[];
}

interface ITupV<T> {
  tag: "TupV";
  contents: [T, T];
}

interface ILListV<T> {
  tag: "LListV";
  contents: T[][];
}

interface IHMatrixV<T> {
  tag: "HMatrixV";
  contents: HMatrix<T>;
}

interface IPolygonV<T> {
  tag: "PolygonV";
  contents: [[T, T][][], [T, T][][], [[T, T], [T, T]], [T, T][]];
}

type SubPath<T> = IClosed<T> | IOpen<T>;

interface IClosed<T> {
  tag: "Closed";
  contents: Elem<T>[];
}

interface IOpen<T> {
  tag: "Open";
  contents: Elem<T>[];
}

type HMatrix<T> = IHMatrix<T>;

interface IHMatrix<T> {
  xScale: T;
  xSkew: T;
  ySkew: T;
  yScale: T;
  dx: T;
  dy: T;
}

type Color = IRGBA | IHSVA;

interface IRGBA {
  tag: "RGBA";
  contents: [number, number, number, number];
}

interface IHSVA {
  tag: "HSVA";
  contents: [number, number, number, number];
}

type Elem<T> =
  | IPt<T>
  | ICubicBez<T>
  | ICubicBezJoin<T>
  | IQuadBez<T>
  | IQuadBezJoin<T>;

interface IPt<T> {
  tag: "Pt";
  contents: [T, T];
}

interface ICubicBez<T> {
  tag: "CubicBez";
  contents: [[T, T], [T, T], [T, T]];
}

interface ICubicBezJoin<T> {
  tag: "CubicBezJoin";
  contents: [[T, T], [T, T]];
}

interface IQuadBez<T> {
  tag: "QuadBez";
  contents: [[T, T], [T, T]];
}

interface IQuadBezJoin<T> {
  tag: "QuadBezJoin";
  contents: [T, T];
}

type OptStatus =
  | INewIter
  | IUnconstrainedRunning
  | IUnconstrainedConverged
  | IEPConverged;

interface INewIter {
  tag: "NewIter";
}

interface IUnconstrainedRunning {
  tag: "UnconstrainedRunning";
}

interface IUnconstrainedConverged {
  tag: "UnconstrainedConverged";
}

interface IEPConverged {
  tag: "EPConverged";
}

type BfgsParams = IBfgsParams;

interface IBfgsParams {
  lastState?: number[];
  lastGrad?: number[];
  invH?: number[][];
  s_list: number[][];
  y_list: number[][];
  numUnconstrSteps: number;
  memSize: number;
}

type Params = IParams;

interface IParams {
  optStatus: OptStatus;
  weight: number; // Constraint weight for exterior point method
  mutableUOstate: Variable[]; // Don't forget... it's mutable!

  // Info for unconstrained optimization
  UOround: number;
  lastUOstate: Tensor;
  lastUOenergy: Scalar;

  // Info for exterior point method
  EPround: number;
  lastEPstate: Tensor;
  lastEPenergy: Scalar;

  // For L-BFGS (TODO)
  bfgsInfo: BfgsParams;
}

// ------------ Types for reverse-mode autodiff

// ----- Helper types

interface Nothing<T> {
  tag: "Nothing";
}

interface Just<T> {
  tag: "Just";
  contents: T;
}

type MaybeVal<T> =
  | Nothing<T>
  | Just<T>;

// ----- Core types

//      s ("single output" node)
//     ... 
//     PARENT node (z) -- has refs to its parents
//      ^
//      | differential (dz/dv)
//      |
//     var (v)         -- has refs to its parents
// (carries gradVal: ds/dv)

// (var and node are used interchangeably)

interface IEdgeAD {
  node: IVarAD;
  differential: number; // Value "flowing down" from parent z (output, which is the node stored here) to child v (input), dz/dv
};

type EdgeAD = IEdgeAD;

interface IVarAD {
  val: number;
  parents: EdgeAD[]; // The resulting values from an expression. e.g. in `z := x + y`, `z` is a parent of `x` and of `y`
  gradVal: MaybeVal<number>;
}

type VarAD = IVarAD;
