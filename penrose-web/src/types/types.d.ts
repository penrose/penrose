/**
 * Output of label generation.
 */
interface LabelData {
  rendered: SVGSVGElement;
  width: number;
  height: number;
}

type VaryMap<T = VarAD> = Map<string, T>;

type FnDone<T> = IFnDone<T>;
interface IFnDone<T> {
  name: string;
  args: ArgVal<T>[];
  optType: OptType;
}

type Fn = IFn;

/**
 * Generic interface for constraint or objective functions
 */
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
  originalTranslation: Translation;
  shapeOrdering: string[];
  shapes: Shape[];
  varyingMap: VaryMap;
}
type State = IState;

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

// These are numbers, not autodiff types, since shapes are only for display (unlike GPIs)
type Properties = { [k: string]: Value<number> };

////////////////////////////////////////////////////////////////////////////

type Shape = IShape;

/**
 * A shape (Graphical Primitive Instance, aka GPI) in penrose has a type (_e.g._ `Circle`) and a set of properties (_e.g._ `center`). This type is specifically used for rendering. See {@link GPI} for the version used for the runtime.
 */
interface IShape {
  shapeType: string;
  properties: Properties;
}

/**
 * The input parameters to computations/objectives/constraints in Style. It can be either an entire shape (`IGPI`) or a value (`IVal`).
 */
type ArgVal<T> = IGPI<T> | IVal<T>;

interface IGPI<T> {
  tag: "GPI";
  contents: GPI<T>;
}

/**
 * A shape (Graphical Primitive Instance, aka GPI) in penrose has a type (_e.g._ `Circle`) and a set of properties (_e.g._ `center`). Each property this a value tagged with its type.
 */
type GPI<T> = [string, { [k: string]: Value<T> }];

interface IVal<T> {
  tag: "Val";
  contents: Value<T>;
}

// NOTE: To make a deep clone, use `clone` from `rfdc`
/**
 * Translation represents the computational graph compiled from a trio of Penrose programs.
 */
type Translation = ITrans<VarAD>;

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

type Expr =
  | IIntLit
  | IAFloat
  | IStringLit
  | IBoolLit
  | IEvar
  | IEPath
  | ICompApp
  | IObjFn
  | IConstrFn
  | IAvoidFn
  | IBinOp
  | IUOp
  | IList
  | ITuple
  | IVector
  | IMatrix
  | IVectorAccess
  | IMatrixAccess
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

interface IEVar {
  tag: "EVar";
  contents: LocalVar;
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

interface IVector {
  tag: "Vector";
  contents: Expr[];
}

interface IMatrix {
  tag: "Matrix";
  contents: Expr[];
}

interface IVectorAccess {
  tag: "VectorAccess";
  contents: [Path, Expr];
}

interface IMatrixAccess {
  tag: "MatrixAccess";
  contents: [Path, Expr[]];
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

type Path = IFieldPath | IPropertyPath | IAccessPath;
// Unused
// | ITypePropertyPath;

interface IFieldPath {
  tag: "FieldPath";
  contents: [BindingForm, string];
}

interface IPropertyPath {
  tag: "PropertyPath";
  contents: [BindingForm, string, string];
}

interface IAccessPath {
  tag: "AccessPath";
  contents: [Path, number[]];
}

// Unused
// interface ITypePropertyPath {
//   tag: "TypePropertyPath";
// }

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

type LocalVar = ILocalVar;

type ILocalVar = string;

/**
 * A value in the penrose system.
 */
type Value<T> =
  | IFloatV<T>
  | IIntV
  | IBoolV<T>
  | IStrV<T>
  | IPtV<T>
  | IPathDataV<T>
  | IPtListV<T>
  | IColorV<T>
  | IPaletteV<T>
  | IFileV<T>
  | IStyleV<T>
  | IListV<T>
  | IVectorV<T>
  | IMatrixV<T>
  | ITupV<T>
  | ILListV<T>
  | IHMatrixV<T>
  | IPolygonV<T>;

/** A floating point number **/
interface IFloatV<T> {
  tag: "FloatV";
  contents: T;
}

/** An integer **/
interface IIntV {
  tag: "IntV";
  contents: number;
}

/** A boolean expression (`True` or `False`) **/
interface IBoolV<T> {
  tag: "BoolV";
  contents: boolean;
}

/** A string literal **/
interface IStrV<T> {
  tag: "StrV";
  contents: string;
}

/** A point in 2D **/
interface IPtV<T> {
  tag: "PtV";
  contents: [T, T];
}

/** A path, similar to an [SVG path](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) **/
interface IPathDataV<T> {
  tag: "PathDataV";
  contents: SubPath<T>[];
}

/** A list of points **/
interface IPtListV<T> {
  tag: "PtListV";
  contents: [T, T][];
}

/** A list of colors **/
interface IPaletteV<T> {
  tag: "PaletteV";
  contents: Color<T>[];
}

/** A color encoded in RGB or HSV **/
interface IColorV<T> {
  tag: "ColorV";
  contents: Color<T>;
}

/** A path to a file **/
interface IFileV<T> {
  tag: "FileV";
  contents: string;
}

/** A string literal for shape styling (e.g. `dotted`) **/
interface IStyleV<T> {
  tag: "StyleV";
  contents: string;
}

/** A list **/
interface IListV<T> {
  tag: "ListV";
  contents: T[];
}

/** A vector of floating-point numbers **/
interface IVectorV<T> {
  tag: "VectorV";
  contents: T[];
}

/** A 2D matrix **/
interface IMatrixV<T> {
  tag: "MatrixV";
  contents: T[][];
}

/** A 2-tuple **/
interface ITupV<T> {
  tag: "TupV";
  contents: [T, T];
}

/** A list of lists **/
interface ILListV<T> {
  tag: "LListV";
  contents: T[][];
}

/**
 * @deprecated
 */
interface IHMatrixV<T> {
  tag: "HMatrixV";
  contents: HMatrix<T>;
}

/**
 * @deprecated
 */
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

type Color<T> = IRGBA<T> | IHSVA<T>;

interface IRGBA<T> {
  tag: "RGBA";
  contents: [T, T, T, T];
}

interface IHSVA<T> {
  tag: "HSVA";
  contents: [T, T, T, T];
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

type LbfgsParams = ILbfgsParams;

// `n` is the size of the varying state
interface ILbfgsParams {
  // TODO: Store as matrix types
  lastState: Maybe<any>; // nx1 (col vec)
  lastGrad: Maybe<any>; // nx1 (col vec)
  // invH: Maybe<any>; // nxn matrix
  s_list: any[]; // list of nx1 col vecs
  y_list: any[]; // list of nx1 col vecs
  numUnconstrSteps: number;
  memSize: number;
}

type Params = IParams;

interface IParams {
  optStatus: OptStatus;
  /** Constraint weight for exterior point method **/
  weight: number;
  /** Info for unconstrained optimization **/
  UOround: number;
  lastUOstate: number[];
  lastUOenergy: number;
  /** Info for exterior point method **/
  EPround: number;
  lastEPstate: number[];
  lastEPenergy: number;

  lastGradient: number[]; // Value of gradient evaluated at the last state
  lastGradientPreconditioned: number[]; // Value of gradient evaluated at the last state, preconditioned by LBFGS
  // ^ Those two are stored to make them available to Style later

  // For L-BFGS
  lbfgsInfo: LbfgsParams;

  xsVars: VarAD[]; // Computational graph (leaf vars), from the bottom up

  // For energy/gradient compilation
  graphs: GradGraphs;

  functionsCompiled: boolean;

  // Higher-order functions (not yet applied with hyperparameters, in this case, just the EP weight)
  objective: any; // number -> (number[] -> number)
  gradient: any; // number -> (number[] -> number[])

  // Applied with weight (or hyperparameters in general) -- may change with the EP round
  currObjective(xs: number[]): number;
  currGradient(xs: number[]): number[];

  // `xsVars` are all the leaves of the energy graph
  energyGraph: VarAD; // This is the top of the energy graph (parent node)
  constrWeightNode: VarAD; // Handle to node for constraint weight (so it can be set as the weight changes)
  epWeightNode: VarAD; // similar to constrWeightNode
}

type WeightInfo = IWeightInfo;

interface IWeightInfo {
  constrWeightNode: VarAD; // Constant
  epWeightNode: VarAD; // Changes (input in optimization, but we do NOT need the gradient WRT it)
  constrWeight: number;
  epWeight: number;
}

// ----- Helper types

interface Nothing<T> {
  tag: "Nothing";
}

interface Just<T> {
  tag: "Just";
  contents: T;
}

type MaybeVal<T> = Nothing<T> | Just<T>;

// ------------ Types for reverse-mode autodiff

// ----- Core types

//      s ("single output" node)
//     ...
//     PARENT node (z) -- has refs to its parents
//      ^
//      | sensitivity (dz/dv)
//      |
//     var (v)         -- has refs to its parents
// (carries gradVal: ds/dv)

// (var and node are used interchangeably)

interface IEdgeAD {
  node: VarAD;

  // Function "flowing down" from parent z (output, which is the node stored here) to child v (input), dz/dv
  // Aka how sensitive the output is to this input -- a function encoded as a computational graph fragment
  sensitivityNode: MaybeVal<VarAD>;
}

type EdgeAD = IEdgeAD;

interface IVarAD {
  val: number; // The value of this node at the time the computational graph was created. This is mostly unused, since most values are compiled out except for leaf nodes

  valDone: boolean; // formerly used to cache energy values in the computational graph in evalEnergyOnGraph; TODO: can be removed if evalEnergyOnGraph is removed

  tag: "custom";
  metadata: string; // Used for storing the kind of weight
  op: string;
  isCompNode: boolean; // comp node (normal computational graph) or grad node (node in computational graph for gradient)
  isInput: boolean; // These inputs need to be distinguished as bindings in the function (e.g. \x y -> x + y)
  parents: EdgeAD[]; // The resulting values from an expression. e.g. in `z := x + y`, `z` is a parent of `x` and of `y`
  children: EdgeAD[];
  parentsGrad: EdgeAD[]; // The resulting values from an expression. e.g. in `z := x + y`, `z` is a parent of `x` and of `y`
  childrenGrad: EdgeAD[];
  gradVal: MaybeVal<number>;
  gradNode: MaybeVal<VarAD>;
  index: number; // -1 if not a leaf node, 0-n for leaf nodes (order in the leaf node list) so we know how to pass in the floats

  debug: boolean; // If true, this prints node debug info on evaluation
  debugInfo: string;

  nodeVisited: boolean;
  // Now used to track whether this node (and its children) has already been computed in the codegen
  name: string; // Name of cached value for this node in codegen (e.g. `const x3 = x1 + x2;` <-- name of node is `x3`)
}

type VarAD = IVarAD;

// ----- Types for generalizing our system autodiff

type VecAD = VarAD[];

type Pt2 = [VarAD, VarAD];

type GradGraphs = IGradGraphs;

interface IGradGraphs {
  inputs: VarAD[];
  energyOutput: VarAD;
  // The energy inputs may be different from the grad inputs bc the former may contain the EP weight (but for the latter, we do not want the derivative WRT the EP weight)
  gradOutputs: VarAD[];
  weight: MaybeVal<VarAD>; // EP weight, a hyperparameter to both energy and gradient; TODO: generalize to multiple hyperparameters
}

type OptInfo = IOptInfo;
// Returned after a call to `minimize`

interface IOptInfo {
  xs: number[];
  energyVal: number;
  normGrad: number;
  newLbfgsInfo: LbfgsParams;
  gradient: number[];
  gradientPreconditioned: number[];
}

type OptDebugInfo = IOptDebugInfo;

type NumMap = Map<string, number>;

interface IOptDebugInfo {
  gradient: NumMap;
  gradientPreconditioned: NumMap;
}
