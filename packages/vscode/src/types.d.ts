//#region Shape/Evaluator-related types

type Shape = IShape;

type Properties = { [k: string]: Value<number> };

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
  // | IIntLit
  | AnnoFloat
  | IStringLit
  | IBoolLit
  | Path // NOTE: changed from EPath
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
  | GPIDecl
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

interface IStringLit extends ASTNode {
  tag: "StringLit";
  contents: string;
}

interface IBoolLit extends ASTNode {
  tag: "BoolLit";
  contents: boolean;
}

interface IEVar {
  tag: "EVar";
  contents: LocalVar;
}

// NOTE: no longer using EPath, and use Path instead in Expr
// interface IEPath {
//   tag: "EPath";
//   contents: Path;
//   // value
// }

interface ICompApp extends ASTNode {
  tag: "CompApp";
  name: Identifier;
  args: Expr[];
}

interface IObjFn extends ASTNode {
  tag: "ObjFn";
  name: Identifier;
  args: Expr[];
}

interface IConstrFn extends ASTNode {
  tag: "ConstrFn";
  name: Identifier;
  args: Expr[];
}

interface IAvoidFn {
  tag: "AvoidFn";
  contents: [string, Expr[]];
}

type BinaryOp = "BPlus" | "BMinus" | "Multiply" | "Divide" | "Exp";

// NOTE: unary + operator not parsed, as they don't change values
type UnaryOp = "UMinus";
interface IBinOp extends ASTNode {
  tag: "BinOp";
  op: BinaryOp;
  left: Expr;
  right: Expr;
}
interface IUOp extends ASTNode {
  tag: "UOp";
  op: UnaryOp;
  arg: Expr;
}

interface IList extends ASTNode {
  tag: "List";
  contents: Expr[];
}

interface ITuple extends ASTNode {
  tag: "Tuple";
  contents: [Expr, Expr];
}

interface IVector extends ASTNode {
  tag: "Vector";
  contents: Expr[];
}

interface IMatrix extends ASTNode {
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

interface GPIDecl extends ASTNode {
  tag: "GPIDecl";
  shapeName: Identifier;
  properties: PropertyDecl[];
}

interface ILayering extends ASTNode {
  tag: "Layering";
  below: Path;
  above: Path;
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

interface IFix extends ASTNode {
  tag: "Fix";
  contents: number;
}

interface IVary extends ASTNode {
  tag: "Vary";
}

interface PropertyDecl extends ASTNode {
  tag: "PropertyDecl";
  name: Identifier;
  value: Expr;
}

// TODO: check how the evaluator/compiler should interact with ASTNode
type Path = IFieldPath | IPropertyPath | IAccessPath;
// Unused
// | ITypePropertyPath;

interface IFieldPath extends ASTNode {
  tag: "FieldPath";
  name: BindingForm;
  field: Identifier;
}

interface IPropertyPath extends ASTNode {
  tag: "PropertyPath";
  name: BindingForm;
  field: Identifier;
  property: Identifier;
}

interface IAccessPath extends ASTNode {
  tag: "AccessPath";
  path: Path;
  indices: number[];
}

// Unused
// interface ITypePropertyPath {
//   tag: "TypePropertyPath";
// }

type BindingForm = SubVar | StyVar;

interface SubVar extends ASTNode {
  tag: "SubVar";
  contents: Identifier;
}

interface StyVar extends ASTNode {
  tag: "StyVar";
  contents: Identifier;
}

interface LocalVar extends ASTNode {
  tag: "LocalVar";
  contents: Identifier;
}

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
//#endregion

//#region State-related types

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

// TODO: use strings for status
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

//#endregion

//#region Helper types

interface Nothing<T> {
  tag: "Nothing";
}

interface Just<T> {
  tag: "Just";
  contents: T;
}

type MaybeVal<T> = Nothing<T> | Just<T>;
//#endregion

//#region Types for reverse-mode autodiff

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
//#endregion

//#region Types for generalizing our system autodiff

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

//#endregion

//#region Domain AST

interface DomainProg extends ASTNode {
  tag: "DomainProg";
  statements: DomainStmt[];
}

interface TypeConstructor {
  tag: "TypeConstructor";
  name: Identifier;
  args: Type[];
}

type DomainStmt =
  | TypeDecl
  | PredicateDecl
  | FunctionDecl
  | ConstructorDecl
  | PreludeDecl
  | NotationDecl
  | SubTypeDecl;

interface TypeDecl extends ASTNode {
  tag: "TypeDecl";
  name: Identifier;
  params: Type[];
}

interface PredicateDecl extends ASTNode {
  tag: "PredicateDecl";
  name: Identifier;
  params: TypeVar[];
  args: Arg[];
}

interface FunctionDecl extends ASTNode {
  tag: "FunctionDecl";
  name: Identifier;
  params: TypeVar[];
  args: Arg[];
  output: Arg;
}
interface ConstructorDecl extends ASTNode {
  tag: "ConstructorDecl";
  name: Identifier;
  params: TypeVar[];
  args: Arg[];
  output: Arg;
}
interface PreludeDecl extends ASTNode {
  tag: "PreludeDecl";
  type: Type;
}
// TODO: check if string type is enough
interface NotationDecl extends ASTNode {
  tag: "NotationDecl";
  from: string;
  to: string;
}
interface SubTypeDecl extends ASTNode {
  tag: "SubTypeDecl";
  subType: Type;
  superType: Type;
}

//#endregion

//#region Style AST

/** Top level type for Style AST */
interface StyProg extends ASTNode {
  tag: "StyProg";
  blocks: HeaderBlock[];
}

// type HeaderBlock = [Header, Block];
interface HeaderBlock extends ASTNode {
  tag: "HeaderBlock";
  header: Header;
  block: Block;
}

interface Block extends ASTNode {
  tag: "Block";
  statements: Stmt[];
}

type Header = Selector | Namespace;

interface Selector extends ASTNode {
  tag: "Selector";
  head: DeclPatterns;
  with?: DeclPatterns;
  where?: RelationPatterns;
  namespace?: Namespace;
}

// NOTE: Instead of a js array typed child. I explicitly wrap them in an ASTNode so location and ancestry info can be better preserved.
// TODO: consider dropping the suffix pattern. It's a bit confusing, and DeclList would have been clearer.
interface DeclPatterns extends ASTNode {
  tag: "DeclPatterns";
  contents: DeclPattern[];
}

interface Namespace extends ASTNode {
  tag: "Namespace";
  contents: StyVar;
}

interface DeclPattern extends ASTNode {
  tag: "DeclPattern";
  type: StyT;
  id: BindingForm;
}

type RelationPattern = RelBind | RelPred;
interface RelationPatterns extends ASTNode {
  tag: "RelationPatterns";
  contents: RelationPattern[];
}

interface RelBind extends ASTNode {
  tag: "RelBind";
  id: BindingForm;
  expr: SelExpr;
}
interface RelPred extends ASTNode {
  tag: "RelPred";
  name: Identifier;
  args: PredArg[];
}

type PredArg = SEBind | RelPred;

// NOTE: the original type is unnecessarily nested and contain type constructor, which is deprecated.
type StyT = Identifier;
// type StyT = ISTTypeVar | ISTCtor;

interface ISTTypeVar {
  tag: "STTypeVar";
  contents: STypeVar;
}

interface ISTCtor {
  tag: "STCtor";
  contents: STypeCtor;
}

type STypeVar = ISTypeVar;

interface ISTypeVar {
  typeVarNameS: string;
  typeVarPosS: SourcePos;
}

type STypeCtor = ISTypeCtor;

interface ISTypeCtor {
  nameConsS: string;
  argConsS: SArg[];
  posConsS: SourcePos;
}

type SArg = ISAVar | ISAT;

interface ISAVar {
  tag: "SAVar";
  contents: BindingForm;
}

interface ISAT {
  tag: "SAT";
  contents: StyT;
}

type SelExpr = SEBind | SEFunc | SEValCons | SEFuncOrValCons;

interface SEBind extends ASTNode {
  tag: "SEBind";
  contents: BindingForm;
}

interface SEFunc extends ASTNode {
  tag: "SEFunc";
  name: Identifier;
  args: SelExpr[];
}

interface SEValCons extends ASTNode {
  tag: "SEValCons";
  name: Identifier;
  args: SelExpr[];
}
// NOTE: This type is used by the style compiler; since the grammar is ambiguous, the compiler will need to narrow down the type of this node when checking the AST.
interface SEFuncOrValCons extends ASTNode {
  tag: "SEFuncOrValCons";
  name: Identifier;
  args: SelExpr[];
}

type SourcePos = ISourcePos;

interface ISourcePos {
  sourceName: string;
  sourceLine: Pos;
  sourceColumn: Pos;
}

type Pos = IPos;

type IPos = number;

type Stmt = PathAssign | IOverride | Delete | IAnonAssign;

interface PathAssign extends ASTNode {
  tag: "PathAssign";
  type: StyType;
  path: Path;
  value: Expr;
}

interface IOverride extends ASTNode {
  tag: "Override";
  path: Path;
  value: Expr;
}

interface Delete extends ASTNode {
  tag: "Delete";
  contents: Path;
}

interface IAnonAssign extends ASTNode {
  tag: "AnonAssign";
  contents: Expr;
}

type StyType = ITypeOf | IListOf;

interface ITypeOf {
  tag: "TypeOf";
  contents: string;
}

interface IListOf {
  tag: "ListOf";
  contents: string;
}

//#endregion

//#region Substance AST and Domain environment types
type SubOut = ISubOut;

type SubProg = SubStmt[];

type ISubOut = [SubProg, [VarEnv, SubEnv], LabelMap];

type LabelMap = { [k: string]: string };

type SubEnv = ISubEnv;

interface ISubEnv {
  exprEqualities: [SubExpr, SubExpr][];
  predEqualities: [SubPredicate, SubPredicate][];
  subPreds: SubPredicate[];
}

type VarEnv = IVarEnv;

interface IVarEnv {
  typeConstructors: { [k: string]: ITypeConstructor };
  valConstructors: { [k: string]: ValConstructor };
  operators: { [k: string]: Operator };
  predicates: { [k: string]: PredicateEnv };

  subTypes: [T, T][];
  typeCtorNames: string[];
  declaredNames: string[];
  stmtNotations: StmtNotationRule[];
  errors: string;
}
// type TypeConstructor = ITypeConstructor;

interface ITypeConstructor {
  nametc: string;
  kindstc: K[];
}

type K = IKtype | IKT;

interface IKtype {
  tag: "Ktype";
  contents: Type;
}

interface IKT {
  tag: "KT";
  contents: T;
}

type T = ITTypeVar | ITConstr;

interface ITTypeVar {
  tag: "TTypeVar";
  contents: TypeVar;
}

interface ITConstr {
  tag: "TConstr";
  contents: TypeCtorApp;
}

type Y = ITypeVarY | IVarY;

interface ITypeVarY {
  tag: "TypeVarY";
  contents: TypeVar;
}

interface IVarY {
  tag: "VarY";
}

type Arg = IAVar | IAT;

interface IAVar {
  tag: "AVar";
}

interface IAT {
  tag: "AT";
  contents: T;
}

type TypeVar = ITypeVar;

interface ITypeVar {
  typeVarName: string;
  typeVarPos: SourcePos;
}

type SubExpr =
  | IVarE
  | IApplyFunc
  | IApplyValCons
  | IDeconstructorE
  | IStringLit;

interface IVarE {
  tag: "VarE";
}

interface IApplyFunc {
  tag: "ApplyFunc";
  contents: Func;
}

interface IApplyValCons {
  tag: "ApplyValCons";
  contents: Func;
}

interface IDeconstructorE {
  tag: "DeconstructorE";
  contents: Deconstructor;
}

interface IStringLit {
  tag: "StringLit";
  contents: string;
}

type SubPredicate = ISubPredicate;

interface ISubPredicate {
  predicateName: string;
  predicateArgs: SubPredArg[];
  predicatePos: SourcePos;
}

type ValConstructor = IValConstructor;

interface IValConstructor {
  namevc: string;
  ylsvc: Y[];
  kindsvc: K[];
  tlsvc: T[];
  tvc: T;
}

type Type = IType;

interface IType {
  typeName: string;
  typePos: SourcePos;
}

type TypeCtorApp = ITypeCtorApp;

interface ITypeCtorApp {
  nameCons: string;
  argCons: Arg[];
  constructorInvokerPos: SourcePos;
}

type Func = IFunc;

interface IFunc {
  nameFunc: string;
  argFunc: SubExpr[];
}

type Operator = IOperator;

interface IOperator {
  nameop: string;
  ylsop: Y[];
  kindsop: K[];
  tlsop: T[];
  top: T;
}

type Deconstructor = IDeconstructor;

interface IDeconstructor {
  fieldDeconstructor: string;
}

type PredicateEnv = IPred1 | IPred2;

interface IPred1 {
  tag: "Pred1";
  contents: Predicate1;
}

interface IPred2 {
  tag: "Pred2";
  contents: Predicate2;
}

type SubPredArg = IPE | IPP;

interface IPE {
  tag: "PE";
  contents: SubExpr;
}

interface IPP {
  tag: "PP";
  contents: SubPredicate;
}

type LabelOption = IDefault | IIDs;

interface IDefault {
  tag: "Default";
}

interface IIDs {
  tag: "IDs";
}

type SubStmt =
  | IDecl
  | IBind
  | IEqualE
  | IEqualQ
  | IApplyP
  | ILabelDecl
  | IAutoLabel
  | INoLabel;

interface IDecl {
  tag: "Decl";
}

interface IBind {
  tag: "Bind";
}

interface IEqualE {
  tag: "EqualE";
  contents: [SubExpr, SubExpr];
}

interface IEqualQ {
  tag: "EqualQ";
  contents: [SubPredicate, SubPredicate];
}

interface IApplyP {
  tag: "ApplyP";
  contents: SubPredicate;
}

interface ILabelDecl {
  tag: "LabelDecl";
}

interface IAutoLabel {
  tag: "AutoLabel";
  contents: LabelOption;
}

interface INoLabel {
  tag: "NoLabel";
}

type StmtNotationRule = IStmtNotationRule;

interface IStmtNotationRule {
  fromSnr: Token[];
  toSnr: Token[];
  patternsSnr: Token[];
  entitiesSnr: Token[];
}

type Predicate1 = IPrd1;

interface IPrd1 {
  namepred1: string;
  ylspred1: Y[];
  kindspred1: K[];
  tlspred1: T[];
}

type Predicate2 = IPrd2;

interface IPrd2 {
  namepred2: string;
  plspred2: Prop[];
}

type Token =
  | IBind
  | IIterator
  | IIter
  | INewLine
  | IPredEq
  | IExprEq
  | IComma
  | ILparen
  | IRparen
  | ISpace
  | ISym
  | IVar
  | IStringLit
  | IRecursivePattern
  | IRecursivePatternElement
  | ISinglePatternElement
  | IPattern
  | IEntitiy
  | IDSLLEntity
  | ILabel
  | IAutoLabel
  | IComment
  | IStartMultiComment
  | IEndMultiComment;

interface IBind {
  tag: "Bind";
}

interface IIterator {
  tag: "Iterator";
}

interface IIter {
  tag: "Iter";
}

interface INewLine {
  tag: "NewLine";
}

interface IPredEq {
  tag: "PredEq";
}

interface IExprEq {
  tag: "ExprEq";
}

interface IComma {
  tag: "Comma";
}

interface ILparen {
  tag: "Lparen";
}

interface IRparen {
  tag: "Rparen";
}

interface ISpace {
  tag: "Space";
}

interface ISym {
  tag: "Sym";
  contents: string;
}

interface IVar {
  tag: "Var";
  contents: string;
}

interface IStringLit {
  tag: "StringLit";
  contents: string;
}

interface IRecursivePattern {
  tag: "RecursivePattern";
  contents: Token[];
}

interface IRecursivePatternElement {
  tag: "RecursivePatternElement";
  contents: Token[];
}

interface ISinglePatternElement {
  tag: "SinglePatternElement";
  contents: Token[];
}

interface IPattern {
  tag: "Pattern";
  contents: [string, boolean];
}

interface IEntitiy {
  tag: "Entitiy";
  contents: string;
}

interface IDSLLEntity {
  tag: "DSLLEntity";
  contents: string;
}

interface ILabel {
  tag: "Label";
  contents: string;
}

interface IAutoLabel {
  tag: "AutoLabel";
}

interface IComment {
  tag: "Comment";
  contents: string;
}

interface IStartMultiComment {
  tag: "StartMultiComment";
  contents: string;
}

interface IEndMultiComment {
  tag: "EndMultiComment";
  contents: string;
}

type Prop = IProp;

interface IProp {
  propName: string;
  propPos: SourcePos;
}

//#endregion

//#region AST nodes
interface SourceLoc {
  line: number;
  col: number;
}

interface ASTNode {
  tag: string;
  start: SourceLoc;
  end: SourceLoc;
  // Optionally for querying
  // children: ASTNode[];
  // file: string;
  // parent: ASTNode; // NOTE: pointer type; don't serialize this
}

interface Identifier extends ASTNode {
  type: string;
  value: string;
}

//#endregion
