type Shape = IShape;

interface IShape {
  shapeType: string;
  properties: {[k: string]: Value<number>};
}

type ArgVal<T> = IGPI<T> | IVal<T>;

interface IGPI<T> {
  tag: "GPI";
  contents: [string, {[k: string]: Value<T>}];
}

interface IVal<T> {
  tag: "Val";
  contents: Value<T>;
}

type Translation<T> = ITrans<T>;

interface ITrans<T> {
  trMap: {[k: string]: {[k: string]: FieldExpr<T>}};
  warnings: string[];
}

type FieldExpr<T> = IFExpr<T> | IFGPI<T>;

interface IFExpr<T> {
  tag: "FExpr";
  contents: TagExpr<T>;
}

interface IFGPI<T> {
  tag: "FGPI";
  contents: [string, {[k: string]: TagExpr<T>}];
}

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

type Expr = IIntLit | IAFloat | IStringLit | IBoolLit | IEPath | ICompApp | IObjFn | IConstrFn | IAvoidFn | IBinOp | IUOp | IList | ITuple | IVector | IMatrix | IVectorAccess | IMatrixAccess | IListAccess | ICtor | ILayering | IPluginAccess | IThenOp;

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

type Path = IFieldPath | IPropertyPath | ILocalVar | IAccessPath;

interface IFieldPath {
  tag: "FieldPath";
  contents: [BindingForm, string];
}

interface IPropertyPath {
  tag: "PropertyPath";
  contents: [BindingForm, string, string];
}

interface ILocalVar {
  tag: "LocalVar";
  contents: string;
}

interface IAccessPath {
  tag: "AccessPath";
  contents: [Path, number[]];
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

type Value<T> = IFloatV<T> | IIntV<T> | IBoolV<T> | IStrV<T> | IPtV<T> | IPathDataV<T> | IPtListV<T> | IPaletteV<T> | IColorV<T> | IFileV<T> | IStyleV<T> | IListV<T> | ITupV<T> | IVectorV<T> | IMatrixV<T> | ILListV<T> | IHMatrixV<T> | IPolygonV<T>;

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

interface IVectorV<T> {
  tag: "VectorV";
  contents: T[];
}

interface IMatrixV<T> {
  tag: "MatrixV";
  contents: T[][];
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

type Elem<T> = IPt<T> | ICubicBez<T> | ICubicBezJoin<T> | IQuadBez<T> | IQuadBezJoin<T>;

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

type OptStatus = INewIter | IUnconstrainedRunning | IUnconstrainedConverged | IEPConverged;

interface INewIter {
  tag: "NewIter";
}

interface IUnconstrainedRunning {
  tag: "UnconstrainedRunning";
  contents: number[];
}

interface IUnconstrainedConverged {
  tag: "UnconstrainedConverged";
  contents: number[];
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
  weight: number;
  optStatus: OptStatus;
  bfgsInfo: BfgsParams;
}

type Header = ISelect | INamespace;

interface ISelect {
  tag: "Select";
  contents: Selector;
}

interface INamespace {
  tag: "Namespace";
  contents: StyVar;
}

type Selector = ISelector;

interface ISelector {
  selHead: DeclPattern[];
  selWith: DeclPattern[];
  selWhere: RelationPattern[];
  selNamespace?: string;
}

type DeclPattern = IPatternDecl;

type IPatternDecl = [StyT, BindingForm];

type RelationPattern = IRelBind | IRelPred;

interface IRelBind {
  tag: "RelBind";
  contents: [BindingForm, SelExpr];
}

interface IRelPred {
  tag: "RelPred";
  contents: Predicate;
}

type Predicate = IPredicate;

interface IPredicate {
  predicateName: string;
  predicateArgs: PredArg[];
  predicatePos: SourcePos;
}

type PredArg = IPE | IPP;

interface IPE {
  tag: "PE";
  contents: SelExpr;
}

interface IPP {
  tag: "PP";
  contents: Predicate;
}

type StyT = ISTTypeVar | ISTCtor;

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

type SelExpr = ISEBind | ISEAppFunc | ISEAppValCons;

interface ISEBind {
  tag: "SEBind";
  contents: BindingForm;
}

interface ISEAppFunc {
  tag: "SEAppFunc";
  contents: [string, SelExpr[]];
}

interface ISEAppValCons {
  tag: "SEAppValCons";
  contents: [string, SelExpr[]];
}

type SourcePos = ISourcePos;

interface ISourcePos {
  sourceName: string;
  sourceLine: Pos;
  sourceColumn: Pos;
}

type Pos = IPos;

type IPos = number;

type Stmt = IPathAssign | IOverride | IDelete | IAnonAssign;

interface IPathAssign {
  tag: "PathAssign";
  contents: [StyType, Path, Expr];
}

interface IOverride {
  tag: "Override";
  contents: [Path, Expr];
}

interface IDelete {
  tag: "Delete";
  contents: Path;
}

interface IAnonAssign {
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

type SubOut = ISubOut;

type ISubOut = [SubStmt[], [VarEnv, SubEnv], {[k: string]: string}];

type SubEnv = ISubEnv;

interface ISubEnv {
  exprEqualities: [SubExpr, SubExpr][];
  predEqualities: [SubPredicate, SubPredicate][];
  bindings: {[k: Var]: SubExpr};
  subPreds: SubPredicate[];
}

type VarEnv = IVarEnv;

interface IVarEnv {
  typeConstructors: {[k: string]: TypeConstructor};
  valConstructors: {[k: string]: ValConstructor};
  operators: {[k: string]: Operator};
  predicates: {[k: string]: PredicateEnv};
  typeVarMap: {[k: TypeVar]: Type};
  typeValConstructor: {[k: T]: ValConstructor};
  varMap: {[k: Var]: T};
  preludes: [Var, T][];
  subTypes: [T, T][];
  typeCtorNames: string[];
  declaredNames: string[];
  stmtNotations: StmtNotationRule[];
  errors: string;
}

type TypeConstructor = ITypeConstructor;

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
  contents: Var;
}

type Arg = IAVar | IAT;

interface IAVar {
  tag: "AVar";
  contents: Var;
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

type SubExpr = IVarE | IApplyFunc | IApplyValCons | IDeconstructorE | IStringLit;

interface IVarE {
  tag: "VarE";
  contents: Var;
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
  nsvc: Var[];
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
  varDeconstructor: Var;
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
  contents: Var[];
}

type SubStmt = IDecl | IBind | IEqualE | IEqualQ | IApplyP | ILabelDecl | IAutoLabel | INoLabel;

interface IDecl {
  tag: "Decl";
  contents: [T, Var];
}

interface IBind {
  tag: "Bind";
  contents: [Var, SubExpr];
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
  contents: [Var, string];
}

interface IAutoLabel {
  tag: "AutoLabel";
  contents: LabelOption;
}

interface INoLabel {
  tag: "NoLabel";
  contents: Var[];
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

type Token = IBind | IIterator | IIter | INewLine | IPredEq | IExprEq | IComma | ILparen | IRparen | ISpace | ISym | IVar | IStringLit | IRecursivePattern | IRecursivePatternElement | ISinglePatternElement | IPattern | IEntitiy | IDSLLEntity | ILabel | IAutoLabel | IComment | IStartMultiComment | IEndMultiComment;

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
  contents: string;
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
