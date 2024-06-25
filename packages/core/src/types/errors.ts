import im from "immutable";
import * as ad from "./ad.js";
import {
  A,
  AbstractNode,
  C,
  Identifier,
  NodeType,
  SourceLoc,
  SourceRange,
} from "./ast.js";
import { Arg, Type } from "./domain.js";
import { FuncParam, FuncSignature } from "./functions.js";
import { State } from "./state.js";
import {
  BinOp,
  BindingForm,
  ColorLit,
  Expr,
  GPIDecl,
  LayoutStages,
  Path,
  UOp,
} from "./style.js";
import {
  Resolved,
  ResolvedExpr,
  ResolvedPath,
  ResolvedStylePath,
  ResolvedUnindexedStylePath,
  StylePath,
  StylePathToCollection,
  StylePathToNamespaceScope,
  StylePathToSubstanceScope,
  StylePathToUnindexedObject,
} from "./stylePathResolution.js";
import { StmtSet, SubExpr, TypeApp } from "./substance.js";
import { ArgValWithExpr, ShapeVal, Val, Value } from "./value.js";

//#region ErrorTypes

export type PenroseError =
  | (DomainError & { errorType: "DomainError" })
  | (SubstanceError & { errorType: "SubstanceError" })
  | (StyleError & { errorType: "StyleError" })
  | (RuntimeError & { errorType: "RuntimeError" });

export type RuntimeError = RuntimeErrorWithContents | NaNError;

export interface RuntimeErrorWithContents {
  tag: "RuntimeError";
  message: string;
}
export interface NaNError {
  tag: "NaNError";
  message: string;
  lastState: State;
}

export type Warning = StyleWarning;

// TODO: does type var ever appear in Substance? If not, can we encode that at the type level?
export type SubstanceError =
  | ParseError
  | DuplicateName
  | TypeNotFound
  | TypeMismatch
  | ArgLengthMismatch
  | VarNotFound
  | InvalidSetIndexingError
  | BadSetIndexRangeError
  | DuplicateIndexError
  | DivideByZeroError
  | InvalidArithmeticValueError
  | UnsupportedIndexingError
  | DeclLiteralError
  | FatalError; // TODO: resolve all fatal errors in the Substance module

export type DomainError =
  | ParseError
  | TypeDeclared
  | TypeNotFound
  | DuplicateName
  | CyclicSubtypes
  | SymmetricTypeMismatch
  | SymmetricArgLengthMismatch
  | OutputLiteralTypeError
  | SubOrSuperLiteralTypeError;

export interface SymmetricTypeMismatch {
  tag: "SymmetricTypeMismatch";
  sourceExpr: AbstractNode;
}

export interface SymmetricArgLengthMismatch {
  tag: "SymmetricArgLengthMismatch";
  sourceExpr: AbstractNode;
}

export interface OutputLiteralTypeError {
  tag: "OutputLiteralTypeError";
  type: Type<A>;
  location: AbstractNode;
}

export interface SubOrSuperLiteralTypeError {
  tag: "SubOrSuperLiteralTypeError";
  type: Type<A>;
  location: AbstractNode;
}

export interface InvalidSetIndexingError {
  tag: "InvalidSetIndexingError";
  index: string;
  location: AbstractNode;
  suggestions: string[];
}

export interface BadSetIndexRangeError {
  tag: "BadSetIndexRangeError";
  index: number;
  location: AbstractNode;
}

export interface DuplicateIndexError {
  tag: "DuplicateIndexError";
  index: string;
  location: AbstractNode;
}

export interface DivideByZeroError {
  tag: "DivideByZeroError";
  location: AbstractNode;
}

export interface InvalidArithmeticValueError {
  tag: "InvalidArithmeticValueError";
  location: AbstractNode;
  value: number;
}

export interface UnsupportedIndexingError {
  tag: "UnsupportedIndexingError";
  iset: StmtSet<A>;
}

export interface DeclLiteralError {
  tag: "DeclLiteralError";
  location: AbstractNode;
  type: TypeApp<A>;
}

export interface CyclicSubtypes {
  tag: "CyclicSubtypes";
  cycles: string[][];
}

export interface TypeDeclared {
  tag: "TypeDeclared";
  typeName: Identifier<A>;
  firstDefined: AbstractNode;
}
export interface DuplicateName {
  tag: "DuplicateName";
  name: Identifier<A>;
  location: AbstractNode;
  firstDefined: AbstractNode;
}
export interface TypeNotFound {
  tag: "TypeNotFound";
  typeName: Identifier<A>;
  possibleTypes?: Identifier<A>[];
}
export interface VarNotFound {
  tag: "VarNotFound";
  variable: Identifier<A>;
  possibleVars?: Identifier<A>[];
}

export interface TypeMismatch {
  tag: "TypeMismatch";
  sourceType: TypeApp<A>;
  expectedType: Type<A>;
  sourceExpr: AbstractNode;
  expectedExpr: AbstractNode;
}
export interface ArgLengthMismatch {
  tag: "ArgLengthMismatch";
  name: Identifier<A>;
  argsGiven: SubExpr<A>[];
  argsExpected: Arg<A>[];
  sourceExpr: AbstractNode;
  expectedExpr: AbstractNode;
}
export interface MultipleLayoutError {
  tag: "MultipleLayoutError";
  decls: LayoutStages<C>[];
}

// NOTE: for debugging purposes
export interface FatalError {
  tag: "Fatal";
  message: string;
}

// NOTE: aliased to AbstractNode for now, can include more types for different errors
export type ErrorSource = AbstractNode;

//#endregion

//#region Style errors

export type StyleError =
  // Misc errors
  | ParseError
  | GenericStyleError
  | StyleErrorList
  | InvalidColorLiteral
  // Selector errors (from Substance)
  | SelectorVarMultipleDecl
  | SelectorFieldNotSupported
  | TaggedSubstanceError
  | SelectorAliasNamingError
  // Block static errors
  | InvalidGPITypeError
  | InvalidGPIPropertyError
  | InvalidFunctionNameError
  | InvalidObjectiveNameError
  | InvalidConstraintNameError
  // Compilation errors
  | AssignAccessError
  | AssignGlobalError
  | AssignSubstanceError
  | BadElementError
  | BadIndexError
  | UnindexableItemError
  | BinOpTypeError
  | CanvasNonexistentDimsError
  | CyclicAssignmentError
  | DeleteGlobalError
  | DeleteSubstanceError
  | MultipleLayoutError
  | MissingPathError
  | UndeclaredSubVarError
  | PathToCollectionError
  | PathToNamespaceError
  | PathToSubstanceError
  | CollectionMemberAccessError
  | MissingShapeError
  | NestedShapeError
  | NotShapeError
  | NotValueError
  | OutOfBoundsError
  | UOpTypeError
  | BadShapeParamTypeError
  | BadArgumentTypeError
  | MissingArgumentError
  | TooManyArgumentsError
  | FunctionInternalError
  | RedeclareNamespaceError
  | NotSubstanceCollectionError
  | NotStyleVariableError
  | LayerOnNonShapesError
  | NonWellFormedPathError
  // Runtime errors
  | RuntimeValueTypeError;

// Compilation warnings
export type StyleWarning =
  | ImplicitOverrideWarning
  | NoopDeleteWarning
  | LayerCycleWarning
  | ShapeBelongsToMultipleGroupsWarning
  | GroupCycleWarning
  | FunctionInternalWarning;

export type FunctionInternalWarning = BBoxApproximationWarning;

export interface StyleDiagnostics {
  errors: im.List<StyleError>;
  warnings: im.List<StyleWarning>;
}

//#region compilation warnings

export interface ImplicitOverrideWarning {
  tag: "ImplicitOverrideWarning";
  path: ResolvedUnindexedStylePath<A>;
}

export interface NoopDeleteWarning {
  tag: "NoopDeleteWarning";
  path: ResolvedUnindexedStylePath<A>;
}
export interface LayerCycleWarning {
  tag: "LayerCycleWarning";
  cycles: string[][];
  approxOrdering: string[];
}
export interface ShapeBelongsToMultipleGroupsWarning {
  tag: "ShapeBelongsToMultipleGroups";
  shape: string;
  groups: string[];
}
export interface GroupCycleWarning {
  tag: "GroupCycleWarning";
  cycles: string[][];
}

export interface BBoxApproximationWarning {
  tag: "BBoxApproximationWarning";
  // tail is the top of stack
  stack: [BBoxApproximationWarningItem, ...BBoxApproximationWarningItem[]];
}

export interface BBoxApproximationWarningItem {
  signature: string;
  callExpression?: ResolvedExpr<A>;
}

//#endregion

export interface GenericStyleError {
  tag: "GenericStyleError";
  messages: string[];
}

export interface StyleErrorList {
  tag: "StyleErrorList";
  errors: StyleError[];
}

export interface ParseError {
  tag: "ParseError";
  message: string;
  location?: SourceLoc;
  fileType?: NodeType;
}

export interface InvalidColorLiteral {
  tag: "InvalidColorLiteral";
  color: ColorLit<A>;
}

export interface SelectorVarMultipleDecl {
  tag: "SelectorVarMultipleDecl";
  varName: BindingForm<A>;
}

// export interface SelectorRelTypeMismatch {
//   tag: "SelectorRelTypeMismatch";
//   varType: TypeConsApp<A>;
//   exprType: TypeConsApp<A>;
// }

export interface SelectorFieldNotSupported {
  tag: "SelectorFieldNotSupported";
  name: BindingForm<A>;
  field: Identifier<A>;
}

export interface TaggedSubstanceError {
  tag: "TaggedSubstanceError";
  error: SubstanceError;
}

export interface SelectorAliasNamingError {
  tag: "SelectorAliasNamingError";
  alias: Identifier<A>;
}

//#region Block statics

export interface InvalidGPITypeError {
  tag: "InvalidGPITypeError";
  givenType: Identifier<A>;
  // expectedType: string;
}

export interface InvalidGPIPropertyError {
  tag: "InvalidGPIPropertyError";
  givenProperty: Identifier<A>;
  expectedProperties: string[];
}

export interface InvalidFunctionNameError {
  tag: "InvalidFunctionNameError";
  givenName: Identifier<A>;
  // expectedName: string;
}

export interface InvalidObjectiveNameError {
  tag: "InvalidObjectiveNameError";
  givenName: Identifier<A>;
  // expectedName: string;
}

export interface InvalidConstraintNameError {
  tag: "InvalidConstraintNameError";
  givenName: Identifier<A>;
  // expectedName: string;
}

//#endregion Block statics

//#region compilation errors

export interface AssignAccessError {
  tag: "AssignAccessError";
  path: ResolvedStylePath<A>;
}

export interface AssignGlobalError {
  tag: "AssignGlobalError";
  path: StylePathToNamespaceScope<A>;
}

export interface AssignSubstanceError {
  tag: "AssignSubstanceError";
  path: StylePathToSubstanceScope<A> | StylePathToCollection<A>;
}

export interface BadElementError {
  tag: "BadElementError";
  coll: ResolvedExpr<A>;
  index: number;
}

export interface BadIndexError {
  tag: "BadIndexError";
  expr: ResolvedExpr<A>;
}

export interface UnindexableItemError {
  tag: "UnindexableItemError";
  expr: ResolvedUnindexedStylePath<A>;
}

export interface BinOpTypeError {
  tag: "BinOpTypeError";
  expr: Resolved<BinOp<A>>;
  left: Value<ad.Num>["tag"];
  right: Value<ad.Num>["tag"];
}

export interface CanvasNonexistentDimsError {
  tag: "CanvasNonexistentDimsError";
  attr: "width" | "height";
  kind: "missing" | "GPI" | "wrong type";
  type?: Expr<A>["tag"];
}

export interface CyclicAssignmentError {
  tag: "CyclicAssignmentError";
  // TODO: improve types, currently the generated id and source location
  cycles: { id: string; src: SourceRange | undefined }[][];
}

export interface DeleteGlobalError {
  tag: "DeleteGlobalError";
  path: StylePath<A>;
}

export interface DeleteSubstanceError {
  tag: "DeleteSubstanceError";
  path: StylePath<A>;
}

export interface MissingPathError {
  tag: "MissingPathError";
  path: StylePath<A>;
}

export interface UndeclaredSubVarError {
  tag: "UndeclaredSubVarError";
  name: Identifier<A>;
}

export interface PathToCollectionError {
  tag: "PathToCollectionError";
  path: StylePathToCollection<A>;
}

export interface PathToSubstanceError {
  tag: "PathToSubstanceError";
  path: StylePathToSubstanceScope<A>;
}

export interface PathToNamespaceError {
  tag: "PathToNamespaceError";
  path: StylePathToNamespaceScope<A>;
}

export interface CollectionMemberAccessError {
  tag: "CollectionMemberAccessError";
  path: StylePathToCollection<A>;
  field: string;
}

export interface MissingShapeError {
  tag: "MissingShapeError";
  path: StylePath<A>;
}

export interface NestedShapeError {
  tag: "NestedShapeError";
  expr: Resolved<GPIDecl<A>>;
}

export interface NotShapeError {
  tag: "NotShapeError";
  path: StylePathToUnindexedObject<A>;
  what: string;
}

export interface NotValueError {
  tag: "NotValueError";
  expr: ResolvedExpr<A>;
  what?: string;
}

export interface OutOfBoundsError {
  tag: "OutOfBoundsError";
  expr: ResolvedPath<A>;
  indices: number[];
}

export interface UOpTypeError {
  tag: "UOpTypeError";
  expr: Resolved<UOp<A>>;
  arg: Value<ad.Num>["tag"];
}

export interface BadShapeParamTypeError {
  tag: "BadShapeParamTypeError";
  path: StylePathToUnindexedObject<A>;
  value: Val<ad.Num> | ShapeVal<ad.Num>;
  expectedType: string;
  passthrough: boolean;
}

export interface BadArgumentTypeError {
  tag: "BadArgumentTypeError";
  funcName: string;
  formalArg: FuncParam;
  actualArg: ArgValWithExpr<ad.Num>;
}

export interface MissingArgumentError {
  tag: "MissingArgumentError";
  funcName: string;
  formalArg: FuncParam;
  callExpression: ResolvedExpr<A>;
}

export interface TooManyArgumentsError {
  tag: "TooManyArgumentsError";
  func: FuncSignature;
  callExpression: ResolvedExpr<A>;
  numProvided: number;
}

export interface FunctionInternalError {
  tag: "FunctionInternalError";
  // NOTE: to be compatible with webworkers, the function body cannot be cloned and can be therefore excluded.
  func: FuncSignature;
  callExpression: ResolvedExpr<A>;
  message: string;
}

export interface RedeclareNamespaceError {
  tag: "RedeclareNamespaceError";
  existingNamespace: string;
  location: SourceRange; // location of the duplicated declaration
}

export interface NotSubstanceCollectionError {
  tag: "NotSubstanceCollectionError";
  path: ResolvedPath<A>;
}

export interface NotStyleVariableError {
  tag: "NotStyleVariableError";
  path: ResolvedPath<A>;
}

export interface LayerOnNonShapesError {
  tag: "LayerOnNonShapesError";
  path: ResolvedPath<A>;
}

export interface NonWellFormedPathError {
  tag: "NonWellFormedPathError";
  path: ResolvedUnindexedStylePath<A>;
}

//#endregion

// TODO(errors): use identifiers here
export interface RuntimeValueTypeError {
  tag: "RuntimeValueTypeError";
  path: Path<A>;
  expectedType: string;
  actualType: string;
}

//#endregion Style errors
