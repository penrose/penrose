import im from "immutable";
import * as ad from "types/ad";
import { A, AbstractNode, C, Identifier, SourceLoc } from "./ast";
import { Arg, TypeConstructor, TypeVar } from "./domain";
import { State } from "./state";
import { BindingForm, BinOp, Expr, GPIDecl, Path, UOp } from "./style";
import { ResolvedPath } from "./styleSemantics";
import { Deconstructor, SubExpr, TypeConsApp } from "./substance";
import { Value } from "./value";

//#region ErrorTypes

// type PenroseError = LanguageError | RuntimeError;
// type LanguageError = DomainError | SubstanceError | StyleError | PluginError;
// type RuntimeError = OptimizerError | EvaluatorError;
// type StyleError = StyleParseError | StyleCheckError | TranslationError;
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

export type Warning = StyleError;

// TODO: does type var ever appear in Substance? If not, can we encode that at the type level?
export type SubstanceError =
  | ParseError
  | DuplicateName
  | TypeNotFound
  | TypeVarNotFound
  | TypeMismatch
  | ArgLengthMismatch
  | TypeArgLengthMismatch
  | VarNotFound
  | DeconstructNonconstructor
  | UnexpectedExprForNestedPred
  | FatalError; // TODO: resolve all fatal errors in the Substance module

export type DomainError =
  | ParseError
  | TypeDeclared
  | TypeVarNotFound
  | TypeNotFound
  | DuplicateName
  | CyclicSubtypes;

export interface UnexpectedExprForNestedPred {
  tag: "UnexpectedExprForNestedPred";
  sourceType: TypeConstructor<A>;
  sourceExpr: AbstractNode;
  expectedExpr: AbstractNode;
}

export interface CyclicSubtypes {
  tag: "CyclicSubtypes";
  cycles: string[][];
}

export interface TypeDeclared {
  tag: "TypeDeclared";
  typeName: Identifier<A>;
}
export interface DuplicateName {
  tag: "DuplicateName";
  name: Identifier<A>;
  location: AbstractNode;
  firstDefined: AbstractNode;
}
export interface TypeVarNotFound {
  tag: "TypeVarNotFound";
  typeVar: TypeVar<A>;
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
  sourceType: TypeConstructor<A>;
  expectedType: TypeConstructor<A>;
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

export interface TypeArgLengthMismatch {
  tag: "TypeArgLengthMismatch";
  sourceType: TypeConstructor<A>;
  expectedType: TypeConstructor<A>;
  sourceExpr: AbstractNode;
  expectedExpr: AbstractNode;
}

export interface DeconstructNonconstructor {
  tag: "DeconstructNonconstructor";
  deconstructor: Deconstructor<A>;
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
  // Selector errors (from Substance)
  | SelectorVarMultipleDecl
  | SelectorDeclTypeMismatch
  | SelectorRelTypeMismatch
  | SelectorFieldNotSupported
  | TaggedSubstanceError
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
  | BinOpTypeError
  | CanvasNonexistentDimsError
  | DeleteAccessError
  | DeleteGlobalError
  | DeleteSubstanceError
  | MissingPathError
  | MissingShapeError
  | NestedShapeError
  | NotCollError
  | NotShapeError
  | NotValueError
  | OutOfBoundsError
  | PropertyMemberError
  | UOpTypeError
  // Runtime errors
  | RuntimeValueTypeError;

export type StyleWarning =
  | IntOrFloat
  // Compilation warnings
  | ImplicitOverrideWarning
  | NoopDeleteWarning;

export interface StyleDiagnostics {
  errors: im.List<StyleError>;
  warnings: im.List<StyleWarning>;
}

export interface IntOrFloat {
  tag: "IntOrFloat";
  message: string;
} // COMBAK: Use this in block checking

//#region compilation warnings

export interface ImplicitOverrideWarning {
  tag: "ImplicitOverrideWarning";
  path: ResolvedPath<C>;
}

export interface NoopDeleteWarning {
  tag: "NoopDeleteWarning";
  path: ResolvedPath<C>;
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
}

export interface SelectorVarMultipleDecl {
  tag: "SelectorVarMultipleDecl";
  varName: BindingForm<A>;
}

export interface SelectorDeclTypeMismatch {
  tag: "SelectorDeclTypeMismatch";
  subType: TypeConsApp<A>;
  styType: TypeConsApp<A>;
}

export interface SelectorRelTypeMismatch {
  tag: "SelectorRelTypeMismatch";
  varType: TypeConsApp<A>;
  exprType: TypeConsApp<A>;
}

export interface SelectorFieldNotSupported {
  tag: "SelectorFieldNotSupported";
  name: BindingForm<A>;
  field: Identifier<A>;
}

export interface TaggedSubstanceError {
  tag: "TaggedSubstanceError";
  error: SubstanceError;
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
  path: Path<C>;
}

export interface AssignGlobalError {
  tag: "AssignGlobalError";
  path: ResolvedPath<C>;
}

export interface AssignSubstanceError {
  tag: "AssignSubstanceError";
  path: ResolvedPath<C>;
}

export interface BadElementError {
  tag: "BadElementError";
  coll: Expr<C>;
}

export interface BadIndexError {
  tag: "BadIndexError";
  expr: Expr<C>;
}

export interface BinOpTypeError {
  tag: "BinOpTypeError";
  expr: BinOp<C>;
  left: Value<ad.Num>["tag"];
  right: Value<ad.Num>["tag"];
}

export interface CanvasNonexistentDimsError {
  tag: "CanvasNonexistentDimsError";
  attr: "width" | "height";
  kind: "missing" | "wrong type";
  type?: string;
}

export interface DeleteAccessError {
  tag: "DeleteAccessError";
  path: Path<C>;
}

export interface DeleteGlobalError {
  tag: "DeleteGlobalError";
  path: ResolvedPath<C>;
}

export interface DeleteSubstanceError {
  tag: "DeleteSubstanceError";
  path: ResolvedPath<C>;
}

export interface MissingPathError {
  tag: "MissingPathError";
  path: string;
}

export interface MissingShapeError {
  tag: "MissingShapeError";
  path: ResolvedPath<C>;
}

export interface NestedShapeError {
  tag: "NestedShapeError";
  expr: GPIDecl<C>;
}

export interface NotCollError {
  tag: "NotCollError";
  expr: Expr<C>;
}

export interface NotShapeError {
  tag: "NotShapeError";
  path: ResolvedPath<C>;
}

export interface NotValueError {
  tag: "NotValueError";
  expr: Expr<C>;
}

export interface OutOfBoundsError {
  tag: "OutOfBoundsError";
  expr: Path<C>;
  indices: number[];
}

export interface PropertyMemberError {
  tag: "PropertyMemberError";
  path: ResolvedPath<C>;
}

export interface UOpTypeError {
  tag: "UOpTypeError";
  expr: UOp<C>;
  arg: Value<ad.Num>["tag"];
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
