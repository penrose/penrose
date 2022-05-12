//#region ErrorTypes
import { A, AbstractNode, Identifier, SourceLoc } from "./ast";
import { Arg, Prop, TypeConstructor, TypeVar } from "./domain";
import { State } from "./state";
import { BindingForm, Path } from "./style";
import { Deconstructor, SubExpr, TypeConsApp } from "./substance";

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
export type StyleErrors = StyleError[];

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
  | CyclicSubtypes
  | NotTypeConsInPrelude;

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
export interface NotTypeConsInPrelude {
  tag: "NotTypeConsInPrelude";
  type: Prop<A> | TypeVar<A>;
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
  // Translation errors (deletion)
  | DeletedPropWithNoSubObjError
  | DeletedPropWithNoFieldError
  | DeletedPropWithNoGPIError
  | CircularPathAlias
  | DeletedNonexistentFieldError
  | DeletedVectorElemError
  // Translation errors (insertion)
  | InsertedPathWithoutOverrideError
  | InsertedPropWithNoFieldError
  | InsertedPropWithNoGPIError
  // Translation validation errors
  | NonexistentNameError
  | NonexistentFieldError
  | NonexistentGPIError
  | NonexistentPropertyError
  | ExpectedGPIGotFieldError
  | InvalidAccessPathError
  | CanvasNonexistentError
  | CanvasNonexistentDimsError
  // Runtime errors
  | RuntimeValueTypeError;

export type StyleWarning = IntOrFloat;

export type StyleWarnings = StyleWarning[];

export interface StyleResults {
  errors: StyleErrors;
  warnings: StyleWarnings;
}

export interface IntOrFloat {
  tag: "IntOrFloat";
  message: string;
} // COMBAK: Use this in block checking

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

export interface DeletedPropWithNoSubObjError {
  tag: "DeletedPropWithNoSubObjError";
  subObj: BindingForm<A>;
  path: Path<A>;
}

export interface DeletedPropWithNoFieldError {
  tag: "DeletedPropWithNoFieldError";
  subObj: BindingForm<A>;
  field: Identifier<A>;
  path: Path<A>;
}

export interface CircularPathAlias {
  tag: "CircularPathAlias";
  path: Path<A>;
}

export interface DeletedPropWithNoGPIError {
  tag: "DeletedPropWithNoGPIError";
  subObj: BindingForm<A>;
  field: Identifier<A>;
  property: Identifier<A>;
  path: Path<A>;
}

export interface DeletedNonexistentFieldError {
  tag: "DeletedNonexistentFieldError";
  subObj: BindingForm<A>;
  field: Identifier<A>;
  path: Path<A>;
}

export interface DeletedVectorElemError {
  tag: "DeletedVectorElemError";
  path: Path<A>;
}

export interface InsertedPathWithoutOverrideError {
  tag: "InsertedPathWithoutOverrideError";
  path: Path<A>;
}

export interface InsertedPropWithNoFieldError {
  tag: "InsertedPropWithNoFieldError";
  subObj: BindingForm<A>;
  field: Identifier<A>;
  property: Identifier<A>;
  path: Path<A>;
}

export interface InsertedPropWithNoGPIError {
  tag: "InsertedPropWithNoGPIError";
  subObj: BindingForm<A>;
  field: Identifier<A>;
  property: Identifier<A>;
  path: Path<A>;
}

//#region Translation validation errors

export interface NonexistentNameError {
  tag: "NonexistentNameError";
  name: Identifier<A>;
  path: Path<A>;
}

export interface NonexistentFieldError {
  tag: "NonexistentFieldError";
  field: Identifier<A>;
  path: Path<A>;
}

export interface NonexistentGPIError {
  tag: "NonexistentGPIError";
  gpi: Identifier<A>;
  path: Path<A>;
}

export interface NonexistentPropertyError {
  tag: "NonexistentPropertyError";
  property: Identifier<A>;
  path: Path<A>;
}

export interface ExpectedGPIGotFieldError {
  tag: "ExpectedGPIGotFieldError";
  field: Identifier<A>;
  path: Path<A>;
}

export interface InvalidAccessPathError {
  tag: "InvalidAccessPathError";
  path: Path<A>;
}

export interface CanvasNonexistentError {
  tag: "CanvasNonexistentError";
}

export interface CanvasNonexistentDimsError {
  tag: "CanvasNonexistentDimsError";
  attr: "width" | "height";
  kind: "missing" | "GPI" | "uninitialized" | "wrong type";
  type?: string;
}

//#endregion Translation validation errors

// TODO(errors): use identifiers here
export interface RuntimeValueTypeError {
  tag: "RuntimeValueTypeError";
  path: Path<A>;
  expectedType: string;
  actualType: string;
}

//#endregion Style errors
