//#region ErrorTypes

// type PenroseError = LanguageError | RuntimeError;
// type LanguageError = DomainError | SubstanceError | StyleError | PluginError;
// type RuntimeError = OptimizerError | EvaluatorError;
// type StyleError = StyleParseError | StyleCheckError | TranslationError;
type PenroseError =
  | (DomainError & { errorType: "DomainError" })
  | (SubstanceError & { errorType: "SubstanceError" })
  | (StyleError & { errorType: "StyleError" });

// TODO: does type var ever appear in Substance? If not, can we encode that at the type level?
type SubstanceError =
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

type DomainError =
  | ParseError
  | TypeDeclared
  | TypeVarNotFound
  | TypeNotFound
  | DuplicateName
  | CyclicSubtypes
  | NotTypeConsInSubtype
  | NotTypeConsInPrelude;

interface UnexpectedExprForNestedPred {
  tag: "UnexpectedExprForNestedPred";
  sourceType: TypeConstructor;
  sourceExpr: ASTNode;
  expectedExpr: ASTNode;
}

interface CyclicSubtypes {
  tag: "CyclicSubtypes";
  cycles: string[][];
}
interface NotTypeConsInPrelude {
  tag: "NotTypeConsInPrelude";
  type: Prop | TypeVar;
}

interface NotTypeConsInSubtype {
  tag: "NotTypeConsInSubtype";
  type: Prop | TypeVar;
}
interface TypeDeclared {
  tag: "TypeDeclared";
  typeName: Identifier;
}
interface DuplicateName {
  tag: "DuplicateName";
  name: Identifier;
  location: ASTNode;
  firstDefined: ASTNode;
}
interface TypeVarNotFound {
  tag: "TypeVarNotFound";
  typeVar: TypeVar;
}
interface TypeNotFound {
  tag: "TypeNotFound";
  typeName: Identifier;
  possibleTypes?: Identifier[];
}
interface VarNotFound {
  tag: "VarNotFound";
  variable: Identifier;
  possibleVars?: string[]; // TODO: use Identifier type, but need to store them in env
}

interface TypeMismatch {
  tag: "TypeMismatch";
  sourceType: TypeConstructor;
  expectedType: TypeConstructor;
  sourceExpr: ASTNode;
  expectedExpr: ASTNode;
}
interface ArgLengthMismatch {
  tag: "ArgLengthMismatch";
  name: Identifier;
  argsGiven: SubExpr[];
  argsExpected: Arg[];
  sourceExpr: ASTNode;
  expectedExpr: ASTNode;
}

interface TypeArgLengthMismatch {
  tag: "TypeArgLengthMismatch";
  sourceType: TypeConstructor;
  expectedType: TypeConstructor;
  sourceExpr: ASTNode;
  expectedExpr: ASTNode;
}

interface DeconstructNonconstructor {
  tag: "DeconstructNonconstructor";
  deconstructor: Deconstructor;
}

// NOTE: for debugging purposes
interface FatalError {
  tag: "Fatal";
  message: string;
}

// NOTE: aliased to ASTNode for now, can include more types for different errors
type ErrorSource = ASTNode;

//#endregion

//#region Style errors

type StyleError =
  // Misc errors
  | ParseError
  | GenericStyleError
  | StyleErrorList
  // Selector errors (from Substance)
  | SelectorDeclTypeError
  | SelectorVarMultipleDecl
  | SelectorDeclTypeMismatch
  | SelectorRelTypeMismatch
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
  // Runtime errors
  | RuntimeValueTypeError;

type StyleWarning =
  | IntOrFloat;

type StyleWarnings = StyleWarning[];

interface StyleResults {
  errors: StyleErrors;
  warnings: StyleWarnings;
}

interface IntOrFloat {
  tag: "IntOrFloat";
  message: string;
}; // COMBAK: Use this in block checking

interface GenericStyleError {
  tag: "GenericStyleError";
  messages: string[];
}

interface StyleErrorList {
  tag: "StyleErrorList";
  errors: StyleError[];
}

interface ParseError {
  tag: "ParseError";
  message: string;
}

interface SelectorDeclTypeError {
  tag: "SelectorDeclTypeError";
  typeName: identifier;
}

interface SelectorVarMultipleDecl {
  tag: "SelectorVarMultipleDecl";
  varName: BindingForm;
}

interface SelectorDeclTypeMismatch {
  tag: "SelectorDeclTypeMismatch";
  subType: TypeConsApp;
  styType: TypeConsApp;
}

interface SelectorRelTypeMismatch {
  tag: "SelectorRelTypeMismatch";
  varType: TypeConsApp;
  exprType: TypeConsApp;
}

interface TaggedSubstanceError {
  tag: "TaggedSubstanceError";
  error: SubstanceError;
}

//#region Block statics

interface InvalidGPITypeError {
  tag: "InvalidGPITypeError";
  givenType: Identifier;
  // expectedType: string;
};

interface InvalidGPIPropertyError {
  tag: "InvalidGPIPropertyError";
  givenProperty: Identifier;
  // expectedProperty: string;
};

interface InvalidFunctionNameError {
  tag: "InvalidFunctionNameError";
  givenName: Identifier;
  // expectedName: string;
};

interface InvalidObjectiveNameError {
  tag: "InvalidObjectiveNameError";
  givenName: Identifier;
  // expectedName: string;
};

interface InvalidConstraintNameError {
  tag: "InvalidConstraintNameError";
  givenName: Identifier;
  // expectedName: string;
};

//#endregion Block statics

interface DeletedPropWithNoSubObjError {
  tag: "DeletedPropWithNoSubObjError";
  subObj: BindingForm;
  path: Path;
}

interface DeletedPropWithNoFieldError {
  tag: "DeletedPropWithNoFieldError";
  subObj: BindingForm;
  field: Identifier;
  path: Path;
}

interface CircularPathAlias {
  tag: "CircularPathAlias";
  path: Path;
}

interface DeletedPropWithNoGPIError {
  tag: "DeletedPropWithNoGPIError";
  subObj: BindingForm;
  field: Identifier;
  property: Identifier;
  path: Path;
}

interface DeletedNonexistentFieldError {
  tag: "DeletedNonexistentFieldError";
  subObj: BindingForm;
  field: Identifier;
  path: Path;
}

interface DeletedVectorElemError {
  tag: "DeletedVectorElemError";
  path: Path;
}

interface InsertedPathWithoutOverrideError {
  tag: "InsertedPathWithoutOverrideError";
  path: Path;
}

interface InsertedPropWithNoFieldError {
  tag: "InsertedPropWithNoFieldError";
  subObj: BindingForm;
  field: Identifier;
  property: Identifier;
  path: Path;
}

interface InsertedPropWithNoGPIError {
  tag: "InsertedPropWithNoGPIError";
  subObj: BindingForm;
  field: Identifier;
  property: Identifier;
  path: Path;
}

//#region Translation validation errors

interface NonexistentNameError {
  tag: "NonexistentNameError";
  name: Identifier;
  path: Path;
};

interface NonexistentFieldError {
  tag: "NonexistentFieldError";
  field: Identifier;
  path: Path;
};

interface NonexistentGPIError {
  tag: "NonexistentGPIError";
  gpi: Identifier;
  path: Path;
};

interface NonexistentPropertyError {
  tag: "NonexistentPropertyError";
  property: Identifier;
  path: Path;
};

interface ExpectedGPIGotFieldError {
  tag: "ExpectedGPIGotFieldError";
  field: Identifier;
  path: Path;
};

interface InvalidAccessPathError {
  tag: "InvalidAccessPathError";
  path: Path;
};

//#endregion Translation validation errors

// TODO(errors): use identifiers here
interface RuntimeValueTypeError {
  tag: "RuntimeValueTypeError";
  path: Path;
  expectedType: string;
  actualType: string;
}

//#endregion Style errors
