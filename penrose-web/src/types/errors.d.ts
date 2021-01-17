//#region ErrorTypes

// type PenroseError = LanguageError | RuntimeError;
// type LanguageError = DomainError | SubstanceError | StyleError | PluginError;
// type RuntimeError = OptimizerError | EvaluatorError;
// type StyleError = StyleParseError | StyleCheckError | TranslationError;
type PenroseError =
  | (DomainError & { type: "DomainError" })
  | (SubstanceError & { type: "SubstanceError" });

// TODO: does type var ever appear in Substance? If not, can we encode that at the type level?
type SubstanceError = DuplicateName | TypeNotFound | TypeVarNotFound;

type DomainError =
  | TypeDeclared
  | TypeVarNotFound
  | TypeNotFound
  | DuplicateName
  | CyclicSubtypes
  | NotTypeConsInSubtype
  | NotTypeConsInPrelude;

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
interface StyleError {
  sources: ErrorSource[];
  message: FormatString; // Style matched failed with Substance object $1 and select in Style $2
}

// NOTE: aliased to ASTNode for now, can include more types for different errors
type ErrorSource = ASTNode;

//#endregion
