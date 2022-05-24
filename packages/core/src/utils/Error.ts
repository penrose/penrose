import { isConcrete } from "engine/EngineUtils";
import { shapedefs } from "shapes/Shapes";
import { Result } from "true-myth";
import { A, AbstractNode, Identifier, SourceLoc } from "types/ast";
import { Arg, Type, TypeConstructor } from "types/domain";
import {
  ArgLengthMismatch,
  CyclicSubtypes,
  DeconstructNonconstructor,
  DomainError,
  DuplicateName,
  FatalError,
  NaNError,
  ParseError,
  PenroseError,
  RuntimeError,
  SelectorFieldNotSupported,
  StyleError,
  SubstanceError,
  TypeArgLengthMismatch,
  TypeMismatch,
  TypeNotFound,
  UnexpectedExprForNestedPred,
  VarNotFound,
} from "types/errors";
import { State } from "types/state";
import { BindingForm } from "types/style";
import { Deconstructor, SubExpr } from "types/substance";
import { prettyPrintPath } from "utils/Util";
const {
  or,
  and,
  ok,
  err,
  andThen,
  match,
  ap,
  unsafelyUnwrap,
  isErr,
  unsafelyGetErr,
} = Result;

// #region error rendering and construction

/**
 * Type pretty printing function.
 * @param t Type to be printed
 */
export const showType = (t: Type<A>): string => {
  if (t.tag === "Prop") {
    return "Prop";
  } else if (t.tag === "TypeVar") {
    return `'${t.name.value}`;
  } else {
    const { name, args } = t;
    if (args.length > 0) {
      const argStrs = args.map(showType);
      return `${name.value}(${argStrs.join(", ")})`;
    } else return `${name.value}`;
  }
};
// COMBAK What's a better way to model warnings?
export const styWarnings = [
  "DeletedPropWithNoSubObjError",
  "DeletedPropWithNoFieldError",
  "DeletedPropWithNoGPIError",
  "DeletedNonexistentFieldError",
];

// TODO: fix template formatting
export const showError = (
  error: DomainError | SubstanceError | StyleError | RuntimeError
): string => {
  switch (error.tag) {
    case "RuntimeError": {
      return error.message;
    }
    case "NaNError": {
      return `NaN encountered during optimization: ${error.message}`;
    }
    case "ParseError":
      return error.message;
    case "TypeDeclared": {
      return `Type ${error.typeName.value} already exists.`;
    }
    // TODO: abstract out this pattern if it becomes more common
    case "VarNotFound": {
      const { variable, possibleVars } = error;
      const msg = `Variable ${variable.value} (at ${loc(
        variable
      )}) does not exist.`;
      if (possibleVars) {
        const suggestions = possibleVars.map((v) => v.value).join(", ");
        return msg + ` Declared variables are: ${suggestions}`;
      } else return msg;
    }
    case "TypeNotFound": {
      const { typeName, possibleTypes } = error;
      const msg = `Type ${typeName.value} (at ${loc(
        typeName
      )}) does not exist.`;
      if (possibleTypes) {
        const suggestions = possibleTypes
          .map(({ value }: Identifier<A>) => value)
          .join(", ");
        return msg + ` Possible types are: ${suggestions}`;
      } else return msg;
    }
    case "TypeVarNotFound": {
      return `Type variable ${error.typeVar.name.value} (at ${loc(
        error.typeVar
      )}) does not exist.`;
    }
    case "DuplicateName": {
      const { firstDefined, name, location } = error;
      return `Name ${name.value} (at ${loc(
        location
      )}) already exists, first declared at ${loc(firstDefined)}.`;
    }
    case "CyclicSubtypes": {
      return `Subtyping relations in this program form a cycle. Cycles of types are:\n${showCycles(
        error.cycles
      )}`;
    }
    case "TypeMismatch": {
      const { sourceExpr, sourceType, expectedExpr, expectedType } = error;
      return `The type of the expression at ${loc(sourceExpr)} '${showType(
        sourceType
      )}' does not match with the expected type '${showType(
        expectedType
      )}' derived from the expression at ${loc(expectedExpr)}.`;
    }
    case "TypeArgLengthMismatch": {
      const { sourceExpr, sourceType, expectedExpr, expectedType } = error;
      return `${expectedType.args.length} arguments expected for type ${
        expectedType.name.value
      } (defined at ${loc(expectedExpr)}), but ${
        sourceType.args.length
      } arguments were given for ${sourceType.name.value} at ${loc(
        sourceExpr
      )} `;
    }
    case "ArgLengthMismatch": {
      const { name, argsGiven, argsExpected, sourceExpr, expectedExpr } = error;
      return `${name.value} expects ${
        argsExpected.length
      } arguments (originally defined at ${loc(expectedExpr)}), but was given ${
        argsGiven.length
      } arguments instead at ${loc(sourceExpr)}.`;
    }
    case "DeconstructNonconstructor": {
      const { variable, field } = error.deconstructor;
      return `Becuase ${variable.value} is not bound to a constructor, ${
        variable.value
      }.${field.value} (at ${loc(
        error.deconstructor
      )}) does not correspond to a field value.`;
    }
    case "UnexpectedExprForNestedPred": {
      const { sourceExpr, sourceType, expectedExpr } = error;
      return `A nested predicate is expected (defined at ${loc(
        expectedExpr
      )}), but an expression of '${showType(
        sourceType
      )}' type was given at ${loc(sourceExpr)}.`;
    }

    // ---- BEGIN STYLE ERRORS
    // COMBAK suggest improvements after reporting errors

    case "GenericStyleError": {
      return `DEBUG: Style failed with errors:\n ${error.messages.join("\n")}`;
    }

    case "StyleErrorList": {
      return error.errors.map(showError).join(", ");
    }

    case "SelectorVarMultipleDecl": {
      return `Style pattern statement has already declared the variable ${error.varName.contents.value}`;
    }

    case "SelectorFieldNotSupported": {
      return `Cannot match on field ${error.name.contents.value}.${
        error.field.value
      } (${loc(
        error.field
      )}) because matching on fields is not fully supported. Currently, only "label" can be matched in selectors.`;
    }

    case "SelectorDeclTypeMismatch": {
      // COMBAK: Add code for prettyprinting types
      return "Mismatched types or wrong subtypes between Substance and Style variables in selector";
    }

    case "SelectorRelTypeMismatch": {
      // COMBAK: Add code for prettyprinting types
      return "Mismatched types or wrong subtypes between variable and expression in relational statement in selector";
    }

    case "TaggedSubstanceError": {
      return showError(error.error); // Substance error
    }

    // --- BEGIN BLOCK STATIC ERRORS

    case "InvalidGPITypeError": {
      const shapeNames: string[] = Object.keys(shapedefs);
      return `Got invalid GPI type ${error.givenType.value}. Available shape types: ${shapeNames}`;
    }

    case "InvalidGPIPropertyError": {
      return `Got invalid GPI property ${error.givenProperty.value}. Available properties: ${error.expectedProperties}`;
    }

    case "InvalidFunctionNameError": {
      return `Got invalid Function name '${error.givenName.value}'.`;
      // COMBAK: Function suggestions, or just print the list
    }

    case "InvalidObjectiveNameError": {
      return `Got invalid objective name '${error.givenName.value}'.`;
      // COMBAK: Objective suggestions, or just print the list
    }

    case "InvalidConstraintNameError": {
      return `Got invalid constraint name '${error.givenName.value}'.`;
      // COMBAK: Constraint suggestions, or just print the list
    }

    // --- END BLOCK STATIC ERRORS

    // TODO(errors): use identifiers here
    case "RuntimeValueTypeError": {
      return `Runtime type error in looking up path '${prettyPrintPath(
        error.path
      )}''s value in translation. Expected type: '${
        error.expectedType
      }]. Got type: ]${error.actualType}].`;
    }

    // ----- END STYLE ERRORS

    case "Fatal": {
      return `FATAL: ${error.message}`;
    }

    default: {
      return `Meta: Cannot render error with contents: ${JSON.stringify(
        error
      )}`;
    }
  }
};

const showCycles = (cycles: string[][]) => {
  const pathString = (path: string[]) => path.join(" -> ");
  return cycles.map(pathString).join("\n");
};

export const cyclicSubtypes = (cycles: string[][]): CyclicSubtypes => ({
  tag: "CyclicSubtypes",
  cycles,
});

// action constructors for error
export const duplicateName = (
  name: Identifier<A>,
  location: AbstractNode,
  firstDefined: AbstractNode
): DuplicateName => ({
  tag: "DuplicateName",
  name,
  location,
  firstDefined,
});

export const typeNotFound = (
  typeName: Identifier<A>,
  possibleTypes?: Identifier<A>[]
): TypeNotFound => ({
  tag: "TypeNotFound",
  typeName,
  possibleTypes,
});

export const varNotFound = (
  variable: Identifier<A>,
  possibleVars?: Identifier<A>[]
): VarNotFound => ({
  tag: "VarNotFound",
  variable,
  possibleVars,
});

export const typeMismatch = (
  sourceType: TypeConstructor<A>,
  expectedType: TypeConstructor<A>,
  sourceExpr: AbstractNode,
  expectedExpr: AbstractNode
): TypeMismatch => ({
  tag: "TypeMismatch",
  sourceExpr,
  sourceType,
  expectedExpr,
  expectedType,
});

export const unexpectedExprForNestedPred = (
  sourceType: TypeConstructor<A>,
  sourceExpr: AbstractNode,
  expectedExpr: AbstractNode
): UnexpectedExprForNestedPred => ({
  tag: "UnexpectedExprForNestedPred",
  sourceType,
  sourceExpr,
  expectedExpr,
});

export const argLengthMismatch = (
  name: Identifier<A>,
  argsGiven: SubExpr<A>[],
  argsExpected: Arg<A>[],
  sourceExpr: AbstractNode,
  expectedExpr: AbstractNode
): ArgLengthMismatch => ({
  tag: "ArgLengthMismatch",
  name,
  argsGiven,
  argsExpected,
  sourceExpr,
  expectedExpr,
});

export const typeArgLengthMismatch = (
  sourceType: TypeConstructor<A>,
  expectedType: TypeConstructor<A>,
  sourceExpr: AbstractNode,
  expectedExpr: AbstractNode
): TypeArgLengthMismatch => ({
  tag: "TypeArgLengthMismatch",
  sourceExpr,
  sourceType,
  expectedExpr,
  expectedType,
});

export const selectorFieldNotSupported = (
  name: BindingForm<A>,
  field: Identifier<A>
): SelectorFieldNotSupported => ({
  tag: "SelectorFieldNotSupported",
  name,
  field,
});

export const deconstructNonconstructor = (
  deconstructor: Deconstructor<A>
): DeconstructNonconstructor => ({
  tag: "DeconstructNonconstructor",
  deconstructor,
});

export const fatalError = (message: string): FatalError => ({
  tag: "Fatal",
  message,
});

export const parseError = (
  message: string,
  location?: SourceLoc
): ParseError => ({
  tag: "ParseError",
  message,
  location,
});

export const nanError = (message: string, lastState: State): NaNError => ({
  tag: "NaNError",
  message,
  lastState,
});

// If there are multiple errors, just return the tag of the first one
export const toStyleErrors = (errors: StyleError[]): PenroseError => {
  if (!errors.length) {
    throw Error("internal error: expected at least one Style error");
  }

  return {
    errorType: "StyleError",
    tag: "StyleErrorList",
    errors,
  };
};

export const genericStyleError = (messages: StyleError[]): PenroseError => ({
  errorType: "StyleError",
  tag: "GenericStyleError",
  messages: messages.map(showError),
});

// const loc = (node: ASTNode) => `${node.start.line}:${node.start.col}`;
// TODO: Show file name
const loc = (node: AbstractNode): string => {
  if (isConcrete(node)) {
    return `line ${node.start.line}, column ${node.start.col + 1} of ${
      node.nodeType
    } program`;
  } else {
    return `generated code by the compiler`; // TODO: better description of where the node is coming from
  }
};

// #endregion

// #region Either monad

export const every = <Ok, Error>(
  ...results: Result<Ok, Error>[]
): Result<Ok, Error> =>
  results.reduce(
    (currentResult: Result<Ok, Error>, nextResult: Result<Ok, Error>) =>
      and(nextResult, currentResult),
    results[0] // TODO: separate out this element in argument
  );

export const safeChain = <Item, Ok, Error>(
  itemList: Item[],
  func: (nextItem: Item, currentResult: Ok) => Result<Ok, Error>,
  initialResult: Result<Ok, Error>
): Result<Ok, Error> =>
  itemList.reduce(
    (currentResult: Result<Ok, Error>, nextItem: Item) =>
      andThen((res: Ok) => func(nextItem, res), currentResult),
    initialResult
  );

export const all = <Ok, Error>(
  results: Result<Ok, Error>[]
): Result<Ok[], Error[]> => {
  const oks = [];
  const errs = [];
  for (const res of results) {
    if (res.isOk()) {
      oks.push(res.value);
    } else {
      errs.push(res.error);
    }
  }
  if (errs.length > 0) {
    return err(errs);
  }
  return ok(oks);
};

// NOTE: re-export all true-myth types to reduce boilerplate
export {
  Result,
  and,
  or,
  ok,
  err,
  andThen,
  ap,
  match,
  unsafelyUnwrap,
  isErr,
  unsafelyGetErr,
};

// #endregion
