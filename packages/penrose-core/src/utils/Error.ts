import { showType } from "compiler/Domain";
import { prettyPrintPath } from "utils/OtherUtils";
import { Maybe, Result } from "true-myth";
const { or, and, ok, err, andThen, match, ap, unsafelyUnwrap, isErr, unsafelyGetErr } = Result;

// #region error rendering and construction

// COMBAK What's a better way to model warnings?
export const styWarnings = [
  "DeletedPropWithNoSubObjError",
  "DeletedPropWithNoFieldError",
  "DeletedPropWithNoGPIError",
  "DeletedNonexistentFieldError",
];

// TODO: fix template formatting
export const showError = (
  error: DomainError | SubstanceError | StyleError
): string => {
  switch (error.tag) {
    case "GenericStyleError": {
      return `DEBUG: Style failed with errors:\n ${error.messages.join("\n")}`;
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
        const suggestions = possibleVars.join(", ");
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
          .map(({ value }: Identifier) => value)
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
    case "NotTypeConsInSubtype": {
      if (error.type.tag === "Prop") {
        return `Prop (at ${loc(
          error.type
        )}) is not a type constructor. Only type constructors are allowed in subtyping relations.`;
      } else {
        return `${error.type.name.value} (at ${loc(
          error.type
        )}) is not a type constructor. Only type constructors are allowed in subtyping relations.`;
      }
    }
    case "CyclicSubtypes": {
      return `Subtyping relations in this program form a cycle. Cycles of types are:\n${showCycles(
        error.cycles
      )}`;
    }
    case "NotTypeConsInPrelude": {
      if (error.type.tag === "Prop") {
        return `Prop (at ${loc(
          error.type
        )}) is not a type constructor. Only type constructors are allowed for prelude values.`;
      } else {
        return `${error.type.name.value} (at ${loc(
          error.type
        )}) is not a type constructor. Only type constructors are allowed in prelude values.`;
      }
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

    case "SelectorDeclTypeError": {
      // COMBAK Maybe this should be a TaggedSubstanceError?
      return "Substance type error in declaration in selector";
    }

    case "SelectorVarMultipleDecl": {
      return "Style pattern statement has already declared the variable ${error.varName.value}";
    }

    case "SelectorDeclTypeMismatch": {
      // COMBAK: Add code for prettyprinting types
      return "Mismatched types or wrong subtypes between Substance and Style variables in selector";
    };

    case "SelectorRelTypeMismatch": {
      // COMBAK: Add code for prettyprinting types
      return "Mismatched types or wrong subtypes between variable and expression in relational statement in selector";
    };

    case "TaggedSubstanceError": {
      return showError(error.error); // Substance error
    };

    case "DeletedPropWithNoSubObjError": {
      return `Sub obj '${error.subObj.contents.value}' has no fields; can't delete path '${prettyPrintPath(error.path)}'`;
    };

    case "DeletedPropWithNoFieldError": {
      return `Sub obj '${error.subObj.contents.value}' already lacks field ${error.field.value}; can't delete path '${prettyPrintPath(error.path)}'`;
    };

    case "CircularPathAlias": {
      return `Path ${prettyPrintPath(error.path)} was aliased to itself`;
    };

    case "DeletedPropWithNoGPIError": {
      return `Sub obj '${error.subObj.contents.value}' does not have GPI '${error.field.value}'; cannot delete property '${error.property.value} in ${prettyPrintPath(error.path)}'`;
    };

    // TODO: Use input path to report location?
    case "DeletedNonexistentFieldError": {
      return `Trying to delete '${error.field.value} from SubObj '${error.subObj.contents.value}', which already lacks the field`;
    };

    case "DeletedVectorElemError": {
      return `Cannot delete an element of a vector: ${prettyPrintPath(error.path)}`;
    };

    case "InsertedPathWithoutOverrideError": {
      return `Overriding path ${prettyPrintPath(error.path)} without override flag set`;
    };

    case "InsertedPropWithNoFieldError": {
      return `Sub obj '${error.subObj.contents.value}' does not have Field '${error.field.value}'; cannot add property '${error.property.value} in ${prettyPrintPath(error.path)}'`;
    };

    case "InsertedPropWithNoGPIError": {
      return `Sub obj '${error.subObj.contents.value}' has field but does not have GPI '${error.field.value}'; cannot add property '${error.property.value} in ${prettyPrintPath(error.path)}'. Expected GPI.`;
    };

    // TODO(errors): use identifiers here
    case "RuntimeValueTypeError": {
      return `Runtime type error in looking up path '${prettyPrintPath(error.path)}''s value in translation. Expected type: ${error.expectedType}. Got type: ${error.actualType}.`;
    };

    // ----- END STYLE ERRORS

    case "Fatal": {
      return `FATAL: ${error.message}`;
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

export const notTypeConsInPrelude = (
  type: Prop | TypeVar
): NotTypeConsInPrelude => ({
  tag: "NotTypeConsInPrelude",
  type,
});

export const notTypeConsInSubtype = (
  type: Prop | TypeVar
): NotTypeConsInSubtype => ({
  tag: "NotTypeConsInSubtype",
  type,
});

// action constructors for error
export const duplicateName = (
  name: Identifier,
  location: ASTNode,
  firstDefined: ASTNode
): DuplicateName => ({
  tag: "DuplicateName",
  name,
  location,
  firstDefined,
});

export const typeNotFound = (
  typeName: Identifier,
  possibleTypes?: Identifier[]
): TypeNotFound => ({
  tag: "TypeNotFound",
  typeName,
  possibleTypes,
});

export const varNotFound = (
  variable: Identifier,
  possibleVars?: string[]
): VarNotFound => ({
  tag: "VarNotFound",
  variable,
  possibleVars,
});

export const typeMismatch = (
  sourceType: TypeConstructor,
  expectedType: TypeConstructor,
  sourceExpr: ASTNode,
  expectedExpr: ASTNode
): TypeMismatch => ({
  tag: "TypeMismatch",
  sourceExpr,
  sourceType,
  expectedExpr,
  expectedType,
});

export const unexpectedExprForNestedPred = (
  sourceType: TypeConstructor,
  sourceExpr: ASTNode,
  expectedExpr: ASTNode
): UnexpectedExprForNestedPred => ({
  tag: "UnexpectedExprForNestedPred",
  sourceType,
  sourceExpr,
  expectedExpr,
});

export const argLengthMismatch = (
  name: Identifier,
  argsGiven: SubExpr[],
  argsExpected: Arg[],
  sourceExpr: ASTNode,
  expectedExpr: ASTNode
): ArgLengthMismatch => ({
  tag: "ArgLengthMismatch",
  name,
  argsGiven,
  argsExpected,
  sourceExpr,
  expectedExpr,
});

export const typeArgLengthMismatch = (
  sourceType: TypeConstructor,
  expectedType: TypeConstructor,
  sourceExpr: ASTNode,
  expectedExpr: ASTNode
): TypeArgLengthMismatch => ({
  tag: "TypeArgLengthMismatch",
  sourceExpr,
  sourceType,
  expectedExpr,
  expectedType,
});

export const deconstructNonconstructor = (
  deconstructor: Deconstructor
): DeconstructNonconstructor => ({
  tag: "DeconstructNonconstructor",
  deconstructor,
});

export const fatalError = (message: string): FatalError => ({
  tag: "Fatal",
  message,
});

export const parseError = (message: string): ParseError => ({
  tag: "ParseError",
  message,
});

// If there are multiple errors, just return the tag of the first one
export const firstStyleError = (messages: StyleError[]): PenroseError => {
  if (!messages.length) {
    throw Error("internal error: expected at least one Style error");
  }

  return {
    errorType: "StyleError",
    tag: messages[0].tag, // COMBAK: is this confusing for showError?
    messages: messages.map(showError),
  }
};

export const genericStyleError = (messages: StyleError[]): PenroseError => ({
  errorType: "StyleError",
  tag: "GenericStyleError",
  messages: messages.map(showError),
});

// const loc = (node: ASTNode) => `${node.start.line}:${node.start.col}`;
// TODO: Show file name
const loc = (node: ASTNode) =>
  `line ${node.start.line}, column ${node.start.col + 1} of ${
  node.nodeType
  } program`;

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

/**
 * Hand-written version of `Result.all`.
 */
export const all = <Ok, Error>(
  results: Result<Ok, Error>[]
): Result<Ok[], Error> => {
  return results.reduce(
    (
      currentResults: Result<Ok[], Error>,
      nextResult: Result<Ok, Error>
    ): Result<Ok[], Error> =>
      currentResults.match({
        Ok: (current: Ok[]) =>
          nextResult.match({
            Ok: (next: Ok) => ok([...current, next]),
            Err: (e: Error) => err(e),
          }),
        Err: (e: Error) => err(e),
      }),
    ok([])
  );
};

// NOTE: re-export all true-myth types to reduce boilerplate
export {
  Maybe,
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
  unsafelyGetErr
};

// #endregion
