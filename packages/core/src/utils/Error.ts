import { Result } from "true-myth";
import { isConcrete } from "../engine/EngineUtils.js";
import { shapeTypes } from "../shapes/Shapes.js";
import * as ad from "../types/ad.js";
import {
  A,
  AbstractNode,
  Identifier,
  NodeType,
  SourceLoc,
  SourceRange,
} from "../types/ast.js";
import { Arg, Type } from "../types/domain.js";
import {
  ArgLengthMismatch,
  BadArgumentTypeError,
  BadShapeParamTypeError,
  CyclicSubtypes,
  DomainError,
  DuplicateName,
  FatalError,
  FunctionInternalError,
  InvalidColorLiteral,
  MissingArgumentError,
  NaNError,
  NotStyleVariableError,
  NotSubstanceCollectionError,
  ParseError,
  PenroseError,
  RedeclareNamespaceError,
  RuntimeError,
  SelectorFieldNotSupported,
  StyleError,
  StyleErrorList,
  StyleWarning,
  SubstanceError,
  SymmetricArgLengthMismatch,
  SymmetricTypeMismatch,
  TooManyArgumentsError,
  TypeDeclared,
  TypeMismatch,
  TypeNotFound,
  VarNotFound,
} from "../types/errors.js";
import { FuncParam, FuncSignature } from "../types/functions.js";
import { State } from "../types/state.js";
import { BindingForm, ColorLit } from "../types/style.js";
import {
  ResolvedExpr,
  ResolvedPath,
  StylePath,
  StylePathToUnindexedObject,
} from "../types/stylePathResolution.js";
import { SubExpr, TypeApp } from "../types/substance.js";
import { ArgVal, ArgValWithExpr, ShapeVal, Val } from "../types/value.js";
import {
  ErrorLoc,
  describeType,
  locOrNone,
  prettyPrintPath,
  prettyResolvedExpr,
  prettyResolvedStylePath,
  subObjectToUniqueName,
  toErrorLoc,
} from "./Util.js";
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

const showArgValType = (v: ArgVal<ad.Num>): string => {
  if (v.tag === "ShapeVal") {
    return `${v.contents.shapeType}Shape`;
  } else {
    const start = `${v.contents.tag} value`;
    if (
      v.contents.tag === "ListV" ||
      v.contents.tag === "VectorV" ||
      v.contents.tag === "TupV"
    ) {
      return start + ` with ${v.contents.contents.length} elements`;
    } else if (
      v.contents.tag === "PtListV" ||
      v.contents.tag === "MatrixV" ||
      v.contents.tag === "LListV"
    ) {
      const { contents } = v.contents;
      const rowLengths = contents.map((row) => row.length);
      if (rowLengths.every((l) => l === rowLengths[0])) {
        return start + ` of shape ${contents.length}-by-${rowLengths[0]}`;
      } else {
        return start + ` of nonrectangular shape`;
      }
    } else if (v.contents.tag === "StrV") {
      return start + ` with content "${v.contents.contents}"`;
    } else {
      return `${v.contents.tag} value`;
    }
  }
};

/**
 * Type pretty printing function.
 * @param t Type to be printed
 */
export const showType = (t: Type<A> | TypeApp<A>): string => {
  return t.name.value;
};
// COMBAK What's a better way to model warnings?
export const styWarnings = [
  "DeletedPropWithNoSubObjError",
  "DeletedPropWithNoFieldError",
  "DeletedPropWithNoGPIError",
  "DeletedNonexistentFieldError",
];

const showPath = (p: StylePath<A>) => prettyResolvedStylePath(p, true);

// TODO: fix template formatting
export const showError = (
  error:
    | DomainError
    | SubstanceError
    | StyleError
    | StyleWarning
    | RuntimeError,
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
    case "InvalidColorLiteral":
      return `${error.color.contents} (at ${loc(
        error.color,
      )}) is not a valid color literal. Color literals must be one of the following formats: #RGB, #RGBA, #RRGGBB, #RRGGBBAA.`;
    case "TypeDeclared": {
      const { typeName, firstDefined } = error;
      if (firstDefined.nodeType === "BuiltinDomain") {
        return `Type ${typeName.value} (at ${loc(
          typeName,
        )}) already exists as a built-in type. Choose a different name for your type to avoid name conflicts.`;
      } else {
        return `Type ${typeName.value} (at ${loc(
          typeName,
        )}) already exists, first declared at ${loc(firstDefined)}.`;
      }
    }
    // TODO: abstract out this pattern if it becomes more common
    case "VarNotFound": {
      const { variable, possibleVars } = error;
      const msg = `Variable ${variable.value} (at ${loc(
        variable,
      )}) does not exist.`;
      if (possibleVars) {
        const suggestions = possibleVars.map((v) => v.value).join(", ");
        return msg + ` Declared variables are: ${suggestions}`;
      } else return msg;
    }
    case "TypeNotFound": {
      const { typeName, possibleTypes } = error;
      const msg = `Type ${typeName.value} (at ${loc(
        typeName,
      )}) does not exist.`;
      if (possibleTypes) {
        const suggestions = possibleTypes
          .map(({ value }: Identifier<A>) => value)
          .join(", ");
        return msg + ` Possible types are: ${suggestions}`;
      } else return msg;
    }
    case "DuplicateName": {
      const { firstDefined, name, location } = error;
      return `Name ${name.value} (at ${loc(
        location,
      )}) already exists, first declared at ${loc(firstDefined)}.`;
    }
    case "CyclicSubtypes": {
      return `Subtyping relations in this program form a cycle. Cycles of types are:\n${showCycles(
        error.cycles,
      )}`;
    }
    case "SymmetricTypeMismatch": {
      const { sourceExpr } = error;
      return `The symmetric predicate at ${loc(
        sourceExpr,
      )} must have arguments all of the same type.`;
    }
    case "SymmetricArgLengthMismatch": {
      const { sourceExpr } = error;
      return `The symmetric predicate at ${loc(
        sourceExpr,
      )} must only have two arguments.`;
    }
    case "OutputLiteralTypeError": {
      const { type, location } = error;
      return `The type ${type.name.value} (at ${loc(
        location,
      )}) is a built-in literal type that cannot be used as outputs of functions and constructors.`;
    }
    case "SubOrSuperLiteralTypeError": {
      const { type, location } = error;
      return `The type ${type.name.value} (at ${loc(
        location,
      )}) is a built-in literal type that cannot be sub-typed or super-typed.`;
    }
    case "TypeMismatch": {
      const { sourceExpr, sourceType, expectedExpr, expectedType } = error;
      return `The type of the expression at ${loc(sourceExpr)} '${showType(
        sourceType,
      )}' does not match with the expected type '${showType(
        expectedType,
      )}' derived from the expression at ${loc(expectedExpr)}.`;
    }
    case "ArgLengthMismatch": {
      const { name, argsGiven, argsExpected, sourceExpr, expectedExpr } = error;
      return `${name.value} expects ${
        argsExpected.length
      } arguments (originally defined at ${loc(expectedExpr)}), but was given ${
        argsGiven.length
      } arguments instead at ${loc(sourceExpr)}.`;
    }
    case "InvalidSetIndexingError": {
      const { index, location, suggestions } = error;
      return `Name \`${index}\` (which is used at ${loc(
        location,
      )}) is not a valid index. Possible indices are: ${suggestions.join(
        ", ",
      )}`;
    }
    case "BadSetIndexRangeError": {
      const { index, location } = error;
      return `A indexed-set range must consist of integers, but value ${index} (at ${loc(
        location,
      )}) is not an integer.`;
    }
    case "DuplicateIndexError": {
      const { index, location } = error;
      return `Index variable \`${index}\` has been declared multiple times at ${loc(
        location,
      )}.`;
    }
    case "DivideByZeroError": {
      const { location } = error;
      return `The expression at ${loc(location)} resulted in division-by-zero.`;
    }
    case "InvalidArithmeticValueError": {
      const { value, location } = error;
      return `The expression at ${loc(
        location,
      )} resulted in the invalid value of ${value}.`;
    }
    case "UnsupportedIndexingError": {
      const { iset } = error;
      return `Indexing on expressions of type ${iset.stmt.tag} (at ${loc(
        iset,
      )}) is not supported`;
    }
    case "DeclLiteralError": {
      const { location, type } = error;
      return `Objects of built-in literal type ${type.name.value} (at ${loc(
        location,
      )}) cannot be declared explicitly; they can only be inferred from literal data in Substance.`;
    }

    // ---- BEGIN STYLE ERRORS
    // COMBAK suggest improvements after reporting errors

    case "GenericStyleError": {
      return `DEBUG: Style failed with errors:\n ${error.messages.join("\n")}`;
    }

    case "StyleErrorList": {
      return error.errors.map(showError).join("\n");
    }

    case "SelectorVarMultipleDecl": {
      return `Style pattern statement has already declared the variable ${error.varName.contents.value}`;
    }

    case "SelectorFieldNotSupported": {
      return `Cannot match on field ${error.name.contents.value}.${
        error.field.value
      } (${loc(
        error.field,
      )}) because matching on fields is not fully supported. Currently, only "label" can be matched in selectors.`;
    }

    case "TaggedSubstanceError": {
      switch (error.error.tag) {
        // special handling for VarNotFound
        case "VarNotFound": {
          const processIdentifier = (x: Identifier<A>) => ({
            ...x,
            value: x.nodeType === "Style" ? x.value : `\`${x.value}\``,
          });
          return showError({
            ...error.error,
            variable: processIdentifier(error.error.variable),
            possibleVars: error.error.possibleVars
              ? error.error.possibleVars.map(processIdentifier)
              : undefined,
          });
        }
        default:
          return showError(error.error); // Substance error
      }
    }

    case "SelectorAliasNamingError": {
      return `Incompatible alias name "${error.alias.value}" in Style selector: \
      Domain or Style pattern statement has already declared the variable ${error.alias.value}`;
    }

    case "MultipleLayoutError": {
      return `Multiple layout pipelines found in Style (${error.decls
        .map((d) => loc(d))
        .join("; ")}). There can only be one unique layout pipeline.`;
    }
    // --- BEGIN BLOCK STATIC ERRORS

    case "InvalidGPITypeError": {
      const shapeNames: string[] = shapeTypes;
      return `Got invalid GPI type ${error.givenType.value}. Available shape types: ${shapeNames}`;
    }

    case "InvalidGPIPropertyError": {
      return `Got invalid GPI property ${error.givenProperty.value} at ${loc(
        error.givenProperty,
      )}. Available properties: ${error.expectedProperties}`;
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

    // --- BEGIN COMPILATION ERRORS

    case "AssignAccessError": {
      return `Cannot directly assign to or delete an index ${showPath(
        error.path,
      )} of a larger structure (at ${loc(error.path)}).`;
    }

    case "AssignGlobalError": {
      const { path } = error;
      const pathStr = showPath(path);
      return `Cannot assign to a namespace ${pathStr} (at ${loc(
        path,
      )}); instead, assign to individual members of the namespace like ${pathStr}.member`;
    }

    case "AssignSubstanceError": {
      const { path } = error;
      return `Cannot assign to Substance object ${showPath(
        error.path,
      )} (at ${loc(path)}).`;
    }

    case "BadElementError": {
      const { coll } = error;
      if (error.coll.tag === "CollectionAccess") {
        const preamble = `The collection access (at ${loc(coll)}) failed`;
        if (error.index === 0) {
          return (
            preamble +
            ` because the collection contains elements that cannot be collected`
          );
        } else {
          return (
            preamble +
            ` because some elements of the collection (in particular, index ${error.index}) have different type from other elements.`
          );
        }
      } else {
        return `Wrong element type at index ${error.index} in ${
          error.coll.tag
        } (at ${loc(error.coll)}).`;
      }
    }

    case "BadIndexError": {
      return `Invalid indexing (at ${loc(error.expr)}).`;
    }

    case "UnindexableItemError": {
      return `The item \`${showPath(error.expr)}\` (at ${loc(
        error.expr,
      )}) is not indexable.`;
    }

    case "BinOpTypeError": {
      return `Unsupported binary operation ${error.expr.op} on types ${
        error.left
      } and ${error.right} (at ${loc(error.expr)}).`;
    }

    case "CanvasNonexistentDimsError": {
      switch (error.kind) {
        case "GPI":
          return `Canvas ${error.attr} must be a numeric literal, but it is a shape.`;
        case "missing":
          return `Canvas ${error.attr} is not defined.\nTry adding:
canvas {
    ${error.attr} = <my desired ${error.attr}>
}`;
        case "wrong type":
          return `Canvas ${error.attr} must be a numeric literal, but it has type ${error.type}.`;
      }
      break; // dead code to please ESLint
    }

    case "CyclicAssignmentError": {
      const cycleString = error.cycles.map((c) =>
        c.map(({ id, src }) =>
          src === undefined ? id : `${id} (${locc("Style", src)})`,
        ),
      );
      return `The Style program contains cyclic variable assignments, where the following variables are defined in cycles:\n${showCycles(
        cycleString,
      )}`;
    }

    case "DeleteGlobalError": {
      return `Cannot delete namespace ${showPath(error.path)} (at ${loc(
        error.path,
      )}).`;
    }

    case "DeleteSubstanceError": {
      const { path } = error;
      return `Cannot delete Substance object ${showPath(error.path)} (at ${loc(
        path,
      )}).`;
    }

    case "MissingPathError": {
      return `Could not find ${showPath(error.path)} (at ${loc(error.path)}).`;
    }

    case "UndeclaredSubVarError": {
      return `The substance object ${error.name.value} (at ${loc(
        error.name,
      )}) has not been declared in the Style block header.`;
    }

    case "PathToNamespaceError": {
      return `The expression ${error.path.name} (at ${loc(
        error.path,
      )}) refers to an entire namespace, which is not a value. Such a path cannot appear here. Instead, use a path that refers to a member of this namespace, e.g., \`${
        error.path.name
      }.field_name.\``;
    }

    case "PathToCollectionError": {
      return `The expression ${error.path.styleName} (at ${loc(
        error.path,
      )}) refers to a collection of non-literal values, which cannot appear here. Instead, access information about the collection using operators like \`listof ... from ${
        error.path.styleName
      }\` or \`numberof ${error.path.styleName}\``;
    }

    case "PathToSubstanceError": {
      return `The expression at ${loc(
        error.path,
      )} refers to a non-literal Substance object \`${subObjectToUniqueName(
        error.path.substanceObject,
      )}\`, which cannot appear here.`;
    }

    case "CollectionMemberAccessError": {
      const { path, field } = error;
      return `The expression ${showPath(path)} (at ${loc(
        path,
      )}) is a collection, and cannot be accessed with the dot-operator. To access field \`${field}\` of each member of the collection, use the collection-access expression \`listof ${field} from ${showPath(
        path,
      )}\`.`;
    }

    case "MissingShapeError": {
      const { path } = error;
      return `Path ${showPath(path)} (at ${loc(path)}) cannot be found.`;
    }

    case "NestedShapeError": {
      return `Cannot define shape (at ${loc(
        error.expr,
      )}) within another shape.`;
    }

    case "NotShapeError": {
      const { path, what } = error;
      const { parent } = path.access;
      return `Expected to find shape at path ${showPath(parent)} (at ${loc(
        parent,
      )}), but found ${what}.`;
    }

    case "NotValueError": {
      return `Expected value (at ${loc(error.expr)}), found ${
        error.what ?? "shape"
      }.`;
    }

    case "OutOfBoundsError": {
      return `Indices ${error.indices
        .map((i) => `[${i}]`)
        .join("")} of path ${showPath(
        error.expr.contents,
      )} out of bounds (at ${loc(error.expr)}).`;
    }

    case "UOpTypeError": {
      return `Unsupported unary operation ${error.expr.op} on type ${
        error.arg
      } (at ${loc(error.expr)}).`;
    }

    case "BadShapeParamTypeError": {
      const expectedType = error.expectedType;
      const expectedClause = `expects type ${expectedType}`;
      const doesNotAcceptClause =
        error.value.tag === "Val"
          ? `does not accept type ${error.value.contents.tag}`
          : `does not accept shape ${error.value.contents.shapeType}`;
      const propertyClause = error.passthrough
        ? `Passthrough shape property ${showPath(error.path)}`
        : `Shape property ${showPath(error.path)}`;
      return `${propertyClause} ${expectedClause} and ${doesNotAcceptClause}.`;
    }

    case "BadArgumentTypeError": {
      const { funcName, formalArg, actualArg } = error;

      const { name: argName, type: expectedType } = formalArg;
      const locStr = loc(actualArg.expr);
      const strExpectedType = describeType(expectedType).symbol;
      const strActualType = showArgValType(actualArg);

      return `Parameter \`${argName}\` (at ${locStr}) of function \`${funcName}\` expects ${strExpectedType} but is given incompatible ${strActualType}.`;
    }

    case "MissingArgumentError": {
      const { funcName, formalArg, callExpression } = error;
      const locStr = loc(callExpression);

      const { name } = formalArg;

      return `Parameter \`${name}\` of function \`${funcName}\` (at ${locStr}) is missing without default value.`;
    }

    case "TooManyArgumentsError": {
      const { func, callExpression, numProvided } = error;
      const locStr = loc(callExpression);

      const expNum = func.params.length;

      return `Function \`${func.name}\` (at ${locStr}) takes at most ${expNum} arguments but is provided with ${numProvided} arguments`;
    }

    case "FunctionInternalError": {
      const { func, callExpression, message } = error;
      const locStr = loc(callExpression);
      return `Function \`${func.name}\` (at ${locStr}) failed with message: ${message}`;
    }
    case "RedeclareNamespaceError": {
      return `Namespace ${
        error.existingNamespace
      } already exists and is redeclared in ${locc("Style", error.location)}.`;
    }

    case "NotSubstanceCollectionError": {
      const { path } = error;
      const locStr = loc(path);
      return `The expression ${showPath(
        path.contents,
      )} (at ${locStr}) is not a collection.`;
    }

    case "NotStyleVariableError": {
      const { path } = error;
      const locStr = loc(path);
      return `The expression ${showPath(
        path.contents,
      )} (at ${locStr}) is not a non-collection style variable.`;
    }

    case "LayerOnNonShapesError": {
      const { path } = error;
      const locStr = loc(path);
      return `Expects \`${prettyResolvedExpr(
        path,
      )}\` (at ${locStr}) to be a shape, but provided with a non-shape.`;
    }

    case "NonWellFormedPathError": {
      const { path } = error;
      return `Path ${showPath(path)} (at ${loc(path)}) is not a valid path.`;
    }

    // --- END COMPILATION ERRORS

    // TODO(errors): use identifiers here
    case "RuntimeValueTypeError": {
      return `Runtime type error in looking up path '${prettyPrintPath(
        error.path,
      )}''s value in translation. Expected type: '${
        error.expectedType
      }]. Got type: ]${error.actualType}].`;
    }

    // ----- END STYLE ERRORS

    // ---- BEGIN STYLE WARNINGS

    case "ImplicitOverrideWarning": {
      return `Implicitly overriding ${showPath(error.path)} (at ${loc(
        error.path,
      )}).`;
    }

    case "NoopDeleteWarning": {
      return `Deleting nonexistent ${showPath(error.path)} (at ${loc(
        error.path,
      )}).`;
    }

    case "LayerCycleWarning": {
      return `Cycles detected in layering order: ${error.cycles
        .map((c) => c.join(", "))
        .join(
          "; ",
        )}. The system approximated a global layering order instead: ${error.approxOrdering.join(
        ", ",
      )}`;
    }

    case "ShapeBelongsToMultipleGroups": {
      return `Shape ${
        error.shape
      } belongs to multiple groups: ${error.groups.join(", ")}`;
    }

    case "GroupCycleWarning": {
      return `Cycles detected in group memberships: ${error.cycles
        .map((c) => c.join(", "))
        .join("; ")}.`;
    }

    case "BBoxApproximationWarning": {
      const topItem = error.stack[error.stack.length - 1];
      const rest = error.stack.slice(0, -1).reverse();
      const location =
        topItem.callExpression === undefined ||
        !isConcrete(topItem.callExpression)
          ? ""
          : `(at ${loc(topItem.callExpression)}) `;
      const topStr = `Function call ${topItem.signature} ${location}uses bounding box approximations`;
      const restStrs = rest.map(
        (item) =>
          `- Function call ${item.signature} uses bounding box approximations`,
      );
      return [topStr, ...restStrs].join(", because\n");
    }

    // ----- END STYLE WARNINGS

    case "Fatal": {
      return `FATAL: ${error.message}`;
    }
  }
};

export const errLocs = (
  e: Exclude<
    DomainError | SubstanceError | StyleError | StyleWarning | RuntimeError,
    StyleErrorList
  >,
): ErrorLoc[] => {
  switch (e.tag) {
    case "RuntimeError":
    case "NaNError": {
      return [];
    }
    case "ParseError":
      if (e.fileType === undefined || e.location === undefined) {
        return [];
      } else {
        return [
          {
            type: e.fileType,
            range: {
              start: e.location,
              end: e.location,
            },
          },
        ];
      }
    case "InvalidColorLiteral":
      return locOrNone(e.color);
    case "TypeDeclared": {
      return locOrNone(e.typeName);
    }
    // TODO: abstract out this pattern if it becomes more common
    case "VarNotFound": {
      return locOrNone(e.variable);
    }
    case "TypeNotFound": {
      return locOrNone(e.typeName);
    }
    case "DuplicateName": {
      return locOrNone(e.location);
    }
    case "CyclicSubtypes": {
      return [];
    }
    case "SymmetricTypeMismatch":
    case "SymmetricArgLengthMismatch":
    case "TypeMismatch":
    case "ArgLengthMismatch": {
      return locOrNone(e.sourceExpr);
    }
    case "OutputLiteralTypeError":
    case "SubOrSuperLiteralTypeError": {
      return locOrNone(e.location);
    }
    case "InvalidSetIndexingError":
    case "BadSetIndexRangeError":
    case "DuplicateIndexError":
    case "DivideByZeroError":
    case "InvalidArithmeticValueError": {
      return locOrNone(e.location);
    }
    case "UnsupportedIndexingError": {
      return locOrNone(e.iset);
    }
    case "DeclLiteralError": {
      return locOrNone(e.location);
    }

    // ---- BEGIN STYLE ERRORS
    // COMBAK suggest improvements after reporting errors

    case "GenericStyleError": {
      return [];
    }

    case "SelectorVarMultipleDecl": {
      return locOrNone(e.varName);
    }

    case "SelectorFieldNotSupported": {
      return locOrNone(e.field);
    }

    case "TaggedSubstanceError": {
      switch (e.error.tag) {
        // special handling for VarNotFound
        case "VarNotFound":
          return locOrNone(e.error.variable);
        default:
          return errLocs(e.error);
      }
    }

    case "SelectorAliasNamingError": {
      return locOrNone(e.alias);
    }

    case "MultipleLayoutError": {
      return e.decls.map(locOrNone).flat();
    }
    // --- BEGIN BLOCK STATIC ERRORS
    case "InvalidGPIPropertyError": {
      return [];
    }
    case "InvalidGPITypeError": {
      return locOrNone(e.givenType);
    }
    case "InvalidFunctionNameError":
    case "InvalidObjectiveNameError":
    case "InvalidConstraintNameError": {
      return locOrNone(e.givenName);
    }

    // --- END BLOCK STATIC ERRORS

    // --- BEGIN COMPILATION ERRORS

    case "AssignAccessError": {
      return locOrNone(e.path);
    }

    case "BadElementError": {
      return locOrNone(e.coll);
    }

    case "BadIndexError":
    case "BinOpTypeError": {
      return locOrNone(e.expr);
    }

    case "CanvasNonexistentDimsError": {
      return [];
    }

    case "CyclicAssignmentError": {
      const cycleLocs = e.cycles
        .map((c) =>
          c
            .map(({ id, src }) =>
              src === undefined
                ? []
                : toErrorLoc({ ...src, nodeType: "Style" }),
            )
            .flat(),
        )
        .flat();
      return cycleLocs;
    }

    case "UndeclaredSubVarError":
      return locOrNone(e.name);

    case "DeleteGlobalError":
    case "DeleteSubstanceError":
    case "MissingPathError":
    case "MissingShapeError":
    case "NotShapeError":
    case "AssignGlobalError":
    case "AssignSubstanceError":
    case "NonWellFormedPathError":
    case "CollectionMemberAccessError":
    case "PathToCollectionError":
    case "PathToNamespaceError":
    case "PathToSubstanceError": {
      return locOrNone(e.path);
    }

    case "NestedShapeError":
    case "NotValueError":
    case "OutOfBoundsError":
    case "UOpTypeError":
    case "UnindexableItemError": {
      return locOrNone(e.expr);
    }

    case "BadShapeParamTypeError": {
      // TODO: incorporate location information in shape parameter errors
      return [];
    }

    case "BadArgumentTypeError": {
      return locOrNone(e.actualArg.expr);
    }

    case "MissingArgumentError":
    case "TooManyArgumentsError":
    case "FunctionInternalError": {
      return locOrNone(e.callExpression);
    }

    case "RedeclareNamespaceError": {
      return [toErrorLoc({ ...e.location, nodeType: "Style" })];
    }

    case "NotSubstanceCollectionError":
    case "NotStyleVariableError":
    case "LayerOnNonShapesError": {
      return locOrNone(e.path);
    }
    // --- END COMPILATION ERRORS

    // TODO(errors): use identifiers here
    case "RuntimeValueTypeError": {
      return locOrNone({ ...e.path, nodeType: "Style" });
    }

    // ----- END STYLE ERRORS

    // ---- BEGIN STYLE WARNINGS

    case "ImplicitOverrideWarning":
    case "NoopDeleteWarning": {
      return locOrNone({ ...e.path, nodeType: "Style" });
    }
    case "BBoxApproximationWarning": {
      const expr = e.stack[e.stack.length - 1].callExpression;
      if (expr === undefined) {
        return [];
      }
      return locOrNone(expr);
    }

    case "LayerCycleWarning":
    case "ShapeBelongsToMultipleGroups":
    case "GroupCycleWarning": {
      return [];
    }

    // ----- END STYLE WARNINGS

    case "Fatal": {
      return [];
    }
  }
};

const showCycles = (cycles: string[][]) => {
  // repeats the cycle start again
  const pathString = (path: string[]) => [...path, path[0]].join(" -> ");
  return cycles.map(pathString).join("\n");
};

export const cyclicSubtypes = (cycles: string[][]): CyclicSubtypes => ({
  tag: "CyclicSubtypes",
  cycles,
});

// action constructors for error
export const typeDeclared = (
  typeName: Identifier<A>,
  firstDefined: AbstractNode,
): TypeDeclared => ({
  tag: "TypeDeclared",
  typeName,
  firstDefined,
});

export const duplicateName = (
  name: Identifier<A>,
  location: AbstractNode,
  firstDefined: AbstractNode,
): DuplicateName => ({
  tag: "DuplicateName",
  name,
  location,
  firstDefined,
});

export const typeNotFound = (
  typeName: Identifier<A>,
  possibleTypes?: Identifier<A>[],
): TypeNotFound => ({
  tag: "TypeNotFound",
  typeName,
  possibleTypes,
});

export const varNotFound = (
  variable: Identifier<A>,
  possibleVars?: Identifier<A>[],
): VarNotFound => ({
  tag: "VarNotFound",
  variable,
  possibleVars,
});

export const symmetricTypeMismatch = (
  sourceExpr: AbstractNode,
): SymmetricTypeMismatch => ({
  tag: "SymmetricTypeMismatch",
  sourceExpr,
});

export const symmetricArgLengthMismatch = (
  sourceExpr: AbstractNode,
): SymmetricArgLengthMismatch => ({
  tag: "SymmetricArgLengthMismatch",
  sourceExpr,
});

export const typeMismatch = (
  sourceType: TypeApp<A>,
  expectedType: Type<A>,
  sourceExpr: AbstractNode,
  expectedExpr: AbstractNode,
): TypeMismatch => ({
  tag: "TypeMismatch",
  sourceExpr,
  sourceType,
  expectedExpr,
  expectedType,
});

export const argLengthMismatch = (
  name: Identifier<A>,
  argsGiven: SubExpr<A>[],
  argsExpected: Arg<A>[],
  sourceExpr: AbstractNode,
  expectedExpr: AbstractNode,
): ArgLengthMismatch => ({
  tag: "ArgLengthMismatch",
  name,
  argsGiven,
  argsExpected,
  sourceExpr,
  expectedExpr,
});

export const selectorFieldNotSupported = (
  name: BindingForm<A>,
  field: Identifier<A>,
): SelectorFieldNotSupported => ({
  tag: "SelectorFieldNotSupported",
  name,
  field,
});

export const fatalError = (message: string): FatalError => ({
  tag: "Fatal",
  message,
});

export const parseError = (
  message: string,
  location?: SourceLoc,
  fileType?: NodeType,
): ParseError => ({
  tag: "ParseError",
  message,
  location,
  fileType,
});

export const invalidColorLiteral = (
  color: ColorLit<A>,
): InvalidColorLiteral => ({
  tag: "InvalidColorLiteral",
  color,
});

export const badShapeParamTypeError = (
  path: StylePathToUnindexedObject<A>,
  value: Val<ad.Num> | ShapeVal<ad.Num>,
  expectedType: string,
  passthrough: boolean,
): BadShapeParamTypeError => ({
  tag: "BadShapeParamTypeError",
  path,
  value,
  expectedType,
  passthrough,
});

export const badArgumentTypeError = (
  funcName: string,
  formalArg: FuncParam,
  actualArg: ArgValWithExpr<ad.Num>,
): BadArgumentTypeError => ({
  tag: "BadArgumentTypeError",
  funcName,
  formalArg,
  actualArg,
});

export const missingArgumentError = (
  funcName: string,
  formalArg: FuncParam,
  callExpression: ResolvedExpr<A>,
): MissingArgumentError => ({
  tag: "MissingArgumentError",
  funcName,
  formalArg,
  callExpression,
});

export const tooManyArgumentsError = (
  func: FuncSignature,
  callExpression: ResolvedExpr<A>,
  numProvided: number,
): TooManyArgumentsError => ({
  tag: "TooManyArgumentsError",
  func,
  callExpression,
  numProvided,
});

export const functionInternalError = (
  func: FuncSignature,
  callExpression: ResolvedExpr<A>,
  message: string,
): FunctionInternalError => ({
  tag: "FunctionInternalError",
  func,
  callExpression,
  message,
});

export const redeclareNamespaceError = (
  existingNamespace: string,
  location: SourceRange,
): RedeclareNamespaceError => ({
  tag: "RedeclareNamespaceError",
  existingNamespace,
  location,
});

export const notSubstanceCollectionError = (
  path: ResolvedPath<A>,
): NotSubstanceCollectionError => ({
  tag: "NotSubstanceCollectionError",
  path,
});

export const notStyleVariableError = (
  path: ResolvedPath<A>,
): NotStyleVariableError => ({
  tag: "NotStyleVariableError",
  path,
});

export const nanError = (message: string, lastState: State): NaNError => ({
  tag: "NaNError",
  message,
  lastState,
});

export const runtimeError = (message: string): PenroseError => ({
  tag: "RuntimeError",
  errorType: "RuntimeError",
  message: message,
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

/* name stands for "`loc` concrete" */
const locc = (nodeType: NodeType, node: SourceRange): string => {
  // @ts-expect-error refactoring
  return `line ${node.start.line}, column ${
    // @ts-expect-error refactoring
    node.start.col + 1
  } of ${nodeType} program`;
};

// const aloc = (node: ASTNode) => `${node.start.line}:${node.start.col}`;
// TODO: Show file name
/* pretty-prints source location of an AST node */
const loc = (node: AbstractNode): string => {
  if (isConcrete(node)) {
    return locc(node.nodeType, node);
  } else {
    return `generated code by the compiler`; // TODO: better description of where the node is coming from
  }
};

export const isPenroseError = (error: unknown): error is PenroseError => {
  return (
    error instanceof Object &&
    "errorType" in error &&
    (error.errorType === "DomainError" ||
      error.errorType === "SubstanceError" ||
      error.errorType === "StyleError" ||
      error.errorType === "RuntimeError")
  );
};

// #endregion

// #region Either monad

export const every = <Ok, Error>(
  ...results: Result<Ok, Error>[]
): Result<Ok, Error> =>
  results.reduce(
    (currentResult: Result<Ok, Error>, nextResult: Result<Ok, Error>) =>
      and(nextResult, currentResult),
    results[0], // TODO: separate out this element in argument
  );

export const safeChain = <Item, Ok, Error>(
  itemList: Item[],
  func: (nextItem: Item, currentResult: Ok) => Result<Ok, Error>,
  initialResult: Result<Ok, Error>,
): Result<Ok, Error> =>
  itemList.reduce(
    (currentResult: Result<Ok, Error>, nextItem: Item) =>
      andThen((res: Ok) => func(nextItem, res), currentResult),
    initialResult,
  );

export const all = <Ok, Error>(
  results: Result<Ok, Error>[],
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
  andThen,
  ap,
  err,
  isErr,
  match,
  ok,
  or,
  unsafelyGetErr,
  unsafelyUnwrap,
};

// #endregion
