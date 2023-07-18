import { Result } from "true-myth";
import { isConcrete } from "../engine/EngineUtils.js";
import { shapeTypes } from "../shapes/Shapes.js";
import * as ad from "../types/ad.js";
import {
  A,
  AbstractNode,
  C,
  Identifier,
  NodeType,
  SourceLoc,
  SourceRange,
} from "../types/ast.js";
import { Arg, Type, TypeConstructor } from "../types/domain.js";
import {
  ArgLengthMismatch,
  BadArgumentTypeError,
  BadShapeParamTypeError,
  CyclicSubtypes,
  DeconstructNonconstructor,
  DomainError,
  DuplicateName,
  FatalError,
  FunctionInternalError,
  InvalidColorLiteral,
  MissingArgumentError,
  NaNError,
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
  TypeArgLengthMismatch,
  TypeMismatch,
  TypeNotFound,
  UnexpectedCollectionAccessError,
  UnexpectedExprForNestedPred,
  VarNotFound,
} from "../types/errors.js";
import {
  CompFunc,
  ConstrFunc,
  FuncParam,
  ObjFunc,
} from "../types/functions.js";
import { State } from "../types/state.js";
import { BindingForm, ColorLit } from "../types/style.js";
import { Deconstructor, SubExpr } from "../types/substance.js";
import { ArgVal, ArgValWithSourceLoc, ShapeVal, Val } from "../types/value.js";
import {
  ErrorLoc,
  describeType,
  locOrNone,
  prettyPrintPath,
  prettyPrintResolvedPath,
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
      return `Type ${error.typeName.value} already exists.`;
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
    case "TypeVarNotFound": {
      return `Type variable ${error.typeVar.name.value} (at ${loc(
        error.typeVar,
      )}) does not exist.`;
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
    case "TypeMismatch": {
      const { sourceExpr, sourceType, expectedExpr, expectedType } = error;
      return `The type of the expression at ${loc(sourceExpr)} '${showType(
        sourceType,
      )}' does not match with the expected type '${showType(
        expectedType,
      )}' derived from the expression at ${loc(expectedExpr)}.`;
    }
    case "TypeArgLengthMismatch": {
      const { sourceExpr, sourceType, expectedExpr, expectedType } = error;
      return `${expectedType.args.length} arguments expected for type ${
        expectedType.name.value
      } (defined at ${loc(expectedExpr)}), but ${
        sourceType.args.length
      } arguments were given for ${sourceType.name.value} at ${loc(
        sourceExpr,
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
      return `Because ${variable.value} is not bound to a constructor, ${
        variable.value
      }.${field.value} (at ${loc(
        error.deconstructor,
      )}) does not correspond to a field value.`;
    }
    case "UnexpectedExprForNestedPred": {
      const { sourceExpr, sourceType, expectedExpr } = error;
      return `A nested predicate is expected (defined at ${loc(
        expectedExpr,
      )}), but an expression of '${showType(
        sourceType,
      )}' type was given at ${loc(sourceExpr)}.`;
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

    case "SelectorDeclTypeMismatch": {
      // COMBAK: Add code for prettyprinting types
      return "Mismatched types or wrong subtypes between Substance and Style variables in selector";
    }

    case "SelectorRelTypeMismatch": {
      // COMBAK: Add code for prettyprinting types
      return "Mismatched types or wrong subtypes between variable and expression in relational statement in selector";
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
      return `Cannot directly assign to or delete an index ${prettyPrintPath(
        error.path,
      )} of a larger structure (at ${loc(error.path)}).`;
    }

    case "AssignGlobalError": {
      return `Cannot assign to global ${prettyPrintResolvedPath(
        error.path,
      )} (at ${locc(
        "Style",
        error.path,
      )}); instead, just assign to ${error.path.members
        .map((id) => id.value)
        .join(".")} inside the ${error.path.name} namespace.`;
    }

    case "AssignSubstanceError": {
      return `Cannot assign to Substance object ${prettyPrintResolvedPath(
        error.path,
      )} (at ${locc("Style", error.path)}).`;
    }

    case "BadElementError": {
      if (error.coll.tag === "CollectionAccess") {
        const preamble = `The collection access (at ${locc(
          "Style",
          error.coll,
        )}) failed`;
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
      return `Cannot delete global ${prettyPrintResolvedPath(
        error.path,
      )} (at ${locc("Style", error.path)}).`;
    }

    case "DeleteSubstanceError": {
      return `Cannot delete Substance object ${prettyPrintResolvedPath(
        error.path,
      )} (at ${locc("Style", error.path)}).`;
    }

    case "MissingPathError": {
      return `Could not find ${prettyPrintResolvedPath(error.path)} (at ${locc(
        "Style",
        error.path,
      )}).`;
    }

    case "MissingShapeError": {
      return `Expected to find shape already defined to hold property ${prettyPrintResolvedPath(
        error.path,
      )} (at ${locc("Style", error.path)}), found nothing.`;
    }

    case "NestedShapeError": {
      return `Cannot define shape (at ${loc(
        error.expr,
      )}) within another shape.`;
    }

    case "NotCollError": {
      return `Cannot index into a non-collection (at ${loc(error.expr)}).`;
    }

    case "IndexIntoShapeListError": {
      return `Cannot index into a list of shapes (at ${loc(error.expr)}).`;
    }

    case "NotShapeError": {
      return `Expected to find shape to hold property ${prettyPrintResolvedPath(
        error.path,
      )} (at ${locc("Style", error.path)}), found ${error.what}.`;
    }

    case "NotValueError": {
      return `Expected value (at ${loc(error.expr)}), found ${
        error.what ?? "shape"
      }.`;
    }

    case "OutOfBoundsError": {
      return `Indices ${error.indices
        .map((i) => `[${i}]`)
        .join("")} of path ${prettyPrintPath(
        error.expr,
      )} out of bounds (at ${loc(error.expr)}).`;
    }

    case "PropertyMemberError": {
      return `Cannot assign to member ${prettyPrintResolvedPath(
        error.path,
      )} of a property (at ${locc("Style", error.path)}).`;
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
        ? `Passthrough shape property ${error.path}`
        : `Shape property ${error.path}`;
      return `${propertyClause} ${expectedClause} and ${doesNotAcceptClause}.`;
    }

    case "BadArgumentTypeError": {
      const { funcName, funcArg, provided } = error;

      const { name: argName, type: expectedType } = funcArg;
      const locStr = locc("Style", provided);
      const strExpectedType = describeType(expectedType).symbol;
      const strActualType = showArgValType(provided);

      return `Parameter \`${argName}\` (at ${locStr}) of function \`${funcName}\` expects ${strExpectedType} but is given incompatible ${strActualType}.`;
    }

    case "MissingArgumentError": {
      const { funcName, funcArg, funcLocation } = error;
      const locStr = locc("Style", funcLocation);

      const { name } = funcArg;

      return `Parameter \`${name}\` of function \`${funcName}\` (at ${locStr}) is missing without default value.`;
    }

    case "TooManyArgumentsError": {
      const { func, funcLocation, numProvided } = error;
      const locStr = locc("Style", funcLocation);

      const expNum = func.params.length;

      return `Function \`${func.name}\` (at ${locStr}) takes at most ${expNum} arguments but is provided with ${numProvided} arguments`;
    }

    case "FunctionInternalError": {
      const { func, location, message } = error;
      const locStr = locc("Style", location);
      return `Function \`${func.name}\` (at ${locStr}) failed with message: ${message}`;
    }
    case "RedeclareNamespaceError": {
      return `Namespace ${
        error.existingNamespace
      } already exists and is redeclared in ${locc("Style", error.location)}.`;
    }

    case "UnexpectedCollectionAccessError": {
      const { name, location } = error;
      const locStr = locc("Style", location);
      return `Style variable \`${name}\` cannot be accessed via the collection access operator (at ${locStr}) because it is not a collection.`;
    }

    case "LayerOnNonShapesError": {
      const { location, expr } = error;
      const locStr = locc("Style", location);
      return `Expects \`${expr}\` (at ${locStr}) to be a shape, but provided with a non-shape.`;
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
      return `Implicitly overriding ${prettyPrintResolvedPath(
        error.path,
      )} (at ${locc("Style", error.path)}).`;
    }

    case "NoopDeleteWarning": {
      return `Deleting nonexistent ${prettyPrintResolvedPath(
        error.path,
      )} (at ${locc("Style", error.path)}).`;
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
      const loc =
        topItem.location === undefined
          ? ""
          : `(at ${locc("Style", topItem.location)}) `;
      const topStr = `Function call ${topItem.signature} ${loc}uses bounding box approximations`;
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
      return [toErrorLoc(e.color)];
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
    case "TypeVarNotFound": {
      return locOrNone(e.typeVar);
    }
    case "DuplicateName": {
      return locOrNone(e.name);
    }
    case "CyclicSubtypes": {
      return [];
    }
    case "SymmetricTypeMismatch":
    case "SymmetricArgLengthMismatch":
    case "TypeMismatch":
    case "TypeArgLengthMismatch":
    case "ArgLengthMismatch":
    case "UnexpectedExprForNestedPred": {
      return locOrNone(e.sourceExpr);
    }
    case "DeconstructNonconstructor": {
      return locOrNone(e.deconstructor);
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

    case "SelectorDeclTypeMismatch": {
      // COMBAK: Add code for prettyprinting types
      return locOrNone(e.styType);
    }

    case "SelectorRelTypeMismatch": {
      // COMBAK: Add code for prettyprinting types
      return locOrNone(e.exprType);
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

    case "DeleteGlobalError":
    case "DeleteSubstanceError":
    case "MissingPathError":
    case "MissingShapeError":
    case "NotShapeError":
    case "PropertyMemberError":
    case "AssignGlobalError":
    case "AssignSubstanceError": {
      return locOrNone({ ...e.path, nodeType: "Style" });
    }

    case "NestedShapeError":
    case "NotCollError":
    case "IndexIntoShapeListError":
    case "NotValueError":
    case "OutOfBoundsError":
    case "UOpTypeError": {
      return locOrNone(e.expr);
    }

    case "BadShapeParamTypeError": {
      // TODO: incorporate location information in shape parameter errors
      return [];
    }

    case "BadArgumentTypeError": {
      return [
        toErrorLoc({
          nodeType: "Style",
          start: e.provided.start,
          end: e.provided.end,
        }),
      ];
    }

    case "MissingArgumentError": {
      return [
        toErrorLoc({
          ...e.funcLocation,
          nodeType: "Style",
        }),
      ];
    }

    case "TooManyArgumentsError": {
      return [
        toErrorLoc({
          ...e.funcLocation,
          nodeType: "Style",
        }),
      ];
    }

    case "FunctionInternalError":
    case "RedeclareNamespaceError":
    case "UnexpectedCollectionAccessError":
    case "LayerOnNonShapesError": {
      return [
        toErrorLoc({
          ...e.location,
          nodeType: "Style",
        }),
      ];
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
      const l = e.stack[e.stack.length - 1].location;
      return l === undefined ? [] : [toErrorLoc({ ...l, nodeType: "Style" })];
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
  sourceType: TypeConstructor<A>,
  expectedType: TypeConstructor<A>,
  sourceExpr: AbstractNode,
  expectedExpr: AbstractNode,
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
  expectedExpr: AbstractNode,
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
  expectedExpr: AbstractNode,
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
  expectedExpr: AbstractNode,
): TypeArgLengthMismatch => ({
  tag: "TypeArgLengthMismatch",
  sourceExpr,
  sourceType,
  expectedExpr,
  expectedType,
});

export const selectorFieldNotSupported = (
  name: BindingForm<A>,
  field: Identifier<A>,
): SelectorFieldNotSupported => ({
  tag: "SelectorFieldNotSupported",
  name,
  field,
});

export const deconstructNonconstructor = (
  deconstructor: Deconstructor<A>,
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
  location?: SourceLoc,
  fileType?: NodeType,
): ParseError => ({
  tag: "ParseError",
  message,
  location,
  fileType,
});

export const invalidColorLiteral = (
  color: ColorLit<C>,
): InvalidColorLiteral => ({
  tag: "InvalidColorLiteral",
  color,
});

export const badShapeParamTypeError = (
  path: string,
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
  funcArg: FuncParam,
  provided: ArgValWithSourceLoc<ad.Num>,
): BadArgumentTypeError => ({
  tag: "BadArgumentTypeError",
  funcName,
  funcArg,
  provided,
});

export const missingArgumentError = (
  funcName: string,
  funcArg: FuncParam,
  funcLocation: SourceRange,
): MissingArgumentError => ({
  tag: "MissingArgumentError",
  funcName,
  funcArg,
  funcLocation,
});

export const tooManyArgumentsError = (
  func: CompFunc | ObjFunc | ConstrFunc,
  funcLocation: SourceRange,
  numProvided: number,
): TooManyArgumentsError => ({
  tag: "TooManyArgumentsError",
  func,
  funcLocation,
  numProvided,
});

export const functionInternalError = (
  func: CompFunc | ObjFunc | ConstrFunc,
  location: SourceRange,
  message: string,
): FunctionInternalError => ({
  tag: "FunctionInternalError",
  func,
  location,
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

export const unexpectedCollectionAccessError = (
  name: string,
  location: SourceRange,
): UnexpectedCollectionAccessError => ({
  tag: "UnexpectedCollectionAccessError",
  name,
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

/* name stands for "`loc` concrete" */
const locc = (nodeType: NodeType, node: SourceRange): string => {
  return `line ${node.start.line}, column ${
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
