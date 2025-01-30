import { SyntaxNode, Tree } from "@lezer/common";
import im from "immutable";
import Result, { err, ok } from "true-myth/result";
import { parser } from "../parser/SubstanceParser.js";
import {
  A,
  ASTNode,
  AbstractNode,
  C,
  Identifier,
  StringLit,
  location,
} from "../types/ast.js";
import {
  Arg,
  ConstructorDecl,
  DomainEnv,
  FunctionDecl,
  Type,
} from "../types/domain.js";
import { ParseError, PenroseError, SubstanceError } from "../types/errors.js";
import {
  ApplyConstructor,
  ApplyFunction,
  ApplyPredicate,
  AutoLabel,
  Bind,
  BooleanExpr,
  CompiledSubProg,
  CompiledSubStmt,
  Decl,
  DeclBind,
  DeclList,
  Func,
  IndexSet,
  LabelDecl,
  LabelMap,
  LabelOption,
  LabelValue,
  LiteralSubExpr,
  NoLabel,
  NumExpr,
  NumberConstant,
  RangeAssign,
  Stmt,
  StmtSet,
  SubArgExpr,
  SubExpr,
  SubProg,
  SubStmt,
  SubstanceEnv,
  TypeApp,
} from "../types/substance.js";
import {
  all,
  argLengthMismatch,
  duplicateName,
  every,
  parseError,
  safeChain,
  typeMismatch,
  typeNotFound,
  varNotFound,
} from "../utils/Error.js";
import { cartesianProduct, toLiteralUniqueName, zip2 } from "../utils/Util.js";
import {
  checkType,
  isLiteralType,
  isSubtype,
  numberType,
  printNode,
  stringType,
  toDomType,
} from "./Domain.js";

const extractText = (src: string, to: number, from: number) =>
  src.slice(from, to);

export const parseSubstance = (
  prog: string,
): Result<SubProg<C>, ParseError> => {
  const res = parser.parse(prog);
  let errorNode: SyntaxNode | undefined;
  res.iterate({
    enter: (node) => {
      if (node.type.isError) errorNode = node.node;
    },
  });
  if (errorNode) {
    return err(
      parseError("error parsing Substance", errorNode.from, "Substance"),
    );
  }
  return validateSubstance(res, prog);
};

const validateSubstance = (
  ast: Tree,
  src: string,
): Result<SubProg<C>, ParseError> => {
  const stmts = [];
  if (ast.topNode.firstChild !== null) {
    const cursor = ast.topNode.firstChild.cursor();
    do {
      const nodeType = cursor.node.type.name;
      if (nodeType === "LineComment" || nodeType === "BlockComment") continue;
      stmts.push(cursor.node);
    } while (cursor.nextSibling());
  }

  const stmtResults = stmts.map((node) => validateStmt(node, src));

  const stmtsResult = all(stmtResults);

  if (stmtsResult.isOk) {
    return ok({
      tag: "SubProg",
      statements: stmtsResult.value,
      start: 0,
      end: ast.length,
      nodeType: "Substance",
    });
  }
  return err(stmtsResult.error[0]);
};

const validateStmt = (
  node: SyntaxNode,
  src: string,
): Result<SubStmt<C>, ParseError> => {
  const stmt = node;

  switch (stmt.type.name) {
    case "TypeApp":
      return validateTypeApp(stmt, src);
    case "PredicateApp":
      return validatePredicate(stmt, src);
    case "Fn_ConsApp":
      return validateFnConsApp(stmt, src);
    case "Labeling":
      return validateLabeling(stmt, src);
    default:
      return err(
        parseError(
          `Unknown statement type: ${stmt.type.name}`,
          stmt.from,
          "Substance",
        ),
      );
  }
};

const validateTypeApp = (
  node: SyntaxNode,
  src: string,
): Result<Decl<C> | DeclList<C> | StmtSet<C>, ParseError> => {
  const type = node.getChild("NamedId");
  const ids = node.getChildren("Identifier");
  const iset = node.getChild("IndexedStatement");

  if (!type || ids.length === 0) {
    return err(parseError("Invalid type application", node.from, "Substance"));
  }

  const typeApp: TypeApp<C> = {
    tag: "TypeApp",
    name: validateID(type.firstChild!, src),
    ...meta(node),
  };

  // Create base statement (Decl or DeclList)
  const baseStmt: Decl<C> | DeclList<C> =
    ids.length === 1
      ? {
          tag: "Decl",
          type: typeApp,
          name: validateID(ids[0], src),
          ...meta(node),
        }
      : {
          tag: "DeclList",
          type: typeApp,
          names: ids.map((id) => validateID(id, src)),
          ...meta(node),
        };

  // If there's an index set, wrap in StmtSet
  if (iset) {
    const indexStmt = validateIndexSet(iset, src);
    if (indexStmt.isErr) {
      return err(indexStmt.error);
    }
    return ok({
      tag: "StmtSet",
      stmt: baseStmt,
      iset: indexStmt.value,
      ...meta(node),
    });
  }

  return ok(baseStmt);
};

// Helper to validate index set
const validateIndexSet = (
  node: SyntaxNode,
  src: string,
): Result<IndexSet<C>, ParseError> => {
  console.log(printNode(node, src));

  const condition = node.getChild("BooleanExpression");

  // Parse ranges
  const indices: RangeAssign<C>[] = [];
  const cursor = node.cursor();

  do {
    if (cursor.type.name === "Identifier") {
      const variable = validateID(cursor.node, src);
      cursor.next(); // Skip 'in'
      cursor.next(); // Move to Range

      const range = validateRange(cursor.node, src);
      if (range.isErr) return range;

      indices.push({
        tag: "RangeAssign",
        variable,
        range: range.value,
        ...meta(cursor.node),
      });
    }
  } while (cursor.nextSibling());

  // Parse condition if present
  let boolExpr: BooleanExpr<C> | undefined;
  if (condition) {
    const expr = validateBooleanExpr(condition, src);
    if (expr.isErr) return err(expr.error);
    boolExpr = expr.value;
  }

  return ok({
    tag: "IndexSet",
    indices,
    condition: boolExpr,
    ...meta(node),
  });
};

// You'll need these helper functions as well:
const validateRange = (
  node: SyntaxNode,
  src: string,
): Result<Range<C>, ParseError> => {
  const nums = node.getChildren("Number");
  if (nums.length !== 2) {
    return err(parseError("Invalid range", node.from, "Substance"));
  }

  return ok({
    tag: "Range",
    low: validateNumber(nums[0], src),
    high: validateNumber(nums[1], src),
    ...meta(node),
  });
};

const validateNumber = (node: SyntaxNode, src: string): NumberConstant<C> => ({
  tag: "NumberConstant",
  contents: Number(extractText(src, node.to, node.from)),
  ...meta(node),
});

const validateBooleanExpr = (
  node: SyntaxNode,
  src: string,
): Result<BooleanExpr<C>, ParseError> => {
  // Base case: true/false literals
  if (node.type.name === "Boolean") {
    return ok({
      tag: "BooleanConstant",
      value: extractText(src, node.to, node.from) === "true",
      ...meta(node),
    });
  }

  // Comparison operator between numeric expressions
  if (node.getChild("CompareOp")) {
    const operator = node.getChild("CompareOp");
    const left = node.getChild("NumericExpression", null, "CompareOp");
    const right = node.getChild("NumericExpression", "CompareOp");

    if (!left || !right || !operator) {
      return err(
        parseError("Invalid comparison expression", node.from, "Substance"),
      );
    }

    const leftExpr = validateNumericExpr(left, src);
    if (leftExpr.isErr) return err(leftExpr.error);

    const rightExpr = validateNumericExpr(right, src);
    if (rightExpr.isErr) return err(rightExpr.error);

    return ok({
      tag: "ComparisonExpr",
      operator: extractText(src, operator.to, operator.from) as
        | "=="
        | "!="
        | "<"
        | "<="
        | ">"
        | ">=",
      left: leftExpr.value,
      right: rightExpr.value,
      ...meta(node),
    });
  }

  // Boolean operators
  const boolOp = node.getChild("BoolOp");
  if (boolOp) {
    const operator = extractText(src, boolOp.to, boolOp.from);

    // Unary operator (!)
    if (operator === "!") {
      const arg = node.getChild("BooleanExpression");
      if (!arg) {
        return err(
          parseError(
            "Invalid unary boolean expression",
            node.from,
            "Substance",
          ),
        );
      }
      const argExpr = validateBooleanExpr(arg, src);
      if (argExpr.isErr) return argExpr;

      return ok({
        tag: "UnaryBooleanExpr",
        operator: "!",
        arg: argExpr.value,
        ...meta(node),
      });
    }

    // Binary operators (&& and ||)
    const left = node.getChild("BooleanExpression");
    const right = node.getChild("BooleanExpression", left?.to);
    if (!left || !right) {
      return err(
        parseError("Invalid binary boolean expression", node.from, "Substance"),
      );
    }

    const leftExpr = validateBooleanExpr(left, src);
    if (leftExpr.isErr) return leftExpr;

    const rightExpr = validateBooleanExpr(right, src);
    if (rightExpr.isErr) return rightExpr;

    return ok({
      tag: "BinaryBooleanExpr",
      operator: operator as "&&" | "||",
      left: leftExpr.value,
      right: rightExpr.value,
      ...meta(node),
    });
  }

  return err(parseError("Invalid boolean expression", node.from, "Substance"));
};

const validateNumericExpr = (
  node: SyntaxNode,
  src: string,
): Result<NumExpr<C>, ParseError> => {
  // If there's only one child, it's an id or number
  const child = node.firstChild;
  if (child && !child.nextSibling) {
    // Handle identifiers
    if (child.type.name === "Identifier") {
      return ok(validateID(child, src));
    }
    // Handle number literals
    else if (child.type.name === "Number") {
      return ok(validateNumber(child, src));
    } else {
      return err(
        parseError("Invalid numeric expression", node.from, "Substance"),
      );
    }
  }

  // Handle arithmetic operators
  const arithOp = node.getChild("ArithOp");
  if (arithOp) {
    const operator = extractText(src, arithOp.to, arithOp.from);

    // Unary minus
    if (operator === "-" && node.getChild("NumericExpression")) {
      const arg = node.getChild("NumericExpression");
      if (!arg) {
        return err(
          parseError("Invalid unary expression", node.from, "Substance"),
        );
      }
      const argExpr = validateNumericExpr(arg, src);
      if (argExpr.isErr) return argExpr;

      return ok({
        tag: "UnaryExpr",
        operator: "-",
        arg: argExpr.value,
        ...meta(node),
      });
    }

    // Binary operators

    const left = node.getChild("NumericExpression");
    const right = node.getChild("NumericExpression", "ArithOp");
    if (!left || !right) {
      return err(
        parseError("Invalid binary expression", node.from, "Substance"),
      );
    }

    const leftExpr = validateNumericExpr(left, src);
    if (leftExpr.isErr) return leftExpr;

    const rightExpr = validateNumericExpr(right, src);
    if (rightExpr.isErr) return rightExpr;

    return ok({
      tag: "BinaryExpr",
      operator: operator as "+" | "-" | "*" | "/" | "%" | "^",
      left: leftExpr.value,
      right: rightExpr.value,
      ...meta(node),
    });
  }

  return err(parseError("Invalid numeric expression", node.from, "Substance"));
};

// You'll also need validateBooleanExpr() to handle conditions,
// but that's a larger piece that would need to be implemented separately

const validatePredicate = (
  node: SyntaxNode,
  src: string,
): Result<ApplyPredicate<C>, ParseError> => {
  const name = node.getChild("NamedId");
  const args = node.getChild("ArgList");

  if (!name || !args) {
    return err(parseError("Invalid predicate", node.from, "Substance"));
  }

  return ok({
    tag: "ApplyPredicate",
    name: validateID(name.firstChild!, src),
    args: validateArgs(args, src),
    ...meta(node),
  });
};

const validateFnConsApp = (
  node: SyntaxNode,
  src: string,
): Result<Bind<C> | DeclBind<C>, ParseError> => {
  const let_ = node.getChild("Let");
  const typeNode = node.getChild("NamedId", null, "Assignment");
  const varNode = node.getChild("Identifier");
  const fnName = node.getChild("NamedId", "Assignment");
  const args = node.getChild("ArgList");

  if (!varNode || !fnName || !args) {
    return err(
      parseError(
        "Invalid function/constructor application",
        node.from,
        "Substance",
      ),
    );
  }

  const func: Func<C> = {
    tag: "Func",
    name: validateID(fnName.firstChild!, src),
    args: validateArgs(args, src),
    ...meta(node),
  };

  if (typeNode && !let_) {
    // Type declaration + binding
    return ok({
      tag: "DeclBind",
      type: {
        tag: "TypeApp",
        name: validateID(typeNode.firstChild!, src),
        ...meta(typeNode),
      },
      variable: validateID(varNode, src),
      expr: func,
      ...meta(node),
    });
  }

  // Simple binding
  return ok({
    tag: "Bind",
    variable: validateID(varNode, src),
    expr: func,
    ...meta(node),
  });
};

const validateLabeling = (
  node: SyntaxNode,
  src: string,
): Result<LabelDecl<C> | AutoLabel<C> | NoLabel<C>, ParseError> => {
  const autoLabel = node.getChild("AutoLabel");
  const label = node.getChild("Label");
  const noLabel = node.getChild("NoLabel");

  if (autoLabel) {
    const all = node.getChild("All");
    const ids = node.getChildren("Identifier");

    const option: LabelOption<C> = all
      ? { tag: "DefaultLabels", ...meta(all) }
      : {
          tag: "LabelIDs",
          variables: ids.map((id) => validateID(id, src)),
          ...meta(node),
        };

    return ok({
      tag: "AutoLabel",
      option,
      ...meta(node),
    });
  } else if (label) {
    const str = node.getChild("String");
    const tex = node.getChild("TeX");
    const identifier = node.getChild("Identifier");

    if (!(str || tex) || !identifier) {
      return err(
        parseError("Invalid label declaration", node.from, "Substance"),
      );
    }
    const labelNode = (str || tex) as SyntaxNode;
    const labelStr = extractText(src, labelNode.to, labelNode.from);
    return ok({
      tag: "LabelDecl",
      variable: validateID(identifier, src),
      label: {
        tag: "StringLit",
        type: "string",
        contents: labelStr.slice(1, -1), // Remove first and last characters (the $ signs)
        ...meta(labelNode),
      },
      labelType: tex ? "MathLabel" : "TextLabel",
      ...meta(node),
    });
  } else if (noLabel) {
    const ids = node.getChildren("Identifier");
    return ok({
      tag: "NoLabel",
      args: ids.map((id) => validateID(id, src)),
      ...meta(node),
    });
  }

  return err(parseError("Invalid labeling statement", node.from, "Substance"));
};

const validateArgs = (node: SyntaxNode, src: string): SubArgExpr<C>[] => {
  const args: SubArgExpr<C>[] = [];

  // Handle each child of the ArgList
  for (let child = node.firstChild; child; child = child.nextSibling) {
    switch (child.type.name) {
      case "Identifier":
        args.push(validateID(child, src));
        break;

      case "String": {
        const literal: StringLit<C> = {
          tag: "StringLit",
          contents: extractText(src, child.to, child.from).slice(1, -1), // Remove quotes
          ...meta(child),
        };
        args.push({
          tag: "LiteralSubExpr",
          contents: literal,
          ...meta(child),
        });
        break;
      }

      case "Number": {
        const num = Number(extractText(src, child.to, child.from));
        const numberLiteral: NumberConstant<C> = {
          tag: "NumberConstant",
          contents: num,
          ...meta(child),
        };
        args.push({
          tag: "LiteralSubExpr",
          contents: numberLiteral,
          ...meta(child),
        });
        break;
      }

      case "ArithOp": {
        // Handle negative numbers
        if (child.nextSibling?.type.name === "Number") {
          const num = -Number(
            extractText(src, child.nextSibling.to, child.nextSibling.from),
          );
          const numberLiteral: NumberConstant<C> = {
            tag: "NumberConstant",
            contents: num,
            ...meta(child),
          };
          args.push({
            tag: "LiteralSubExpr",
            contents: numberLiteral,
            ...meta(child),
          });
          child = child.nextSibling; // Skip the number since we processed it
        }
        break;
      }

      case ",": // Skip commas
        break;

      default:
        // Skip other tokens like parentheses
        break;
    }
  }

  return args;
};

const validateID = (node: SyntaxNode, src: string): Identifier<C> => ({
  tag: "Identifier",
  type: "identifier",
  value: extractText(src, node.to, node.from),
  ...meta(node),
});

const meta = (
  node: SyntaxNode,
): { start: number; end: number; nodeType: "Substance" } => ({
  start: node.from,
  end: node.to,
  nodeType: "Substance",
});

export const compileSubstance = (
  prog: string,
  domEnv: DomainEnv,
): Result<SubstanceEnv, PenroseError> => {
  const astOk = parseSubstance(prog);
  if (astOk.isOk) {
    const ast = astOk.value;
    // prepare Substance env
    const subEnv = initSubstanceEnv();
    // check the substance ast and produce an env or report errors
    const checkerOk = checkSubstance(ast, domEnv, subEnv);
    return checkerOk.match({
      Ok: ({ subEnv, contents: ast }) =>
        ok(postprocessSubstance(domEnv, { ...subEnv, ast })),
      Err: (e) => err({ ...e[0], errorType: "SubstanceError" }),
    });
  } else {
    return err({ ...astOk.error, errorType: "SubstanceError" });
  }
};

export const initSubstanceEnv = (): SubstanceEnv => ({
  labels: im.Map<string, LabelValue>(),
  objs: im.Map<string, Type<C>>(),
  objIds: [],
  literals: [],
  ast: { tag: "SubProg", nodeType: "Substance", statements: [] },
});

//#region Postprocessing

const EMPTY_LABEL: LabelValue = { value: "", type: "NoLabel" };

// recreate all literals as plain objects
// create default labels
// process labeling directives
export const postprocessSubstance = (
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): SubstanceEnv => {
  subEnv.labels = im.Map(
    [...subEnv.objs.keys()].map((id) => [id, EMPTY_LABEL]),
  );
  return subEnv.ast.statements.reduce(
    (subEnv, stmt: CompiledSubStmt<A>) =>
      processLabelStmt(stmt, domEnv, subEnv),
    subEnv,
  );
};

const processLabelStmt = (
  stmt: SubStmt<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): SubstanceEnv => {
  switch (stmt.tag) {
    case "AutoLabel": {
      if (stmt.option.tag === "DefaultLabels") {
        const ids = subEnv.objIds;
        const newLabels: LabelMap = im.Map(
          ids.map((id) => [id.value, { value: id.value, type: "MathLabel" }]),
        );

        return {
          ...subEnv,
          labels: newLabels,
        };
      } else {
        const ids = stmt.option.variables;
        const newLabels: LabelMap = subEnv.labels.merge(
          im.Map(
            ids.map((id) => [id.value, { value: id.value, type: "MathLabel" }]),
          ),
        );
        return {
          ...subEnv,
          labels: newLabels,
        };
      }
    }
    case "LabelDecl": {
      const { variable, label, labelType } = stmt;
      return {
        ...subEnv,
        labels: subEnv.labels.set(variable.value, {
          value: label.contents,
          type: labelType,
        }),
      };
    }
    case "NoLabel": {
      const ids = stmt.args;
      const newLabels: LabelMap = subEnv.labels.merge(
        ids.map((id) => [id.value, EMPTY_LABEL]),
      );
      return {
        ...subEnv,
        labels: newLabels,
      };
    }
    default:
      return subEnv;
  }
};

//#endregion

//#region Semantic checker

interface WithEnv<T> {
  subEnv: SubstanceEnv;
  contents: T;
}
interface WithEnvAndType<T> {
  subEnv: SubstanceEnv;
  contents: T;
  type: Type<A>;
}
type CheckerResult<T> = Result<WithEnv<T>, SubstanceError[]>;
type ResultWithType<T> = Result<WithEnvAndType<T>, SubstanceError[]>;

/**
 * Top-level function for the Substance semantic checker. Given a Substance AST and an initial context, it outputs either a `SubstanceError` or an `Env` context.
 *
 * @param prog compiled AST of a Domain program
 * @param domEnv  environment from the Domain checker
 */
export const checkSubstance = (
  prog: SubProg<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<CompiledSubProg<A>> => {
  const { statements } = prog;
  // check all statements
  const contents: CompiledSubStmt<A>[] = [];
  const stmtsOk: CheckerResult<CompiledSubStmt<A>[]> = safeChain(
    statements,
    (stmt, { subEnv: currEnv, contents: stmts }) =>
      checkStmt(stmt, domEnv, currEnv).andThen(
        ({ subEnv: checkedEnv, contents: checkedStmts }) =>
          ok({
            subEnv: checkedEnv,
            contents: [...stmts, ...checkedStmts],
          }),
      ),
    ok({ subEnv, contents }),
  );

  return stmtsOk.andThen((r) =>
    ok({
      subEnv: r.subEnv,
      contents: {
        ...prog,
        statements: r.contents,
      },
    }),
  );
};

const checkStmt = (
  stmt: Stmt<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<CompiledSubStmt<A>[]> => {
  if (stmt.tag === "StmtSet") return checkStmtISetHelper(stmt, domEnv, subEnv);
  else return checkSingleStmt(stmt, domEnv, subEnv);
};

const checkStmtISetHelper = (
  stmtSet: StmtSet<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<CompiledSubStmt<A>[]> => {
  const { stmt } = stmtSet;
  switch (stmt.tag) {
    case "Decl": {
      // special, smarter handling for decl
      return checkDeclISet({ ...stmtSet, stmt }, domEnv, subEnv);
    }
    case "DeclList": {
      // special, smarter handling for declList
      return checkDeclListISet({ ...stmtSet, stmt }, domEnv, subEnv);
    }
    case "Bind": {
      return checkStmtISet(
        { ...stmtSet, stmt },
        domEnv,
        subEnv,
        substISetBind,
        checkBind,
      );
    }
    case "DeclBind": {
      return checkStmtISet(
        { ...stmtSet, stmt },
        domEnv,
        subEnv,
        substISetDeclBind,
        checkDeclBind,
      );
    }
    case "ApplyPredicate": {
      return checkStmtISet(
        { ...stmtSet, stmt },
        domEnv,
        subEnv,
        substISetPredicate,
        checkPredicate,
      );
    }
    case "AutoLabel": {
      return checkStmtISet(
        { ...stmtSet, stmt },
        domEnv,
        subEnv,
        substISetAutoLabel,
        checkAutoLabel,
      );
    }
    case "LabelDecl": {
      return checkStmtISet(
        { ...stmtSet, stmt },
        domEnv,
        subEnv,
        substISetLabelDecl,
        checkLabelDecl,
      );
    }
    case "NoLabel": {
      return checkStmtISet(
        { ...stmtSet, stmt },
        domEnv,
        subEnv,
        substISetNoLabel,
        checkNoLabel,
      );
    }
    default: {
      return err([
        {
          tag: "UnsupportedIndexingError",
          iset: stmtSet,
        },
      ]);
    }
  }
};

const checkSingleStmt = (
  stmt: SubStmt<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<CompiledSubStmt<A>[]> => {
  switch (stmt.tag) {
    case "Decl": {
      return checkDecl(stmt, domEnv, subEnv);
    }
    case "DeclList": {
      return checkDeclList(stmt, domEnv, subEnv);
    }
    case "Bind": {
      return checkBind(stmt, domEnv, subEnv);
    }
    case "DeclBind": {
      return checkDeclBind(stmt, domEnv, subEnv);
    }
    case "ApplyPredicate": {
      return checkPredicate(stmt, domEnv, subEnv);
    }
    case "AutoLabel": {
      return checkAutoLabel(stmt, domEnv, subEnv);
    }
    case "LabelDecl": {
      return checkLabelDecl(stmt, domEnv, subEnv);
    }
    case "NoLabel": {
      return checkNoLabel(stmt, domEnv, subEnv);
    }
  }
};

type ISetSubst = Map<string, number>;

const evalISet = (iset: IndexSet<A>): Result<ISetSubst[], SubstanceError> => {
  const { indices, condition } = iset;

  // Check for duplication in variable declarations
  const variables = new Set<string>();
  for (const varName of indices.map((i) => i.variable.value)) {
    if (variables.has(varName)) {
      return err({
        tag: "DuplicateIndexError",
        index: varName,
        location: iset,
      });
    }
    variables.add(varName);
  }

  type VarValPair = [string, number];
  const possValsPerVar: [VarValPair][][] = [];
  for (const { variable, range } of indices) {
    const name = variable.value;
    const { high, low } = range;
    const highVal = high.contents,
      lowVal = low.contents;

    if (!Number.isInteger(lowVal)) {
      return err({
        tag: "BadSetIndexRangeError",
        index: lowVal,
        location: low,
      });
    }

    if (!Number.isInteger(highVal)) {
      return err({
        tag: "BadSetIndexRangeError",
        index: highVal,
        location: high,
      });
    }

    if (high.contents < low.contents) {
      return ok([]);
    }

    // a list of [[name, value]]
    const possVals = im
      .Range(low.contents, high.contents + 1)
      .toArray()
      .map((n): [VarValPair] => [[name, n]]);
    // for example, if we write `i in [1, 3]`,
    // then possVals would contain `[["i", 1]], [["i", 2]], [["i", 3]]`
    // This structure makes it easier to combine these using Cartesian products.
    possValsPerVar.push(possVals);
  }

  const [first, ...rest] = possValsPerVar;
  if (first !== undefined) {
    const cprod = rest.reduce(
      (p: VarValPair[][], c: VarValPair[][]) =>
        cartesianProduct(
          p,
          c,
          () => true,
          (p1, p2) => [...p1, ...p2],
        ),
      first,
    );

    // Each element of "cprod" represents a substitution.

    const substitutions = cprod.map((cprod) => new Map(cprod));

    const condVals = all(substitutions.map((s) => evalCond(condition, s)));
    if (condVals.isErr) {
      // Outputting the first error because if there were to be multiple errors,
      // the errors will all be the same, caused by different substitutions.
      return err(condVals.error[0]);
    } else return ok(substitutions.filter((s, i) => condVals.value[i]));
  } else {
    return ok([]);
  }
};

const evalCond = (
  b: BooleanExpr<A> | undefined,
  subst: ISetSubst,
): Result<boolean, SubstanceError> => {
  if (b === undefined) {
    return ok(true);
  }
  if (b.tag === "BooleanConstant") {
    const { value } = b;
    return ok(value);
  } else if (b.tag === "BinaryBooleanExpr") {
    const { operator, left, right } = b;
    if (operator === "&&") {
      const lValRes = evalCond(left, subst);
      if (lValRes.isErr) return err(lValRes.error);
      // short-circuiting - if left side is false, then return false.
      if (!lValRes.value) return ok(false);
      else return evalCond(right, subst);
    } else {
      const lValRes = evalCond(left, subst);
      if (lValRes.isErr) return err(lValRes.error);
      // short-cirsuiting - if left side is true, then return true
      if (lValRes.value) return ok(true);
      else return evalCond(right, subst);
    }
  } else if (b.tag === "UnaryBooleanExpr") {
    const { arg } = b;
    const argValRes = evalCond(arg, subst);
    return argValRes.andThen((b) => ok(!b));
  } else {
    const { operator, left, right } = b;
    const lValRes = evalNum(left, subst);
    if (lValRes.isErr) return err(lValRes.error);
    const rValRes = evalNum(right, subst);
    if (rValRes.isErr) return err(rValRes.error);

    const lVal = lValRes.value,
      rVal = rValRes.value;

    // We use closeEqual due to floating-point precision issues
    // since numbers are internally represented as floating-point numbers
    if (operator === "<")
      return ok(closeEqual(lVal, rVal) ? false : lVal < rVal);
    else if (operator === ">")
      return ok(closeEqual(lVal, rVal) ? false : lVal > rVal);
    else if (operator === "<=")
      return ok(closeEqual(lVal, rVal) ? true : lVal <= rVal);
    else if (operator === ">=")
      return ok(closeEqual(lVal, rVal) ? true : lVal >= rVal);
    else if (operator === "==") return ok(closeEqual(lVal, rVal));
    else return ok(!closeEqual(lVal, rVal));
  }
};

const closeEqual = (x: number, y: number): boolean => {
  const EPS = 0.00001;
  return Math.abs(x - y) < EPS;
};

const evalNum = (
  n: NumExpr<A>,
  subst: ISetSubst,
): Result<number, SubstanceError> => {
  const result = evalNumHelper(n, subst);
  if (result.isErr) return err(result.error);

  const value = result.value;

  if (isNaN(value)) {
    // NaN is invalid
    return err({
      tag: "InvalidArithmeticValueError",
      location: n,
      value,
    });
  }
  return ok(value);
};

const evalNumHelper = (
  n: NumExpr<A>,
  subst: ISetSubst,
): Result<number, SubstanceError> => {
  if (n.tag === "NumberConstant") {
    return ok(n.contents);
  } else if (n.tag === "Identifier") {
    return substISetVarNumber(n.value, n, subst);
  } else if (n.tag === "UnaryExpr") {
    const { arg } = n;
    const argValRes = evalNum(arg, subst);
    return argValRes.andThen((n) => ok(-n));
  } else {
    const { operator, left, right } = n;
    const lValRes = evalNum(left, subst);
    if (lValRes.isErr) return err(lValRes.error);
    const rValRes = evalNum(right, subst);
    if (rValRes.isErr) return err(rValRes.error);

    const lVal = lValRes.value,
      rVal = rValRes.value;

    if (operator === "+") return ok(lVal + rVal);
    else if (operator === "-") return ok(lVal - rVal);
    else if (operator === "*") return ok(lVal * rVal);
    else if (operator === "^") return ok(lVal ** rVal);
    else {
      // div or mod
      if (rVal === 0) {
        return err({
          tag: "DivideByZeroError",
          location: n,
        });
      }
      if (operator === "/") return ok(lVal / rVal);
      else return ok(lVal % rVal);
    }
  }
};

const substISetVarNumber = (
  v: string,
  location: AbstractNode,
  subst: ISetSubst,
): Result<number, SubstanceError> => {
  // If already a number, use that number.
  if (!isNaN(Number(v))) {
    return ok(Number(v));
  }

  const isetVarValue = subst.get(v);
  if (isetVarValue === undefined) {
    return err({
      tag: "InvalidSetIndexingError",
      index: v,
      location,
      suggestions: [...subst.keys()],
    });
  }
  return ok(isetVarValue);
};

const substISetVarStr = (
  v: string,
  location: AbstractNode,
  subst: ISetSubst,
): Result<string, SubstanceError> => {
  const underscorePos = v.lastIndexOf("_");
  if (underscorePos === -1) {
    return ok(v);
  }

  const prefix = v.slice(0, underscorePos);
  const isetVarName = v.slice(underscorePos + 1);

  const isetVarValue = substISetVarNumber(isetVarName, location, subst);
  return isetVarValue.andThen((idx) => ok(`${prefix}_${idx}`));
};

const substISetId = (
  id: Identifier<A>,
  subst: ISetSubst,
): Result<Identifier<A>, SubstanceError> => {
  return substISetVarStr(id.value, id, subst).andThen((substitutedID: string) =>
    ok({
      ...id,
      value: substitutedID,
    }),
  );
};

const substISetExpr = (
  expr: SubExpr<A>,
  subst: ISetSubst,
): Result<SubExpr<A>, SubstanceError> => {
  const { tag } = expr;
  switch (tag) {
    case "ApplyFunction":
    case "ApplyConstructor":
    case "Func":
      return substISetFunc(expr, subst);
    default:
      return substSubArgExpr(expr, subst);
  }
};

const substSubArgExpr = (
  expr: SubArgExpr<A>,
  subst: ISetSubst,
): Result<SubArgExpr<A>, SubstanceError> => {
  const { tag } = expr;
  if (tag === "Identifier") {
    // first, if the identifier coincides with the iset index variable, then just use the value of that variable.
    const n = substISetVarNumber(expr.value, expr, subst);
    if (n.isOk) {
      return ok({
        ...expr,
        tag: "LiteralSubExpr",
        contents: {
          ...expr,
          tag: "NumberConstant",
          contents: n.value,
        },
      });
    } else {
      // otherwise try to substitute the last part (after underscore) of the identifier
      return substISetId(expr, subst);
    }
  } else {
    return ok(expr);
  }
};

const substISetFunc = (
  func: ApplyFunction<A> | ApplyConstructor<A> | Func<A>,
  subst: ISetSubst,
): Result<ApplyFunction<A> | ApplyConstructor<A> | Func<A>, SubstanceError> => {
  // Don't substitute over function names
  const substArgs = safeChain<SubArgExpr<A>, SubArgExpr<A>[], SubstanceError>(
    func.args,
    (arg, curr: SubArgExpr<A>[]) =>
      substSubArgExpr(arg, subst).andThen((sArg) => ok([...curr, sArg])),
    ok([]),
  );
  if (substArgs.isErr) {
    return err(substArgs.error);
  }

  return ok({
    ...func,
    args: substArgs.value,
  });
};

const substISetBind = (
  bind: Bind<A>,
  subst: ISetSubst,
): Result<Bind<A>, SubstanceError> => {
  const { variable, expr } = bind;
  const substVariable = substISetId(variable, subst);
  if (substVariable.isErr) return err(substVariable.error);
  const substExpr = substISetExpr(expr, subst);
  if (substExpr.isErr) return err(substExpr.error);
  return ok({
    ...bind,
    variable: substVariable.value,
    expr: substExpr.value,
  });
};

const substISetDeclBind = (
  declBind: DeclBind<A>,
  subst: ISetSubst,
): Result<DeclBind<A>, SubstanceError> => {
  const { variable, expr } = declBind;
  const substVariable = substISetId(variable, subst);
  if (substVariable.isErr) return err(substVariable.error);
  const substExpr = substISetExpr(expr, subst);
  if (substExpr.isErr) return err(substExpr.error);
  return ok({
    ...declBind,
    variable: substVariable.value,
    expr: substExpr.value,
  });
};

const substISetPredicate = (
  pred: ApplyPredicate<A>,
  subst: ISetSubst,
): Result<ApplyPredicate<A>, SubstanceError> => {
  const substArgs = safeChain<SubArgExpr<A>, SubArgExpr<A>[], SubstanceError>(
    pred.args,
    (arg, curr: SubArgExpr<A>[]) =>
      substSubArgExpr(arg, subst).andThen((sArg) => ok([...curr, sArg])),
    ok([]),
  );
  if (substArgs.isErr) {
    return err(substArgs.error);
  }

  return ok({
    ...pred,
    args: substArgs.value,
  });
};

const substISetLabelDecl = (
  labelDecl: LabelDecl<A>,
  subst: ISetSubst,
): Result<LabelDecl<A>, SubstanceError> =>
  substISetId(labelDecl.variable, subst).andThen((id) =>
    ok({ ...labelDecl, variable: id }),
  );

const substISetAutoLabel = (
  autoLabel: AutoLabel<A>,
  subst: ISetSubst,
): Result<AutoLabel<A>, SubstanceError> => {
  if (autoLabel.option.tag === "DefaultLabels") {
    return ok(autoLabel);
  } else {
    const { variables } = autoLabel.option;
    const substVariablesResult = all(
      variables.map((variable) => substISetId(variable, subst)),
    );
    if (substVariablesResult.isErr) {
      // NOTE: return first error
      return err(substVariablesResult.error[0]);
    }

    return ok({
      ...autoLabel,
      option: {
        ...autoLabel.option,
        variables: substVariablesResult.value,
      },
    });
  }
};

const substISetNoLabel = (
  noLabel: NoLabel<A>,
  subst: ISetSubst,
): Result<NoLabel<A>, SubstanceError> => {
  const { args: variables } = noLabel;
  const substVariablesResult = all(
    variables.map((variable) => substISetId(variable, subst)),
  );
  if (substVariablesResult.isErr) {
    // return first error
    return err(substVariablesResult.error[0]);
  }

  return ok({
    ...noLabel,
    args: substVariablesResult.value,
  });
};

const checkTypeApp = (
  t: TypeApp<A>,
  domEnv: DomainEnv,
  allowLiteralType: boolean = false,
): Result<undefined, SubstanceError> => {
  if (!allowLiteralType && isLiteralType(toDomType(t))) {
    return err({
      tag: "DeclLiteralError",
      location: t,
      type: t,
    });
  } else {
    return checkType(toDomType(t), domEnv).andThen(() => ok(undefined));
  }
};

export const checkDecl = (
  stmt: Decl<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
  allowLiteralType: boolean = false,
): CheckerResult<Decl<A>[]> => {
  const decl = stmt;
  const { type, name: nameId } = decl;
  // check type constructor
  const typeOk = checkTypeApp(type, domEnv, allowLiteralType);
  // need to make sure that the types are not built-in types
  if (typeOk.isErr) return err([typeOk.error]);

  return createVars(type, [nameId], subEnv, decl);
};

const checkDeclISet = (
  stmtSet: StmtSet<A> & { stmt: Decl<A> },
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<Decl<A>[]> => {
  const { stmt: decl, iset } = stmtSet;
  const { type, name: uncompiledNameId } = decl;
  const typeOk = checkTypeApp(type, domEnv);
  if (typeOk.isErr) return err([typeOk.error]);

  const isetSubstsResult = evalISet(iset);
  if (isetSubstsResult.isErr) return err([isetSubstsResult.error]);

  const isetSubsts = isetSubstsResult.value;
  const substIdsResult = all(
    isetSubsts.map((subst) => substISetId(uncompiledNameId, subst)),
  );

  if (substIdsResult.isErr) {
    return err(substIdsResult.error);
  }

  const substIds = substIdsResult.value;

  return createVars(type, substIds, subEnv, decl);
};

export const checkDeclList = (
  stmt: DeclList<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<Decl<A>[]> => {
  const declList = stmt;
  const { type, names: nameIds } = declList;

  // check type constructor
  const typeOk = checkTypeApp(type, domEnv);
  if (typeOk.isErr) {
    return err([typeOk.error]);
  }

  return createVars(type, nameIds, subEnv, declList);
};

const checkDeclListISet = (
  stmtSet: StmtSet<A> & { stmt: DeclList<A> },
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<Decl<A>[]> => {
  const { stmt: declList, iset } = stmtSet;
  const { type, names: uncompiledNameIds } = declList;
  const typeOk = checkTypeApp(type, domEnv);
  if (typeOk.isErr) return err([typeOk.error]);

  const isetSubstsResult = evalISet(iset);
  if (isetSubstsResult.isErr) return err([isetSubstsResult.error]);

  const isetSubsts = isetSubstsResult.value;
  const substIdsResult = all(
    isetSubsts
      .map((subst) =>
        uncompiledNameIds.map((uncompiledNameId) =>
          substISetId(uncompiledNameId, subst),
        ),
      )
      .flat(),
  );

  if (substIdsResult.isErr) {
    return err(substIdsResult.error);
  }

  const substIds = substIdsResult.value;

  return createVars(type, substIds, subEnv, declList);
};

const createVars = (
  type: TypeApp<A>,
  nameIds: Identifier<A>[],
  subEnv: SubstanceEnv,
  node:
    | Decl<A>
    | DeclList<A>
    | (StmtSet<A> & { stmt: Decl<A> })
    | (StmtSet<A> & { stmt: DeclList<A> }),
): CheckerResult<Decl<A>[]> => {
  let vars = subEnv.objs;
  const varIDs = [...subEnv.objIds];
  const equivalentDecls: Decl<A>[] = [];
  const errs: SubstanceError[] = [];

  for (const nameId of nameIds) {
    const { value: name } = nameId;
    const dup = varIDs.find((id) => id.value === name);
    if (dup !== undefined) {
      errs.push(duplicateName(nameId, node, dup));
    }

    vars = vars.set(name, toDomType(type));
    varIDs.push(nameId);
    equivalentDecls.push({
      ...location(node),
      tag: "Decl",
      type,
      name: nameId,
    });
  }

  if (errs.length > 0) {
    return err(errs);
  }

  return ok({
    subEnv: { ...subEnv, objs: vars, objIds: varIDs },
    contents: equivalentDecls,
  });
};

export const checkBind = (
  stmt: Bind<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
  allowUndeclaredVarToLiteral: boolean = false,
): CheckerResult<Bind<A>[]> => {
  const { variable, expr } = stmt;
  const varOk = checkVar(variable, domEnv, subEnv);
  const exprOk = checkExpr(expr, domEnv, subEnv, allowUndeclaredVarToLiteral);
  // check bind type
  return subtypeOf(exprOk, varOk, domEnv).andThen(
    ({ subEnv, contents: [e, v] }) => {
      const updatedBind: Bind<A> = { ...stmt, variable: v, expr: e };
      return ok({
        subEnv,
        contents: [updatedBind],
      });
    },
  );
};

const checkDeclBind = (
  stmt: DeclBind<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<(Decl<A> | Bind<A>)[]> => {
  const declBind = stmt;
  const { type, variable, expr } = declBind;

  const decl: Decl<A> = {
    ...declBind,
    tag: "Decl",
    name: variable,
  };

  if ("expr" in decl) {
    delete decl.expr;
  }

  const declResult = checkDecl(decl, domEnv, subEnv);
  if (declResult.isErr) return err(declResult.error);
  const { subEnv: checkedDeclEnv, contents: checkedDecls } = declResult.value;

  const bind: Bind<A> = {
    ...declBind,
    tag: "Bind",
  };

  if ("type" in bind) {
    delete bind.type;
  }

  const bindResult = checkBind(bind, domEnv, checkedDeclEnv);
  if (bindResult.isErr) return err(bindResult.error);
  const { subEnv: checkedBindEnv, contents: checkedBinds } = bindResult.value;

  return ok({
    subEnv: checkedBindEnv,
    contents: [...checkedDecls, ...checkedBinds],
  });
};

export const checkPredicate = (
  expr: ApplyPredicate<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
  allowUndeclaredVarToLiteral: boolean = false,
): CheckerResult<[ApplyPredicate<A>]> => {
  const { name, args } = expr;

  const decl = domEnv.predicateDecls.get(name.value);

  if (decl !== undefined) {
    const argsOk = checkArgs(
      args,
      decl.args,
      { givenExpr: expr, expectedExpr: decl, name },
      domEnv,
      subEnv,
      allowUndeclaredVarToLiteral,
    );

    return argsOk.andThen((r) =>
      ok({
        subEnv: r.subEnv,
        contents: [
          {
            ...expr,
            args: r.contents,
          },
        ],
      }),
    );
  } else {
    return err([
      typeNotFound(
        name,
        [...domEnv.predicateDecls.values()].map((p) => p.name),
      ),
    ]);
  }
};

const checkFunc = (
  expr: Func<A> | ApplyFunction<A> | ApplyConstructor<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
  allowUndeclaredVarToLiteral: boolean = false,
): ResultWithType<ApplyConstructor<A> | ApplyFunction<A>> => {
  const fname = expr.name.value;
  let decl: ConstructorDecl<A> | FunctionDecl<A> | undefined;
  if (domEnv.constructorDecls.has(fname)) {
    expr = { ...expr, tag: "ApplyConstructor" };
    decl = domEnv.constructorDecls.get(fname);
  } else if (domEnv.functionDecls.has(fname)) {
    expr = { ...expr, tag: "ApplyFunction" };
    decl = domEnv.functionDecls.get(fname);
  } else {
    return err([
      typeNotFound(expr.name, [
        ...[...domEnv.constructorDecls.values()].map((c) => c.name),
        ...[...domEnv.functionDecls.values()].map((c) => c.name),
      ]),
    ]);
  }

  const newExpr: ApplyConstructor<A> | ApplyFunction<A> = expr;

  if (decl !== undefined) {
    const { output } = decl;
    const argsOk = checkArgs(
      newExpr.args,
      decl.args,
      {
        givenExpr: newExpr,
        expectedExpr: decl,
        name: newExpr.name,
      },
      domEnv,
      subEnv,
      allowUndeclaredVarToLiteral,
    );

    const outputOk = argsOk.andThen((r) =>
      withType(r.subEnv, output.type, {
        ...newExpr,
        args: r.contents,
      }),
    );

    return outputOk;
  } else {
    return err([typeNotFound(expr.name)]);
  }
};

export const checkArgs = (
  actualArgs: SubArgExpr<A>[],
  expectedArgs: Arg<A>[],
  where: {
    givenExpr: AbstractNode;
    expectedExpr: AbstractNode;
    name: Identifier<A>;
  },
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
  allowUndeclaredVarToLiteral: boolean = false,
): CheckerResult<SubArgExpr<A>[]> => {
  if (actualArgs.length !== expectedArgs.length) {
    return err([
      argLengthMismatch(
        where.name,
        actualArgs,
        expectedArgs,
        where.givenExpr,
        where.expectedExpr,
      ),
    ]);
  }
  const argPairs = zip2(actualArgs, expectedArgs);
  const contents: SubArgExpr<A>[] = [];

  const argsOk = safeChain<
    [SubArgExpr<A>, Arg<A>],
    WithEnv<SubArgExpr<A>[]>,
    SubstanceError[]
  >(
    argPairs,
    ([actual, expected], args) =>
      matchArg(
        actual,
        expected,
        domEnv,
        args.subEnv,
        allowUndeclaredVarToLiteral,
      ).andThen((res) =>
        ok({
          subEnv: res.subEnv,
          contents: [...args.contents, res.contents],
        }),
      ),
    ok({ subEnv, contents }),
  );

  return argsOk;
};

const checkLabelDecl = (
  stmt: LabelDecl<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<LabelDecl<A>[]> => {
  return checkVar(stmt.variable, domEnv, subEnv).andThen(({ subEnv }) =>
    ok({ subEnv, contents: [stmt] }),
  );
};

const checkAutoLabel = (
  stmt: AutoLabel<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<AutoLabel<A>[]> => {
  // NOTE: no checking required
  if (stmt.option.tag === "DefaultLabels") {
    return ok({ subEnv, contents: [stmt] });
  } else {
    const varsOk = every(
      ...stmt.option.variables.map((v) => checkVar(v, domEnv, subEnv)),
    );
    return varsOk.andThen(({ subEnv }) => ok({ subEnv, contents: [stmt] }));
  }
};

const checkNoLabel = (
  stmt: NoLabel<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<[NoLabel<A>]> => {
  const argsOk = every(...stmt.args.map((a) => checkVar(a, domEnv, subEnv)));
  return argsOk.andThen(({ subEnv }) => ok({ subEnv, contents: [stmt] }));
};

const checkStmtISet = <T extends StmtSet<A>>(
  stmtSet: T,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
  substFunc: (
    stmt: T["stmt"],
    isetSubst: ISetSubst,
  ) => Result<T["stmt"], SubstanceError>,
  checkerFunc: (
    stmt: T["stmt"],
    domEnv: DomainEnv,
    subEnv: SubstanceEnv,
  ) => CheckerResult<CompiledSubStmt<A>[]>,
): CheckerResult<CompiledSubStmt<A>[]> => {
  const { stmt, iset } = stmtSet;
  const isetSubstsResult = evalISet(iset);
  if (isetSubstsResult.isErr) return err([isetSubstsResult.error]);

  const isetSubsts = isetSubstsResult.value;
  const substStmtsResult = all(
    isetSubsts.map((subst) => substFunc(stmt, subst)),
  );
  if (substStmtsResult.isErr) {
    return err(substStmtsResult.error);
  }
  const substStmts = substStmtsResult.value;

  return safeChain(
    substStmts,
    (substStmt, curr: WithEnv<CompiledSubStmt<A>[]>) => {
      const { subEnv: currEnv, contents: currStmts } = curr;
      const checked = checkerFunc(substStmt, domEnv, currEnv);
      if (checked.isErr) return err(checked.error);
      const { subEnv: checkedEnv, contents: newStmts } = checked.value;
      return ok({
        subEnv: checkedEnv,
        contents: [...currStmts, ...newStmts],
      });
    },
    ok({ subEnv, contents: [] }),
  );
};

// TODO: in general, true-myth seem to have trouble transforming data within the monad when the transformation itself can go wrong. If the transformation function cannot return errors, it's completely fine to use `ap`. This particular scenario is technically handled by `andThen`, but it seems to have problems with curried functions.
export const subtypeOf = <T1 extends ASTNode<A>, T2 extends ASTNode<A>>(
  type1: ResultWithType<T1>,
  type2: ResultWithType<T2>,
  domEnv: DomainEnv,
): CheckerResult<[T1, T2]> => {
  // TODO: find a more elegant way of writing this
  return type1.match({
    Ok: ({ type: t1, subEnv: subEnv, contents: expr1 }) =>
      type2.match({
        Ok: ({ type: t2, contents: expr2 }) => {
          // TODO: Check ordering of types, maybe annotated the ordering in the signature
          // TODO: call the right type equality function
          if (isSubtype(t1, t2, domEnv))
            return ok({ subEnv, contents: [expr1, expr2] });
          else {
            return err([typeMismatch(toSubType(t1), t2, expr1, expr2)]);
          }
        },
        Err: (e2) => err(e2),
      }),
    Err: (e1) => err(e1),
  });
};

const withType = <T>(
  subEnv: SubstanceEnv,
  type: Type<A>,
  contents: T,
): ResultWithType<T> => ok({ type, subEnv, contents });

export const checkExpr = (
  expr: SubExpr<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
  allowUndeclaredVarToLiteral: boolean = false,
): ResultWithType<SubExpr<A>> => {
  switch (expr.tag) {
    case "Func":
      return checkFunc(expr, domEnv, subEnv, allowUndeclaredVarToLiteral);
    case "ApplyFunction":
      return checkFunc(expr, domEnv, subEnv, allowUndeclaredVarToLiteral); // NOTE: the parser technically doesn't output this type, put in for completeness
    case "ApplyConstructor":
      return checkFunc(expr, domEnv, subEnv, allowUndeclaredVarToLiteral); // NOTE: the parser technically doesn't output this type, put in for completeness
    default:
      return checkSubArgExpr(expr, domEnv, subEnv);
  }
};

export const checkSubArgExpr = (
  expr: SubArgExpr<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): ResultWithType<SubArgExpr<A>> => {
  switch (expr.tag) {
    case "Identifier":
      return checkVar(expr, domEnv, subEnv);
    case "LiteralSubExpr":
      return checkLiteralSubExpr(expr, domEnv, subEnv);
  }
};

const addLiteral = (
  lits: LiteralSubExpr<A>[],
  lit: LiteralSubExpr<A>,
): LiteralSubExpr<A>[] => {
  // not the most efficient code
  // O(n) to add something into the list ... not great
  const unames = lits.map((l) => toLiteralUniqueName(l.contents.contents));
  const uname = toLiteralUniqueName(lit.contents.contents);

  if (unames.includes(uname)) {
    return [...lits];
  } else {
    return [...lits, lit];
  }
};

export const checkLiteralSubExpr = (
  expr: LiteralSubExpr<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): ResultWithType<LiteralSubExpr<A>> => {
  if (expr.contents.tag === "StringLit") {
    // add it to the list of literals
    return ok({
      type: stringType,
      subEnv: {
        ...subEnv,
        literals: addLiteral(subEnv.literals, expr),
      },
      contents: expr,
    });
  } else {
    return ok({
      type: numberType,
      subEnv: {
        ...subEnv,
        literals: addLiteral(subEnv.literals, expr),
      },
      contents: expr,
    });
  }
};

/**
 * Given a concrete type in Substance and the formal type in Domain (which may include type variables), check if the concrete type is well-formed and possibly add to the substitution map.
 *
 * @param givenType concrete type from Substance
 * @param formalType Domain type from Domain
 * @param sourceExpr the expression with the Substance type (for error reporting)
 * @param expectedExpr the expression where the Domain type is declared (for error reporting)
 * @param substEnv substitution environment
 */
const substituteArg = (
  givenType: TypeApp<A>,
  formalType: Type<A>,
  sourceExpr: SubArgExpr<A>,
  expectedExpr: Arg<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<SubArgExpr<A>> => {
  // TODO: check ordering of types
  if (!isSubtype(toDomType(givenType), formalType, domEnv)) {
    return err([typeMismatch(givenType, formalType, sourceExpr, expectedExpr)]);
  }
  return ok({
    subEnv,
    contents: sourceExpr,
  });
};
const matchArg = (
  expr: SubArgExpr<A>,
  arg: Arg<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
  allowUndeclaredVarToLiteral: boolean = false,
): CheckerResult<SubArgExpr<A>> => {
  // check and get the type of the expression
  const exprOk: ResultWithType<SubArgExpr<A>> = checkSubArgExpr(
    expr,
    domEnv,
    subEnv,
  );

  // Since Style compiler uses Substance checker
  // and we allow undeclared variables to literals
  // we add this special check

  // If we allow undeclared variables that refer to literals
  if (exprOk.isErr && allowUndeclaredVarToLiteral) {
    const allErrorTags = exprOk.error.map((e) => e.tag);
    // If there is only one error and that error is due to an undeclared variable
    if (allErrorTags.length === 1 && allErrorTags[0] === "VarNotFound") {
      // if the expression is really a variable
      if (expr.tag === "Identifier") {
        // if we are expecting to see a literal type
        if (isLiteralType(arg.type)) {
          // then just infer that this variable has the literal type!
          return ok({
            subEnv: {
              ...subEnv,
              objIds: [...subEnv.objIds, expr],
              objs: subEnv.objs.set(expr.value, arg.type),
            },
            contents: expr,
          });
        }
      }
    }
  }

  // check against the formal argument
  const argSubstOk = exprOk.andThen(({ subEnv, type }) =>
    substituteArg(toSubType(type), arg.type, expr, arg, domEnv, subEnv),
  );
  // if everything checks out, return env as a formality
  return argSubstOk;
};

const toSubType = (type: Type<A>): TypeApp<A> => ({ ...type, tag: "TypeApp" });

export const checkVar = (
  variable: Identifier<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): ResultWithType<Identifier<A>> => {
  const type = subEnv.objs.find((_, key) => key === variable.value);
  if (type) {
    return ok({ type, subEnv, contents: variable });
  } else {
    const possibleVars = subEnv.objIds;
    // TODO: find vars of the same type for error reporting (need to check expr first)
    return err([varNotFound(variable, possibleVars)]);
  }
};
//#endregion

//#region Substance pretty printer

export const prettySubstance = (prog: SubProg<A>): string =>
  prog.statements.map((stmt) => prettyStmt(stmt)).join("\n");

export const prettyCompiledSubstance = (prog: CompiledSubProg<A>): string =>
  prettySubstance(prog);

export const prettyStmt = (stmt: Stmt<A>): string => {
  if (stmt.tag !== "StmtSet") {
    return prettySingleStmt(stmt);
  } else {
    // TOOD: use more informative pretty-printing
    return `${prettySingleStmt(stmt.stmt)} ${prettyIndexSet(stmt.iset)}`;
  }
};

const prettyIndexSet = (iset: IndexSet<A>): string => {
  const rangeStrings: string[] = [];
  for (const range of iset.indices) {
    const varName = range.variable.value;
    const low = range.range.low.contents;
    const high = range.range.high.contents;
    rangeStrings.push(`${varName} in [${low}, ${high}]`);
  }
  const rangeString = rangeStrings.join(", ");

  if (iset.condition === undefined) {
    return `for ${rangeString}`;
  } else {
    return `for ${rangeString} where ${prettyCond(iset.condition)}`;
  }
};

const prettyCond = (cond: BooleanExpr<A>): string => {
  if (cond.tag === "BooleanConstant") {
    return cond.value.toString();
  } else if (cond.tag === "BinaryBooleanExpr") {
    return `(${prettyCond(cond.left)} ${cond.operator} ${prettyCond(
      cond.right,
    )})`;
  } else if (cond.tag === "UnaryBooleanExpr") {
    return `(${cond.operator}${prettyCond(cond.arg)})`;
  } else {
    return `(${prettyNum(cond.left)} ${cond.operator} ${prettyNum(
      cond.right,
    )})`;
  }
};

const prettyNum = (n: NumExpr<A>): string => {
  if (n.tag === "NumberConstant") {
    return `${n.contents}`;
  } else if (n.tag === "Identifier") {
    return n.value;
  } else if (n.tag === "UnaryExpr") {
    return `(${n.operator}${prettyNum(n.arg)})`;
  } else {
    return `(${prettyNum(n.left)} ${n.operator} ${prettyNum(n.right)})`;
  }
};

const prettyCompiledStmt = (stmt: CompiledSubStmt<A>): string => {
  return prettySingleStmt(stmt);
};

const prettySingleStmt = (stmt: SubStmt<A>): string => {
  switch (stmt.tag) {
    case "Decl": {
      const { type, name } = stmt;
      return `${prettyType(type)} ${prettyVar(name)}`;
    }
    case "DeclList": {
      const { type, names } = stmt;
      const pNames = names.map(prettyVar).join(", ");
      return `${prettyType(type)} ${pNames}`;
    }
    case "Bind": {
      const { variable, expr } = stmt;
      return `${prettyVar(variable)} := ${prettyExpr(expr)}`;
    }
    case "DeclBind": {
      const { type, variable, expr } = stmt;
      return `${prettyType(type)} ${prettyVar(variable)} := ${prettyExpr(
        expr,
      )}`;
    }
    case "AutoLabel":
      return `AutoLabel ${prettyLabelOpt(stmt.option)}`;
    case "NoLabel":
      return `NoLabel ${stmt.args.map((a) => prettyVar(a)).join(", ")}`;
    case "LabelDecl":
      return `Label ${prettyVar(stmt.variable)} $${stmt.label.contents}$`;
    case "ApplyPredicate":
      return prettyPredicate(stmt);
  }
};

export const prettySubNode = (
  node: SubExpr<A> | SubStmt<A> | TypeApp<A>,
): string => {
  switch (node.tag) {
    case "TypeApp":
      return prettyType(node);
    case "Bind":
    case "Decl":
    case "AutoLabel":
    case "NoLabel":
    case "LabelDecl":
    case "ApplyPredicate":
    case "DeclList":
    case "DeclBind":
      return prettyStmt(node);
    default:
      return prettyExpr(node);
  }
};

export const prettyPredicate = (pred: ApplyPredicate<A>): string => {
  const { name, args } = pred;
  const argStr = args.map((a) => prettyPredArg(a)).join(", ");
  return `${prettyVar(name)}(${argStr})`;
};

const prettyPredArg = (arg: SubArgExpr<A>): string => {
  return prettyExpr(arg);
};

const prettyType = (type: TypeApp<A>): string => {
  const { name } = type;
  return `${prettyVar(name)}`;
};

const prettyLabelOpt = (opt: LabelOption<A>): string => {
  switch (opt.tag) {
    case "DefaultLabels":
      return "All";
    case "LabelIDs":
      return opt.variables.map((v) => prettyVar(v)).join(", ");
  }
};

export const prettyVar = (v: Identifier<A>): string => {
  return v.value;
};

const prettyLiteralSubExpr = (e: NumberConstant<A> | StringLit<A>): string => {
  if (e.tag === "NumberConstant") {
    return e.contents.toString();
  } else {
    return e.contents;
  }
};
const prettyExpr = (expr: SubExpr<A>): string => {
  switch (expr.tag) {
    case "Identifier":
      return prettyVar(expr);
    case "LiteralSubExpr":
      return prettyLiteralSubExpr(expr.contents);
    case "ApplyFunction":
    case "Func":
    case "ApplyConstructor": {
      const { name, args } = expr;
      const argStr = args.map((arg) => prettyExpr(arg)).join(", ");
      return `${prettyVar(name)}(${argStr})`;
    }
  }
};

//#endregion
