import * as _ from "lodash";
import { insertExpr } from "engine/EngineUtils";

// TODO: Write pseudocode / code comments / tests for the Style compiler

// Really it should be SubOut, not SubProg:

// -- | 'SubOut' is the output of the Substance compiler, comprised of:
// -- * Substance AST
// -- * (Variable environment, Substance environment)
// -- * A mapping from Substance ids to their coresponding labels
// data SubOut =
//   SubOut SubProg (VarEnv, SubEnv) LabelMap

//#region consts
const ANON_KEYWORD = "ANON";
const LOCAL_KEYWORD = "$LOCAL";

//#endregion

//#region utils

// numbers from 0 to r-1 w/ increment of 1
const numbers = (r: number): number[] => {
  const l = 0;
  if (l > r) { throw Error("invalid range"); }
  const arr = [];
  for (let i = l; i < r; i++) {
    arr.push(i);
  }
  return arr;
}

export function numbered<A>(xs: A[]): [A, number][] {
  if (!xs) throw Error("fail");
  return _.zip(xs, numbers(xs.length)) as [A, number][]; // COMBAK: Don't know why typescript has problem with this
};

// TODO move to util

export function isLeft<A>(val: any): val is Left<A> {
  if ((val as Left<A>).tag === 'Left') return true;
  return false;
}

export function isRight<B>(val: any): val is Right<B> {
  if ((val as Right<B>).tag === 'Right') return true;
  return false;
}

export function Left<A>(val: A): Left<A> {
  return { contents: val, tag: 'Left' };
}

export function Right<B>(val: B): Right<B> {
  return { contents: val, tag: 'Right' };
}

export function foldM<A, B, C>(xs: A[], f: (acc: B, curr: A, i: number) => Either<C, B>, init: B): Either<C, B> {
  let res = init;
  let resW: Either<C, B> = Right(init); // wrapped

  for (let i = 0; i < xs.length; i++) {
    resW = f(res, xs[i], i);
    if (resW.tag === "Left") { return resW; } // Stop fold early on first error and return it
    res = resW.contents;
  }

  return resW;
};

function justs<T>(xs: MaybeVal<T>[]): T[] {
  return xs.filter(x => x.tag === "Just").map(x => {
    if (x.tag === "Just") { return x.contents; }
    throw Error("unexpected"); // Shouldn't happen
  });
}

const safeContentsList = (x: any) => x ? x.contents : [];

const toString = (x: BindingForm): string => x.contents.value;

// https://stackoverflow.com/questions/12303989/cartesian-product-of-multiple-arrays-in-javascript
const cartesianProduct =
  (...a: any[]) => a.reduce((a, b) => a.flatMap((d: any) => b.map((e: any) => [d, e].flat())));

const pathString = (p: Path): string => {
  if (p.tag === "FieldPath") {
    return `${p.name.contents.value}.${p.field.value}`;
  } else if (p.tag === "PropertyPath") {
    return `${p.name.contents.value}.${p.field.value}.${p.property.value}`;
  } else throw Error("pathStr not implemented");
};

//#endregion

//#region Some code for prettyprinting

const ppExpr = (e: SelExpr): string => {
  if (e.tag === "SEBind") {
    return e.contents.contents.value;
  } else if (["SEFunc", "SEValCons", "SEFuncOrValCons"].includes(e.tag)) {
    const args = e.args.map(ppExpr);
    return `${e.name.value}(${args})`;
  } else if ((e as any as StyVar).tag === "StyVar") {
    return (e as any as StyVar).contents.value;
  } else { console.log("res", e); throw Error("unknown tag"); }
};

const ppRelArg = (r: PredArg): string => {
  if (r.tag === "RelPred") {
    return ppRelPred(r);
  } else {
    return ppExpr(r)
  };
};

const ppRelBind = (r: RelBind): string => {
  const expr = ppExpr(r.expr);
  return `${r.id.contents.value} := ${expr}`;
}

const ppRelPred = (r: RelPred): string => {
  const args = r.args.map(ppRelArg).join(', ');
  const name = r.name.value;
  return `${name}(${args})`;
};

export const ppRel = (r: RelationPattern): string => {
  if (r.tag === "RelBind") {
    return ppRelBind(r);
  } else if (r.tag === "RelPred") {
    return ppRelPred(r);
  } else throw Error("unknown tag");
};

//#endregion

//#region Types and code for selector checking and environment construction

const initSelEnv = (): SelEnv => { // Note that JS objects are by reference, so you have to make a new one each time
  return {
    sTypeVarMap: {},
    varProgTypeMap: {},
    sErrors: [],
    skipBlock: false,
    header: { tag: "Nothing" },
  };
}

const dummySourceLoc = (): SourceLoc => {
  return { line: -1, col: -1 };
};

// COMBAK: Make fake identifier from string (e.g. if we don't have a source loc, make fake source loc)
const dummyIdentifier = (name: string): Identifier => {
  return {
    type: "value",
    value: name,
    tag: "dummyIdentifier",
    start: dummySourceLoc(),
    end: dummySourceLoc()
  }
};

// Add a mapping from Sub or Sty var to the selector's environment
// g, (x : |T)
// NOTE: Mutates the map in `m`
const addMapping = (k: BindingForm, v: StyT, m: SelEnv, p: ProgType): SelEnv => {
  m.sTypeVarMap[toString(k)] = v;
  m.varProgTypeMap[toString(k)] = [p, k];
  return m;
};

// Judgment 3. G; g |- |S_o ok ~> g'
// `checkDeclPattern`
const checkDeclPatternAndMakeEnv = (varEnv: VarEnv, selEnv: SelEnv, stmt: DeclPattern): SelEnv => {

  const [styType, bVar] = [stmt.type, stmt.id];
  if (bVar.tag === "StyVar") {
    // rule Decl-Sty-Context
    // NOTE: this does not aggregate *all* possible errors. May just return first error.
    // y \not\in dom(g)

    // TODO(errors)

    return addMapping(bVar, styType, selEnv, { tag: "StyProgT" });
  } else if (bVar.tag === "SubVar") {
    // rule Decl-Sub-Context
    // x \not\in dom(g)

    // TODO(errors)
    // TODO: Check subtypes
    // TODO: Check `skip block` condition

    return addMapping(bVar, styType, selEnv, { tag: "SubProgT" });
  } else throw Error("unknown tag");
};

// Judgment 6. G; g |- [|S_o] ~> g'
// `checkDeclPatterns` w/o error-checking, just addMapping for StyVars and SubVars
const checkDeclPatternsAndMakeEnv = (varEnv: VarEnv, selEnv: SelEnv, decls: DeclPattern[]): SelEnv => {
  return decls.reduce((s, p) => checkDeclPatternAndMakeEnv(varEnv, s, p), selEnv);
};

// ported from `checkPair`, `checkSel`, and `checkNamespace`
const checkHeader = (varEnv: VarEnv, header: Header): SelEnv => {
  if (header.tag === "Selector") {
    // Judgment 7. G |- Sel ok ~> g
    const sel: Selector = header;
    const selEnv_afterHead = checkDeclPatternsAndMakeEnv(varEnv, initSelEnv(), sel.head.contents);
    // Check `with` statements
    // TODO: Did we get rid of `with` statements?

    const selEnv_decls = checkDeclPatternsAndMakeEnv(varEnv, selEnv_afterHead, safeContentsList(sel.with));
    // TODO(error): rel_errs
    return selEnv_decls;
  } else if (header.tag === "Namespace") {
    // TODO(error)
    return initSelEnv();
  } else throw Error("unknown Style header tag");
}

// Returns a sel env for each selector in the Style program, in the same order
// previously named `checkSels`
export const checkSelsAndMakeEnv = (varEnv: VarEnv, prog: HeaderBlock[]): SelEnv[] => {
  const selEnvs: SelEnv[] = prog.map(e => {
    const res = checkHeader(varEnv, e.header);
    // Put selector AST in just for debugging
    res.header = { tag: "Just", contents: e.header };
    return res;
  });
  // const errors = ... TODO(errors)
  return selEnvs; // TODO
};

//#endregion

//#region Types and code for finding substitutions

// Judgment 20. A substitution for a selector is only correct if it gives exactly one
//   mapping for each Style variable in the selector. (Has test)
export const fullSubst = (selEnv: SelEnv, subst: Subst): boolean => {
  // Check if a variable is a style variable, not a substance one
  const isStyVar = (e: string): boolean => selEnv.varProgTypeMap[e][0].tag === "StyProgT";

  // Equal up to permutation (M.keys ensures that there are no dups)
  const selStyVars = Object.keys(selEnv.sTypeVarMap).filter(isStyVar);
  const substStyVars = Object.keys(subst);
  // Equal up to permutation (keys of an object in js ensures that there are no dups)
  return _.isEqual(selStyVars.sort(), substStyVars.sort());
};

// Check that there are no duplicate keys or vals in the substitution (Has test)
export const uniqueKeysAndVals = (subst: Subst): boolean => {
  // All keys already need to be unique in js, so only checking values
  const vals = Object.values(subst);
  const valsSet = {};

  for (let i = 0; i < vals.length; i++) {
    valsSet[vals[i]] = 0; // This 0 means nothing, we just want to create a set of values
  }

  // All entries were unique if length didn't change (ie the nub didn't change)
  return Object.keys(valsSet).length === vals.length;
};

const couldMatchRels = (typeEnv: VarEnv, rels: RelationPattern[], stmt: SubStmt): boolean => {
  // TODO < (this is an optimization)
  return true;
};

//#region (subregion? TODO fix) Applying a substitution
//// Apply a substitution to various parts of Style (relational statements, exprs, blocks)

// Recursively walk the tree, looking up and replacing each Style variable encountered with a Substance variable
// If a Sty var doesn't have a substitution (i.e. substitution map is bad), keep the Sty var and move on
// COMBAK: return "maybe" if a substitution fails?
// COMBAK: Add a type for `lv`? It's not used here
const substituteBform = (lv: any, subst: Subst, bform: BindingForm): BindingForm => {
  // theta(B) = ...
  if (bform.tag === "SubVar") {
    // Variable in backticks in block or selector (e.g. `X`), so nothing to substitute
    return bform;
  } else if (bform.tag === "StyVar") {
    // Look up the substitution for the Style variable and return a Substance variable
    // Returns result of mapping if it exists (y -> x)
    const res = subst[bform.contents.value];

    if (res) {
      return {
        ...bform, // Copy the start/end loc of the original Style variable, since we don't have Substance parse info (COMBAK)
        tag: "SubVar",
        contents: {
          ...bform.contents,  // Copy the start/end loc of the original Style variable, since we don't have Substance parse info
          type: "value",
          value: res
        }
      };
    } else { // Nothing to substitute
      return bform;
    }
  } else throw Error("unknown tag");
};

const substituteExpr = (subst: Subst, expr: SelExpr): SelExpr => {
  // theta(f[E]) = f([theta(E)]
  if (expr.tag !== "SEBind" && ["SEFunc", "SEValCons", "SEFuncOrValCons"].includes(expr.tag)) { // COMBAK: Remove SEFuncOrValCons
    return {
      ...expr,
      args: expr.args.map(arg => substituteExpr(subst, arg))
    };
  } else throw Error("unsupported tag");
};

const substitutePredArg = (subst: Subst, predArg: PredArg): PredArg => {
  if (predArg.tag === "RelPred") {
    return {
      ...predArg,
      args: predArg.args.map(arg => substitutePredArg(subst, arg))
    };
  } else if (predArg.tag === "SEBind") {
    return {
      ...predArg,
      contents: substituteBform({ tag: "Nothing" }, subst, predArg.contents) // COMBAK: Why is bform here...
    };
  } else { console.log("unknown tag", subst, predArg); throw Error("unknown tag"); }
};

// theta(|S_r) = ...
export const substituteRel = (subst: Subst, rel: RelationPattern): RelationPattern => {
  if (rel.tag === "RelBind") {
    // theta(B := E) |-> theta(B) := theta(E)
    return {
      ...rel,
      id: substituteBform({ tag: "Nothing" }, subst, rel.id),
      expr: substituteExpr(subst, rel.expr),
    };
  } else if (rel.tag === "RelPred") {
    // theta(Q([a]) = Q([theta(a)])
    return {
      ...rel,
      args: rel.args.map(arg => substitutePredArg(subst, arg)),
    };
  } else throw Error("unknown tag");
};

// Applies a substitution to a list of relational statement theta([|S_r])
// TODO: assumes a full substitution
const substituteRels = (subst: Subst, rels: RelationPattern[]): RelationPattern[] => {
  // COMBAK: Remove these logs
  // console.log("Before substitution", subst, rels, rels.map(ppRel));
  const res = rels.map(rel => substituteRel(subst, rel));
  // console.error("After substitution", subst, res, res.map(ppRel));
  // console.log("-------");
  return res;
};

//#endregion (subregion? TODO fix)

//#region Applying a substitution to a block

//// Substs for the translation semantics (more tree-walking on blocks, just changing binding forms)

const mkLocalVarName = (lv: LocalVarSubst): string => {
  if (lv.tag === "LocalVarId") {
    const [blockNum, substNum] = lv.contents;
    return `${LOCAL_KEYWORD}_block${blockNum}_subst${substNum}`;
  } else if (lv.tag === "NamespaceId") {
    return lv.contents;
  } else throw Error("unknown error");
};

const substitutePath = (lv: LocalVarSubst, subst: Subst, path: Path): Path => {
  if (path.tag === "FieldPath") {
    return {
      ...path,
      name: substituteBform({ tag: "Just", contents: lv }, subst, path.name)
    };
  } else if (path.tag === "PropertyPath") {
    return {
      ...path,
      name: substituteBform({ tag: "Just", contents: lv }, subst, path.name)
    };
  } else if (path.tag === "InternalLocalVar") {
    // Note that the local var becomes a path
    // Use of local var 'v' (on right-hand side of '=' sign in Style) gets transformed into field path reference 'LOCAL_<ids>.v'
    // where <ids> is a string generated to be unique to this selector match for this block

    // COMBAK / HACK: Is there some way to get rid of all these dummy values?
    return {
      start: dummySourceLoc(),
      end: dummySourceLoc(),
      tag: "FieldPath",
      name: {
        start: dummySourceLoc(),
        end: dummySourceLoc(),
        tag: "SubVar",
        contents: {
          ...dummyIdentifier(mkLocalVarName(lv))
        }
      },
      field: dummyIdentifier(path.contents)
    };
  } else if (path.tag === "AccessPath") {
    // COMBAK: Check if this works / is needed (wasn't present in original code)
    return {
      ...path,
      path: substitutePath(lv, subst, path.path)
    };
  } else {
    // COMBAK / ISSUE: This case shouldn't be here
    if ((path as any).tag === "LocalVar") {
      console.error("unexpected tag", path);

      return {
        start: dummySourceLoc(),
        end: dummySourceLoc(),
        tag: "FieldPath",
        name: {
          start: dummySourceLoc(),
          end: dummySourceLoc(),
          tag: "SubVar",
          contents: {
            ...dummyIdentifier(mkLocalVarName(lv))
          }
        },
        field: (path as any).contents // COMBAK ? ISSUE
      };
    }
    throw Error("unknown tag");
  }
};

const substituteField = (lv: LocalVarSubst, subst: Subst, field: PropertyDecl): PropertyDecl => {
  return {
    ...field,
    value: substituteBlockExpr(lv, subst, field.value)
  };
};

const isPath = (expr: Expr): expr is Path => {
  return ["FieldPath", "PropertyPath", "AccessPath", "LocalVar"].includes(expr.tag);
};

const substituteBlockExpr = (lv: LocalVarSubst, subst: Subst, expr: Expr): Expr => {
  // COMBAK delete this
  // console.error("expr", expr);

  if (isPath(expr)) {
    return substitutePath(lv, subst, expr);
  } else if (expr.tag === "CompApp" || expr.tag === "ObjFn" || expr.tag === "ConstrFn") {
    return {
      ...expr,
      args: expr.args.map((arg: Expr) => substituteBlockExpr(lv, subst, arg))
    };
  } else if (expr.tag === "AvoidFn") {
    return {
      ...expr,
      contents: [expr.contents[0], expr.contents[1].map((arg: Expr) => substituteBlockExpr(lv, subst, arg))]
    };
  } else if (expr.tag === "BinOp") {
    return {
      ...expr,
      left: substituteBlockExpr(lv, subst, expr.left),
      right: substituteBlockExpr(lv, subst, expr.right)
    };
  } else if (expr.tag === "UOp") {
    return {
      ...expr,
      arg: substituteBlockExpr(lv, subst, expr.arg)
    };
  } else if (expr.tag === "List" || expr.tag === "Vector" || expr.tag === "Matrix") {
    return {
      ...expr,
      contents: expr.contents.map((e: Expr) => substituteBlockExpr(lv, subst, e))
    };
  } else if (expr.tag === "ListAccess") {
    return {
      ...expr,
      contents: [substitutePath(lv, subst, expr.contents[0]), expr.contents[1]]
    };
  } else if (expr.tag === "GPIDecl") {
    return {
      ...expr,
      properties: expr.properties.map((p: PropertyDecl) => substituteField(lv, subst, p))
    };
  } else if (expr.tag === "Layering") {
    return {
      ...expr,
      below: substitutePath(lv, subst, expr.below),
      above: substitutePath(lv, subst, expr.above)
    };
  } else if (expr.tag === "PluginAccess") {
    return {
      ...expr,
      contents: [expr.contents[0],
      substituteBlockExpr(lv, subst, expr.contents[1]),
      substituteBlockExpr(lv, subst, expr.contents[2])]
    };
  } else if (expr.tag === "Tuple" || expr.tag === "ThenOp") {
    return {
      ...expr,
      contents: [substituteBlockExpr(lv, subst, expr.contents[0]),
      substituteBlockExpr(lv, subst, expr.contents[1])]
    };
  } else if (expr.tag === "VectorAccess") {
    return {
      ...expr,
      contents: [substitutePath(lv, subst, expr.contents[0]), substituteBlockExpr(lv, subst, expr.contents[1])]
    };
  } else if (expr.tag === "MatrixAccess") {
    return {
      ...expr,
      contents: [substitutePath(lv, subst, expr.contents[0]), expr.contents[1].map(e => substituteBlockExpr(lv, subst, e))]
    };
  } else if (expr.tag === "Fix" || expr.tag === "Vary" || expr.tag === "StringLit" || expr.tag === "BoolLit") {
    // No substitution for literals
    return expr;
  } else { console.error("expr", expr); throw Error("unknown tag"); }
};

const substituteLine = (lv: LocalVarSubst, subst: Subst, line: Stmt): Stmt => {
  if (line.tag === "PathAssign") {
    return {
      ...line,
      path: substitutePath(lv, subst, line.path),
      value: substituteBlockExpr(lv, subst, line.value)
    };
  } else if (line.tag === "Override") {
    return {
      ...line,
      path: substitutePath(lv, subst, line.path),
      value: substituteBlockExpr(lv, subst, line.value)
    };
  } else if (line.tag === "Delete") {
    return {
      ...line,
      contents: substitutePath(lv, subst, line.contents)
    };
  } else throw Error("Case should not be reached (anonymous statement should be substituted for a local one in `nameAnonStatements`)");
};

// Assumes a full substitution
const substituteBlock = ([subst, si]: [Subst, number], [block, bi]: [Block, number], name: MaybeVal<string>): Block => {

  const lvSubst: LocalVarSubst = name.tag === "Nothing"
    ? { tag: "LocalVarId", contents: [bi, si] }
    : { tag: "NamespaceId", contents: name.contents };

  return {
    ...block,
    statements: block.statements.map(line => substituteLine(lvSubst, subst, line))
  };
};

//#endregion Applying a substitution to a block

const toSubVar = (b: BindingForm): Var => {
  if (b.tag === "SubVar") {
    return b.contents.value;
  } else if (b.tag === "StyVar") {
    // return b.contents.value;
    throw Error("there shouldn't be a style variable in a selector expression; should have been substituted out");
  } else throw Error("unknown tag");
};

// Convert Style expression to Substance expression (for ease of comparison in matching)
const toSubExpr = (e: SelExpr): SubExpr => {
  if (e.tag === "SEBind") {
    return {
      tag: "VarE",
      contents: toSubVar(e.contents)
    };
  } else if (e.tag === "SEFunc") {
    return {
      tag: "ApplyFunc",
      contents: {
        nameFunc: e.name.value,
        argFunc: e.args.map(toSubExpr)
      }
    };
  } else if (e.tag === "SEValCons") {
    return {
      tag: "ApplyValCons",
      contents: {
        nameFunc: e.name.value,
        argFunc: e.args.map(toSubExpr)
      }
    };
  } else if (e.tag === "SEFuncOrValCons") {
    throw Error("compiler should have disambiguated func/val cons");
  } else throw Error("unknown tag");
};

const toSubPredArg = (a: PredArg): SubPredArg => {
  if (a.tag === "SEBind") {
    return {
      tag: "PE",
      contents: { // Only vars, no exprs
        tag: "VarE",
        contents: a.contents.contents.value
      }
    };
  } else if (a.tag === "RelPred") {
    return {
      tag: "PP",
      contents: toSubPred(a)
    };
  } else throw Error("unknown tag");
};

// Convert Style predicate to Substance predicate (for ease of comparison in matching)
const toSubPred = (p: RelPred): SubPredicate => {
  return {
    predicateName: p.name.value,
    predicateArgs: p.args.map(toSubPredArg),
    predicatePos: { // Unused dummy position, since the info is lost anyway
      sourceName: "undefined",
      sourceLine: 0,
      sourceColumn: 0
    }
  }
};

const varsEq = (v1: Var, v2: Var): boolean => {
  return v1 === v2; // String comparison
};

const predsEq = (p1: SubPredicate, p2: SubPredicate): boolean => {
  // !! COMBAK !!: this equality check is possible because the Substance exprs don't contain location information. But if they do, this has to be rewritten...
  // This equality check removes SourcePos
  return _.isEqual([p1.predicateName, p1.predicateArgs], [p2.predicateName, p2.predicateArgs]);
};

const exprsMatchArr = (typeEnv: VarEnv, subE: Func, selE: Func): boolean => {
  return _.isEqual(subE, selE);
  // COMBAK: Depends on subtype implementation 
  // COMBAK: Implement this match WRT subtyping
};

// New judgment (number?): expression matching that accounts for subtyping. G, B, . |- E0 <| E1
// We assume the latter expression has already had a substitution applied
const exprsMatch = (typeEnv: VarEnv, subE: SubExpr, selE: SubExpr): boolean => {
  // We match value constructor applications if one val ctor is a subtype of another
  // whereas for function applications, we match only if the exprs are equal (for now)
  // This is because a val ctor doesn't "do" anything besides wrap its values
  // whereas functions with the same type could do very different things, so we don't
  // necessarily want to match them by subtyping
  // (e.g. think of the infinite functions from Vector -> Vector)

  // rule Match-Expr-Var
  if (subE.tag === "VarE" && selE.tag === "VarE") {
    return varsEq(subE.contents, selE.contents);
  } else if (subE.tag === "ApplyFunc" && selE.tag === "ApplyFunc") { // rule Match-Expr-Fnapp
    // !! COMBAK !!: this equality check is possible because the Substance exprs don't contain location information. But if they do, this has to be rewritten...
    return _.isEqual(subE.contents, selE.contents);
  } else if (subE.tag === "ApplyValCons" && selE.tag === "ApplyValCons") { // rule Match-Expr-Vconsapp
    return exprsMatchArr(typeEnv, subE.contents, selE.contents);
  } else {
    return false;
  }
};

// Judgment 11. b; theta |- S <| |S_r
// After all Substance variables from a Style substitution are substituted in, check if 
const relMatchesLine = (typeEnv: VarEnv, subEnv: SubEnv, s1: SubStmt, s2: RelationPattern): boolean => {
  if (s1.tag === "Bind" && s2.tag === "RelBind") { // rule Bind-Match
    const bvar = s2.id;
    if (s2.id.tag === "StyVar") { // COMBAK: Error
      throw Error("Style variable ${rel.id.contents.value} found in relational statement ${ppRel(rel)}. Should not be present!");

    } else if (s2.id.tag === "SubVar") {
      // B |- E = |E
      const [subVar, sVar] = [s1.contents[0], s2.id.contents.value];
      const selExpr = toSubExpr(s2.expr); // TODO ^
      const subExpr = s1.contents[1];
      return varsEq(subVar, sVar) && exprsMatch(typeEnv, subExpr, selExpr); // TODO ^
      // COMBAK: Add this condition when the Substance typechecker is implemented
      // || exprsDeclaredEqual(subEnv, expr, selExpr); // B |- E = |E
    } else throw Error("unknown tag");

  } else if (s1.tag === "ApplyP" && s2.tag === "RelPred") { // rule Pred-Match
    const [pred, sPred] = [s1.contents, s2];
    const selPred = toSubPred(sPred); // TODO ^
    return predsEq(pred, selPred);     // TODO < check if this needs to be a deep equality check
    // COMBAK: Add this condition when the Substance typechecker is implemented
    // || C.predsDeclaredEqual subEnv pred selPred // B |- Q <-> |Q

  } else {
    return false; // Only match two bind lines or two predicate lines
  }
};

// Judgment 13. b |- [S] <| |S_r
const relMatchesProg = (typeEnv: VarEnv, subEnv: SubEnv, subProg: SubProg, rel: RelationPattern): boolean => {
  return subProg.some(line => relMatchesLine(typeEnv, subEnv, line, rel));
};

// Judgment 15. b |- [S] <| [|S_r]
const allRelsMatch = (typeEnv: VarEnv, subEnv: SubEnv, subProg: SubProg, rels: RelationPattern[]): boolean => {
  return rels.every(rel => relMatchesProg(typeEnv, subEnv, subProg, rel));
};

// Judgment 17. b; [theta] |- [S] <| [|S_r] ~> [theta']
// Folds over [theta]
const filterRels = (typeEnv: VarEnv, subEnv: SubEnv, subProg: SubProg, rels: RelationPattern[], substs: Subst[]): Subst[] => {
  const subProgFiltered = subProg.filter(line => couldMatchRels(typeEnv, rels, line));
  return substs.filter(subst => allRelsMatch(typeEnv, subEnv, subProgFiltered, substituteRels(subst, rels)));
};

//// Match declaration statements

const combine = (s1: Subst, s2: Subst): Subst => {
  return { ...s1, ...s2 };
};

// TODO check for duplicate keys (and vals)
// (x) operator combines two lists of substitutions: [subst] -> [subst] -> [subst]
// the way merge is used, I think each subst in the second argument only contains one mapping
const merge = (s1: Subst[], s2: Subst[]): Subst[] => {
  if (s2.length === 0) { return s1; }
  if (s1.length === 0) { return s2; }
  return cartesianProduct(s1, s2).map(([a, b]: Subst[]) => combine(a, b));
};

// Judgment 9. G; theta |- T <| |T
// Assumes types are nullary, so doesn't return a subst, only a bool indicating whether the types matched
// Ported from `matchType`
const typesMatched = (varEnv: VarEnv, substanceType: T, styleType: StyT): boolean => {
  if (substanceType.tag === "TConstr") {
    return substanceType.contents.nameCons === styleType.value;
    // TODO/COMBAK: Implement subtype checking
    // && isSubtype(substanceType, toSubType(styleType), varEnv);
  }

  // TODO(errors)
  console.log(substanceType, styleType);
  throw Error("expected two nullary types");
};

// Judgment 10. theta |- x <| B
const matchBvar = (subVar: Var, bf: BindingForm): MaybeVal<Subst> => {
  if (bf.tag === "StyVar") {
    const newSubst = {};
    newSubst[toString(bf)] = subVar; // StyVar matched SubVar
    return {
      tag: "Just",
      contents: newSubst
    };
  } else if (bf.tag === "SubVar") {
    if (subVar === bf.contents.value) { // Substance variables matched; comparing string equality
      return {
        tag: "Just",
        contents: {}
      }
    } else {
      return { tag: "Nothing" }; // TODO: Note, here we distinguish between an empty substitution and no substitution... but why?
    }
  } else throw Error("unknown tag");
};

// Judgment 12. G; theta |- S <| |S_o
// TODO: Not sure why Maybe<Subst> doesn't work in the type signature?
const matchDeclLine = (varEnv: VarEnv, line: SubStmt, decl: DeclPattern): MaybeVal<Subst> => {
  if (line.tag === "Decl") {
    const [subT, subVar] = line.contents;
    const [styT, bvar] = [decl.type, decl.id];

    // substitution is only valid if types matched first
    if (typesMatched(varEnv, subT, styT)) {
      return matchBvar(subVar, bvar);
    }
  }

  // Sty decls only match Sub decls
  return { tag: "Nothing" };
};

// Judgment 16. G; [theta] |- [S] <| [|S_o] ~> [theta']
const matchDecl = (varEnv: VarEnv, subProg: SubProg, initSubsts: Subst[], decl: DeclPattern): Subst[] => {
  // Judgment 14. G; [theta] |- [S] <| |S_o
  const newSubsts = subProg.map(line => matchDeclLine(varEnv, line, decl));
  const res = merge(initSubsts, justs(newSubsts)); // TODO inline
  // COMBAK: Inline this
  // console.log("substs to combine:", initSubsts, justs(newSubsts));
  // console.log("res", res);
  return res;
};

// Judgment 18. G; [theta] |- [S] <| [|S_o] ~> [theta']
// Folds over [|S_o]
const matchDecls = (varEnv: VarEnv, subProg: SubProg, decls: DeclPattern[], initSubsts: Subst[]): Subst[] => {
  return decls.reduce((substs, decl) => matchDecl(varEnv, subProg, substs, decl), initSubsts);
};

// Judgment 19. g; G; b; [theta] |- [S] <| Sel
// NOTE: this uses little gamma (not in paper) to check substitution validity
// ported from `find_substs_sel`
const findSubstsSel = (varEnv: VarEnv, subEnv: SubEnv, subProg: SubProg, [header, selEnv]: [Header, SelEnv]): Subst[] => {
  if (header.tag === "Selector") {
    const sel = header;
    const decls = sel.head.contents.concat(safeContentsList(sel.with));
    const rels = safeContentsList(sel.where);
    const initSubsts: Subst[] = [];
    const rawSubsts = matchDecls(varEnv, subProg, decls, initSubsts);
    const substCandidates = rawSubsts.filter(subst => fullSubst(selEnv, subst));
    const filteredSubsts = filterRels(varEnv, subEnv, subProg, rels, substCandidates);
    const correctSubsts = filteredSubsts.filter(uniqueKeysAndVals);
    return correctSubsts;
  } else if (header.tag === "Namespace") {
    // No substitutions for a namespace (not in paper)
    return [];
  } else throw Error("unknown tag");

};

// Find a list of substitutions for each selector in the Sty program. (ported from `find_substs_prog`)
export const findSubstsProg = (varEnv: VarEnv, subEnv: SubEnv, subProg: SubProg,
  styProg: HeaderBlock[], selEnvs: SelEnv[]): Subst[][] => {

  if (selEnvs.length !== styProg.length) { throw Error("expected same # selEnvs as selectors"); }
  const selsWithEnvs = _.zip(styProg.map((e: HeaderBlock) => e.header), selEnvs); // TODO: Why can't I type it [Header, SelEnv][]? It shouldn't be undefined after the length check

  return selsWithEnvs.map(selAndEnv => findSubstsSel(varEnv, subEnv, subProg, selAndEnv as [Header, SelEnv]));
};

//#endregion

//#region Naming anon statements

// Style AST preprocessing:
// For any anonymous statement only (e.g. `encourage near(x.shape, y.shape)`),
// replace it with a named statement (`local.<UNIQUE_ID> = encourage near(x.shape, y.shape)`)
// Note the UNIQUE_ID only needs to be unique within a block (since local will assign another ID that's globally-unique)
// Leave all other statements unchanged

const nameAnonStatement = ([i, b]: [number, Stmt[]], s: Stmt): [number, Stmt[]] => {
  // Transform stmt into local variable assignment "ANON_$counter = e" and increment counter
  if (s.tag === "AnonAssign") {
    const stmt: Stmt = {
      ...s,
      tag: "PathAssign",
      type: { tag: "TypeOf", contents: "Nothing" }, // TODO: Why is it parsed like this?
      path: {
        tag: "InternalLocalVar", contents: `\$${ANON_KEYWORD}_${i}`,
        start: dummySourceLoc(), end: dummySourceLoc() // Unused bc compiler internal
      },
      value: s.contents
    };
    return [i + 1, b.concat([stmt])];
  } else {
    return [i, b.concat([s])];
  }
};

const nameAnonBlock = (b: Block): Block => {
  return {
    ...b,
    statements: b.statements.reduce(
      (acc, curr) => nameAnonStatement(acc, curr), // Not sure why this can't be point-free
      [0, []] as [number, Stmt[]]
    )[1]
  };
};

export const nameAnonStatements = (prog: StyProg): StyProg => {
  const p = prog.blocks;
  return {
    ...prog,
    blocks: p.map(hb => ({ ...hb, block: nameAnonBlock(hb.block) }))
  };
};

//#endregion

//#region Translating Style program

const initTrans = (): Translation => {
  return { trMap: {}, warnings: [] };
};


///////// Translation judgments
/* Note: All of the folds below use foldM.
   foldM stops accumulating when the first fatal error is reached, using "Either [Error]" as a monad
   (Non-fatal errors are stored as warnings in the translation)
   foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
   example:
   f acc elem = if elem < 0 then Left ["wrong " ++ show elem] else Right $ elem : acc
   foldM f [] [1, 9, -1, 2, -2] = Left ["wrong -1"]
   foldM f [] [1, 9] = Right [9,1]  */
// Judgment 26. D |- phi ~> D'
// This is where interesting things actually happen (each line is interpreted and added to the translation)

// Related functions in `Evaluator`: findExpr, insertExpr

const addWarn = (tr: Translation, warn: Warning): Translation => {
  return {
    ...tr,
    warnings: tr.warnings.concat(warn)
  };
};

// Note this mutates the translation, and we return the translation reference just as a courtesy
const deleteProperty = (trans: Translation, name: BindingForm, field: Identifier, property: Identifier): Translation => {
  const trn = trans.trMap;
  const pathStr = pathString({ tag: "PropertyPath", name, field, property } as Path);

  const nm = name.contents.value;
  const fld = field.value;
  const prp = property.value;

  const fieldDict = trn[nm];

  if (!fieldDict) {
    // TODO (errors / warnings): Should this be fatal?
    const err = `Err: Sub obj '${nm}' has no fields; can't delete path '${pathStr}'`;
    return addWarn(trans, err);
  }

  const prop: FieldExpr<VarAD> = fieldDict[fld];

  if (!prop) {
    // TODO (errors / warnings): Should this be fatal?
    const err = `Err: Sub obj '${nm}' already lacks field '${fld}'; can't delete path ${pathStr}`;
    return addWarn(trans, err);
  }

  if (prop.tag === "FExpr") {
    // COMBAK (missing feature): Deal with path aliasing
    throw Error("deal with path aliasing");
    // TODO (error): deal with non-alias error
  } else if (prop.tag === "FGPI") {
    // TODO(error, warning): check if the property is member of properties of GPI
    const gpiDict = prop.contents[1];
    delete gpiDict.prp;
    return trans;
  } else throw Error("unknown tag");
};

// Note this mutates the translation, and we return the translation reference just as a courtesy
const deleteField = (trans: Translation, name: BindingForm, field: Identifier): Translation => {
  // TODO (errors)
  let trn = trans.trMap;
  const fieldDict = trn[name.contents.value];

  if (!fieldDict) {
    // TODO(errors / warnings)
    const warn = `Err: Sub obj '${name.contents.value}' has no fields; can't delete field '${field.value}'`;
    return addWarn(trans, warn);
  }

  if (!(field.value in fieldDict)) {
    // TODO(errors / warnings)
    const warn = `Warn: SubObj '${name.contents.value}' already lacks field '${field.value}'`;
    return addWarn(trans, warn);
  }

  delete fieldDict[field.value];
  return trans;
};

// NOTE: This function mutates the translation
// rule Line-delete
const deletePath = (trans: Translation, path: Path): Either<StyErrors, Translation> => {
  // TODO(errors): Maybe fix how warnings are reported? Right now they are put into the "translation"
  if (path.tag === "FieldPath") {
    const transWithWarnings = deleteField(trans, path.name, path.field);
    return Right(transWithWarnings);
  } else if (path.tag === "PropertyPath") {
    let transWithWarnings = deleteProperty(trans, path.name, path.field, path.property);
    return Right(transWithWarnings);
  } else if (path.tag === "AccessPath") {
    // TODO(error)
    return Left([`Cannot delete an element of a vector: ${path}`])
  } else if (path.tag === "InternalLocalVar") {
    throw Error("Compiler should not be deleting a local variable; this should have been removed in a earlier compiler pass");
  } else throw Error("unknown tag");
};

// NOTE: This function mutates the translation
const addPath = (override: boolean, trans: Translation, path: Path, expr: TagExpr<VarAD>): Either<StyErrors, Translation> => {
  // TODO(errors) <
  // Extend `insertExpr` with an optional flag to deal with errors and warnings
  // Move `insertExpr`, etc. into a translation utils file, as currently we import from Evaluator
  // - Removed: addField, addProperty

  return Right(insertExpr(path, expr, trans));
};

const translateLine = (trans: Translation, stmt: Stmt): Either<StyErrors, Translation> => {
  if (stmt.tag === "PathAssign") {
    return addPath(false, trans, stmt.path, { tag: "OptEval", contents: stmt.value });
  } else if (stmt.tag === "Override") {
    return addPath(true, trans, stmt.path, { tag: "OptEval", contents: stmt.value });
  } else if (stmt.tag === "Delete") {
    return deletePath(trans, stmt.contents);
  } else throw Error("unknown tag");
};

// Judgment 25. D |- |B ~> D' (modified to be: theta; D |- |B ~> D')
const translateBlock = (name: MaybeVal<string>, blockWithNum: [Block, number], trans: Translation, substWithNum: [Subst, number]): Either<StyErrors, Translation> => {
  const blockSubsted: Block = substituteBlock(substWithNum, blockWithNum, name);

  // TODO: Remove these lines
  console.error("block pre-subst", blockWithNum);
  console.error("block post-subst", blockSubsted);

  return foldM(blockSubsted.statements, translateLine, trans);
};

// Judgment 24. [theta]; D |- |B ~> D'
// This is a selector, not a namespace, so we substitute local vars with the subst/block IDs
const translateSubstsBlock = (trans: Translation, substsNum: [Subst, number][], blockWithNum: [Block, number]): Either<StyErrors, Translation> => {
  return foldM(substsNum, (trans, substNum, i) => translateBlock({ tag: "Nothing" }, blockWithNum, trans, substNum), trans);
};

const checkBlock = (selEnv: SelEnv, block: Block): StyErrors => {
  // COMBAK: Block checking and return block errors, not generic sty errors
  return [];
};

// Judgment 23, contd.
const translatePair = (varEnv: VarEnv, subEnv: SubEnv, subProg: SubProg, trans: Translation, hb: HeaderBlock, blockNum: number): Either<StyErrors, Translation> => {

  if (hb.header.tag === "Namespace") {
    const selEnv = initSelEnv();
    const bErrs = checkBlock(selEnv, hb.block); // COMBAK

    if (selEnv.sErrors.length > 0 || bErrs.length > 0) {
      // This is a namespace, not selector, so we substitute local vars with the namespace's name
      // skip transSubstsBlock; only one subst
      return {
        tag: "Left",
        contents: selEnv.sErrors.concat(bErrs)
      }
    }

    const subst = {};
    // COMBAK / errors: Keep the AST node from `hb.header` for error reporting?
    return translateBlock({ tag: "Just", contents: hb.header.contents.contents.value as any as string }, [hb.block, blockNum], trans, [subst, 0]);

  } else if (hb.header.tag === "Selector") {
    const selEnv = checkHeader(varEnv, hb.header);
    const bErrs = checkBlock(selEnv, hb.block); // COMBAK

    // If any Substance variable in the selector environment doesn't exist in the Substance program (e.g. Set `A`),
    // skip this block (because the Substance variable won't exist in the translation)

    if (selEnv.skipBlock) {
      return Right(trans);
    }

    if (selEnv.sErrors.length > 0 || bErrs.length > 0) {
      return {
        tag: "Left",
        contents: selEnv.sErrors.concat(bErrs)
      };
    }

    // For creating unique local var names
    const substs = findSubstsSel(varEnv, subEnv, subProg, [hb.header, selEnv]);
    return translateSubstsBlock(trans, numbered(substs), [hb.block, blockNum]);

  } else throw Error("unknown tag");
};

const insertNames = (trans: Translation): Translation => {
  // TODO <
  return trans;

};

const insertLabels = (trans: Translation, labels: LabelMap): Translation => {
  // TODO <
  return trans;
};

const translateStyProg = (varEnv: VarEnv, subEnv: SubEnv, subProg: SubProg, styProg: StyProg, labelMap: LabelMap, styVals: number[]): Either<StyErrors, Translation> => {
  // COMBAK: Deal with styVals

  const res = foldM(styProg.blocks, (trans, hb, i) => translatePair(varEnv, subEnv, subProg, trans, hb, i), initTrans());

  if (isLeft(res)) { return res; } // Return errors

  const trans = res.contents;
  const transWithNames = insertNames(trans); // TODO
  const transWithNamesAndLabels = insertLabels(transWithNames, labelMap); // TODO

  // COMBAK: Do this with plugins
  // const styValMap = styJsonToMap(styVals);
  // const transWithPlugins = evalPluginAccess(styValMap, transWithNamesAndLabels);
  // return Right(transWithPlugins);
  return Right(transWithNamesAndLabels);
};

//#endregion

// TODO: Improve this type signature
// export const compileStyle = (env: VarEnv, subAST: SubProg, styAST: StyProg): State => {
export const compileStyle = (stateJSON: any, styJSON: any): State => {

  const info = stateJSON.default.contents;
  console.log("compiling style (stateJSON)", info);

  console.log("compiled style with new parser", styJSON.default);

  // Types from compileTrio
  const state: State = info[0];
  // const env: VarEnv = info[1]; // This is redundant with subOut
  // const styProgInit: StyProg = info[2];
  const styProgInit: StyProg = styJSON.default;
  const subOut: SubOut = info[3];

  const subProg: SubProg = subOut[0];
  const varEnv: VarEnv = subOut[1][0];
  const subEnv: SubEnv = subOut[1][1];
  // TODO: Bring back `eqEnv`?
  const labelMap: LabelMap = subOut[2];

  console.log("subOut", subOut);

  // (Porting from `compileStyle` in `GenOptProblem`)

  // Check selectors; return list of selector environments (`checkSels`)
  const selEnvs = checkSelsAndMakeEnv(varEnv, styProgInit.blocks);

  console.log("selEnvs", selEnvs);

  // Find substitutions (`find_substs_prog`)
  const subss = findSubstsProg(varEnv, subEnv, subProg, styProgInit.blocks, selEnvs); // TODO: Use `eqEnv`

  console.log("substitutions", subss);

  // Name anon statements
  const styProg: StyProg = nameAnonStatements(styProgInit);

  console.log("old prog", styProgInit);
  console.log("new prog, substituted", styProg);

  // Translate style program
  const styVals: number[] = []; // COMBAK: Deal with style values when we have plugins
  const trans = translateStyProg(varEnv, subEnv, subProg, styProg, labelMap, styVals);

  // Gen opt problem and state
  // TODO <

  // Compute layering
  // TODO(@wodeni)

  return {} as State;

};
