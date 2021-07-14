import {
  appendStmt,
  applyBind,
  applyConstructor,
  applyFunction,
  applyPredicate,
  applyTypeDecl,
  ArgExpr,
  ArgStmtDecl,
  autoLabelStmt,
  domainToSubType,
  matchSignatures,
  nullaryTypeCons,
  replaceStmt,
} from "analysis/SubstanceAnalysis";
import { prettyStmt, prettySubstance } from "compiler/Substance";
import consola, { LogLevel } from "consola";
import { dummyIdentifier } from "engine/EngineUtils";
import { Map } from "immutable";
import { cloneDeep, compact, range, times, without } from "lodash";
import { createChoice } from "pandemonium/choice";
import { createRandom } from "pandemonium/random";
import seedrandom from "seedrandom";
import {
  checkAddStmt,
  checkAddStmts,
  checkReplaceExprName,
  checkSwapExprArgs,
  checkSwapStmtArgs,
  executeMutation,
  executeMutations,
  IMutation,
  Mutation,
  showOp,
  showOps,
} from "synthesis/Mutation";
import { Identifier } from "types/ast";
import {
  Arg,
  ConstructorDecl,
  DomainStmt,
  Env,
  FunctionDecl,
  PredicateDecl,
  Type,
  TypeConstructor,
  TypeDecl,
} from "types/domain";
import {
  ApplyConstructor,
  ApplyFunction,
  ApplyPredicate,
  Bind,
  Decl,
  SubExpr,
  SubPredArg,
  SubProg,
  SubRes,
  SubStmt,
  TypeConsApp,
} from "types/substance";
import { checkReplaceStmtName } from "./Mutation";

type RandomFunction = (min: number, max: number) => number;

const log = consola
  .create({ level: LogLevel.Debug })
  .withScope("Substance Synthesizer");

//#region Synthesizer setting types

type All = "*";
type ArgOption = "existing" | "generated" | "mixed";
type ArgReuse = "distinct" | "repeated";
type MatchSetting = string[] | All;
export interface DeclTypes {
  type: MatchSetting;
  predicate: MatchSetting;
  constructor: MatchSetting;
  function: MatchSetting;
}
export interface SynthesizerSetting {
  mutationCount: [number, number];
  argOption: ArgOption;
  argReuse: ArgReuse;
  weights: {
    type: number;
    predicate: number;
    constructor: number;
  };
  add: DeclTypes;
  delete: DeclTypes;
  edit: DeclTypes;
}

//#endregion

//#region Synthesis context

interface Candidates {
  types: Map<string, TypeDecl>;
  functions: Map<string, FunctionDecl>;
  predicates: Map<string, PredicateDecl>;
  constructors: Map<string, ConstructorDecl>;
}

export interface SynthesizedSubstance {
  prog: SubProg;
  ops: Mutation[];
}

export class SynthesisContext {
  names: Map<string, number>;
  declaredIDs: Map<string, Identifier[]>;
  prog: SubProg;
  numStmts: number;
  maxStmts: number;
  candidates: Candidates;
  subRes: SubRes | undefined;
  ops: Mutation[];
  template: SubProg | undefined;
  argCxt: Identifier[];

  constructor(subRes?: SubRes) {
    this.numStmts = 0;
    this.names = Map();
    this.declaredIDs = Map();
    this.maxStmts = 0;
    this.candidates = {
      types: Map(),
      constructors: Map(),
      functions: Map(),
      predicates: Map(),
    };
    this.subRes = subRes;
    this.prog = {
      tag: "SubProg",
      statements: [],
      nodeType: "SyntheticSubstance",
      children: [], // TODO: add statements as children
    };
    this.ops = [];
    this.argCxt = [];
  }

  loadTemplate = () => {
    // If there is a template substance program, load in the relevant info
    if (this.subRes) {
      const [subEnv, env] = this.subRes;
      this.template = cloneDeep(subEnv.ast);
      this.prog = cloneDeep(subEnv.ast);
      env.varIDs.forEach((id) => {
        const type = env.vars.get(id.value);
        this.addID(type!.name.value, id); // TODO: enforce the existence of var types
      });
      log.debug(`Loaded template:\n${prettySubstance(this.prog)}`);
    }
  };

  findCandidates = (env: Env, setting: DeclTypes) => {
    this.candidates = {
      types: filterBySetting(env.types, setting.type),
      functions: filterBySetting(env.functions, setting.function),
      predicates: filterBySetting(env.predicates, setting.predicate),
      constructors: filterBySetting(env.constructors, setting.constructor),
    };
  };

  irintCandidates = () => {
    console.log("types: ", ...this.candidates.types.keys());
    console.log("predicates: ", ...this.candidates.predicates.keys());
    console.log("functions: ", ...this.candidates.functions.keys());
    console.log("constructors: ", ...this.candidates.constructors.keys());
  };

  getCandidates = (type: DomainStmt["tag"]): Map<string, DomainStmt> => {
    switch (type) {
      case "TypeDecl":
        return this.candidates.types;
      case "ConstructorDecl":
        return this.candidates.constructors;
      case "FunctionDecl":
        return this.candidates.functions;
      case "PredicateDecl":
        return this.candidates.predicates;
      case undefined:
        return Map();
    }
    throw new Error(`${type} is not supported by the synthesizer`);
  };

  candidateTypes = (): DomainStmt["tag"][] =>
    declTypes.filter((type) => !this.getCandidates(type).isEmpty());

  //n append a statement to the generated program
  appendStmt = (stmt: SubStmt) => {
    this.prog = appendStmt(this.prog, stmt);
    this.numStmts++;
  };

  updateProg = (prog: SubProg): void => {
    this.prog = prog;
  };

  replaceStmt = (
    originalStmt: SubStmt,
    newStmt: SubStmt,
    mutationType: string
  ): void => {
    this.prog = replaceStmt(this.prog, originalStmt, newStmt);
    // COMBAK: fix this
    // this.ops.push({
    //   tag: "Replace",
    //   old: originalStmt,
    //   new: newStmt,
    //   mutationType,
    // });
  };

  removeStmt = (stmt: SubStmt) => {
    const index = this.prog.statements.indexOf(stmt);
    if (index > -1) {
      this.prog.statements.splice(index, 1);
    } else {
      throw new Error(
        `Statement cannot be removed because it doesn't exist: ${prettyStmt(
          stmt
        )}`
      );
    }
    // COMBAK: increment counter?
  };

  getOps = (): Mutation[] => this.ops;

  showOps = (): string => showOps(this.ops);

  addArg = (id: Identifier): void => {
    this.argCxt.push(id);
  };

  resetArgContext = (): void => {
    this.argCxt = [];
  };

  reset = (maxStmts: number) => {
    this.ops = [];
    this.maxStmts = maxStmts;
    this.names = Map();
    this.declaredIDs = Map();
    this.prog = {
      tag: "SubProg",
      statements: [],
      nodeType: "SyntheticSubstance",
      children: [],
    };
    this.argCxt = [];
    this.loadTemplate();
  };

  addID = (typeStr: string, id: Identifier) => {
    const ids = this.declaredIDs.get(typeStr);
    if (ids) {
      this.declaredIDs = this.declaredIDs.set(typeStr, [...ids, id]);
    } else {
      this.declaredIDs = this.declaredIDs.set(typeStr, [id]);
    }
  };

  /**
   * Remove a declared ID, NOTE: this should be called whenever we delete a Decl
   * statement from list of statements to keep them in sync.
   * @param typeStr either a Bind or Decl that is staged to be deleted
   * @param id the identifier that is being removed
   */
  removeID = (typeStr: string, id: Identifier) => {
    const ids = this.declaredIDs.get(typeStr);
    if (ids) {
      // keep all IDs that aren't id
      const newIDs = ids.filter((otherID) => otherID.value !== id.value);
      this.declaredIDs = this.declaredIDs.set(typeStr, newIDs);
    } else {
      log.warn(`Could not find any IDs for ${typeStr}`);
    }
  };

  findIDs = (typeStr: string, excludeList?: Identifier[]): Identifier[] => {
    const possibleIDs = this.declaredIDs.get(typeStr);
    if (possibleIDs) {
      const candidates = possibleIDs.filter((id) =>
        excludeList ? !excludeList.includes(id) : true
      );
      return candidates;
    } else return [];
  };

  generateID = (typeName: Identifier): Identifier => {
    const typeStr = typeName.value;
    const prefix = typeStr[0].toLowerCase();
    const index = this.nextIndex(prefix);
    const id: Identifier = dummyIdentifier(
      `${prefix}${index}`,
      "SyntheticSubstance"
    );
    this.addID(typeStr, id);
    log.debug(`generated an ID ${prefix}${index} for ${typeName.value}`);
    return id;
  };

  nextIndex = (prefix: string): number => {
    const lastIndex = this.names.get(prefix);
    if (lastIndex !== undefined) {
      this.names = this.names.set(prefix, lastIndex + 1);
      return lastIndex + 1;
    } else {
      this.names = this.names.set(prefix, 0);
      return 0;
    }
  };

  autoLabel = (): void => this.appendStmt(autoLabelStmt);
}

//#endregion

//#region Main synthesizer

interface WithStmts<T> {
  res: T;
  stmts: SubStmt[];
}

export class Synthesizer {
  env: Env;
  cxt: SynthesisContext;
  setting: SynthesizerSetting;
  private choice: <T>(array: Array<T>) => T;
  private random: RandomFunction;

  constructor(
    env: Env,
    setting: SynthesizerSetting,
    subRes?: SubRes,
    seed = "synthesizerSeed"
  ) {
    this.env = env;
    this.cxt = new SynthesisContext(subRes);
    this.setting = setting;
    const rng = seedrandom(seed);
    this.choice = createChoice(rng);
    this.random = createRandom(rng);
  }

  /**
   * Top-level function for generating multiple Substance programs.
   * @param numProgs number of Substance programs to generate
   * @returns an array of Substance programs and some metadata (e.g. mutation operation record)
   */
  generateSubstances = (numProgs: number): SynthesizedSubstance[] =>
    times(numProgs, () => {
      const sub = this.generateSubstance();
      return sub;
    });

  generateSubstance = (): SynthesizedSubstance => {
    const numStmts = this.random(...this.setting.mutationCount);
    this.cxt.reset(numStmts);
    times(numStmts, (n) => {
      this.mutateProgram();
      log.debug(
        `Mutation #${n} done. The program so far:\n${prettySubstance(
          this.cxt.prog
        )}`
      );
    });
    // add autolabel statement
    this.cxt.autoLabel();
    // DEBUG: report results
    log.info(prettySubstance(this.cxt.prog));
    log.info("Operations:\n", this.cxt.showOps());
    log.info("----------");
    return {
      prog: this.cxt.prog,
      ops: this.cxt.getOps(),
    };
  };

  findMutations = (stmt: SubStmt): Mutation[] => {
    log.debug(`Finding mutations for ${prettyStmt(stmt)}`);
    const ops: (Mutation | undefined)[] = [
      checkSwapStmtArgs(stmt, (p: ApplyPredicate) => {
        const indices = range(0, p.args.length);
        const elem1 = this.choice(indices);
        const elem2 = this.choice(without(indices, elem1));
        return [elem1, elem2];
      }),
      checkSwapExprArgs(stmt, (f: ArgExpr) => {
        const indices = range(0, f.args.length);
        const elem1 = this.choice(indices);
        const elem2 = this.choice(without(indices, elem1));
        return [elem1, elem2];
      }),
      checkReplaceStmtName(stmt, (p: ApplyPredicate) => {
        const matchingNames: string[] = matchSignatures(p, this.env).map(
          (decl) => decl.name.value
        );
        const options = without(matchingNames, p.name.value);
        const pick = this.choice(options);
        // TODO: check length of options
        return pick;
      }),
      checkReplaceExprName(stmt, (e: ArgExpr) => {
        const matchingNames: string[] = matchSignatures(e, this.env).map(
          (decl) => decl.name.value
        );
        const options = without(matchingNames, e.name.value);
        const pick = this.choice(options);
        // TODO: check length of options
        return pick;
      }),
    ];
    const mutations = compact(ops);
    log.debug(
      `Available mutations for ${prettyStmt(stmt)}:\n${showOps(mutations)}`
    );
    return mutations;
  };

  mutateProgram = (): void => {
    // const ops = ["add", "delete", "edit"];
    const ops = ["add", "edit"];
    const op = this.choice(ops);
    if (op === "add") {
      this.cxt.findCandidates(this.env, this.setting.add);
      this.addStmt();
    } else if (op === "delete") {
      this.cxt.findCandidates(this.env, this.setting.delete);
      this.deleteStmt();
    } else if (op === "edit") {
      this.cxt.findCandidates(this.env, this.setting.edit);
      this.editStmt();
    }
  };

  // TODO: add max iteration # to terminate the search if no applicable edit is found
  editStmt = (): void => {
    log.debug(`Picking a statement to edit...`);
    // pick a kind of statement to edit
    const chosenType = this.choice(this.cxt.candidateTypes());
    // get all possible types within this kind
    const candidates = [...this.cxt.getCandidates(chosenType).keys()];
    // choose a name
    const chosenName = this.choice(candidates);
    log.debug(
      `Chosen name is ${chosenName}, candidates ${candidates}, chosen type ${chosenType}`
    );
    const stmt = this.findStmt(chosenType, chosenName);
    if (stmt !== undefined) {
      log.debug(`Editing statement: ${prettyStmt(stmt)}`);
      // find all available mutations for the given statement
      const mutations = this.findMutations(stmt);
      log.debug(`Possible mutations:\n${showOps(mutations)}`);
      // if there's any valid mutation, pick one to execute
      if (mutations.length > 0) {
        const mutation: Mutation = this.choice(mutations);
        log.debug(`Picked mutation: ${showOp(mutation)}`);
        const newProg: SubProg = executeMutation(this.cxt.prog, mutation);
        this.cxt.ops.push(mutation);
        this.cxt.updateProg(newProg);
      } else this.editStmt(); // if there's no good option, repeat
    } else {
      log.debug(
        `Couldn't find a statement to edit. Repeating the edit action...`
      );
      this.editStmt(); // if there's no good option, repeat
    }
  };

  // editStmt = (op: Update["tag"]): void => {
  //   this.cxt.findCandidates(this.env, this.setting.edit);
  //   const chosenType = this.choice(this.cxt.candidateTypes());
  //   const candidates = [...this.cxt.getCandidates(chosenType).keys()];
  //   const chosenName = this.choice(candidates);
  //   const stmt = this.findStmt(chosenType, chosenName);
  //   if (stmt && (stmt.tag === "ApplyPredicate" || stmt.tag === "Bind")) {
  //     log.debug(`Editing statement: ${prettyStmt(stmt)}`);
  //     switch (op) {
  //       case "TypeChange": {
  //         const options = argMatches(stmt, this.env);
  //         if (options.length > 0) {
  //           const pick = this.choice(options);
  //           this.typeChange(stmt, pick);
  //         }
  //         break;
  //       }
  //     }
  //   } else {
  //     log.debug(
  //       `Failed to find a statement when editing. Candidates are: ${candidates}. Chosen name to find is ${chosenName} of ${chosenType}`
  //     );
  //   }
  // };

  //   // COMBAK: fix
  // typeChange = (oldStmt: ApplyPredicate | Bind, pick: ArgStmtDecl): void => {
  //   let newStmt = oldStmt;
  //   if (pick.tag === "PredicateDecl") {
  //     newStmt = this.generatePredicate(pick);
  //   } else if (pick.tag === "FunctionDecl") {
  //     newStmt = this.generateFunction(pick);
  //   } else {
  //     newStmt = this.generateConstructor(pick);
  //   }
  //   // remove old statement
  //   if (
  //     newStmt.tag === "Bind" &&
  //     oldStmt.tag === "Bind" &&
  //     newStmt.variable.type !== oldStmt.variable.type
  //   ) {
  //     // old bind was replaced by a bind with diff type
  //     this.cascadingDelete(oldStmt); // remove refs to the old bind
  //     newStmt = newStmt as Bind;
  //   } else {
  //     // otherwise we can simple delete
  //     this.cxt.removeStmt(oldStmt);
  //   }
  //   // this.cxt.ops.push({
  //   //   tag: "Replace",
  //   //   stmt: oldStmt,
  //   //   newStmt: newStmt,
  //   //   mutationType: "TypeChange",
  //   // });
  // };

  // NOTE: every synthesizer that 'addStmt' calls is expected to append its result to the AST, instead of just returning it. This is because certain lower-level functions are allowed to append new statements (e.g. 'generateArg'). Otherwise, we could write this module as a combinator.
  addStmt = (): void => {
    this.cxt.findCandidates(this.env, this.setting.add);
    const chosenType = this.choice(this.cxt.candidateTypes());
    let possibleOps: Mutation[] | undefined;
    log.debug(`Adding statement of ${chosenType} type`);
    if (chosenType === "TypeDecl") {
      const op = checkAddStmt(
        this.cxt.prog,
        this.cxt,
        (cxt: SynthesisContext) => {
          const type: TypeDecl = this.choice(
            cxt.candidates.types.toArray().map(([, b]) => b)
          );
          return this.generateType(type, cxt);
        }
      );
      possibleOps = op ? [op] : undefined;
    } else if (chosenType === "PredicateDecl") {
      possibleOps = checkAddStmts(
        this.cxt.prog,
        this.cxt,
        (cxt: SynthesisContext) => {
          const pred = this.choice(
            this.cxt.candidates.predicates.toArray().map(([, b]) => b)
          );
          const { res, stmts } = this.generatePredicate(pred, cxt);
          return [...stmts, res];
        }
      );
    } else if (chosenType === "FunctionDecl") {
      possibleOps = checkAddStmts(
        this.cxt.prog,
        this.cxt,
        (cxt: SynthesisContext) => {
          const func = this.choice(
            this.cxt.candidates.functions.toArray().map(([, b]) => b)
          );
          const { res, stmts } = this.generateFunction(func, cxt);
          return [...stmts, res];
        }
      );
    } else if (chosenType === "ConstructorDecl") {
      possibleOps = checkAddStmts(
        this.cxt.prog,
        this.cxt,
        (cxt: SynthesisContext) => {
          const cons = this.choice(
            this.cxt.candidates.constructors.toArray().map(([, b]) => b)
          );
          const { res, stmts } = this.generateConstructor(cons, cxt);
          return [...stmts, res];
        }
      );
    }
    if (possibleOps) {
      log.debug(`Mutations selected for add:\n${showOps(possibleOps)}`);
      const newProg: SubProg = executeMutations(this.cxt.prog, possibleOps);
      this.cxt.ops.concat(possibleOps);
      this.cxt.updateProg(newProg);
    }
  };

  deleteStmt = (): void => {
    log.debug("Deleting statement");
    const chosenType = this.choice(this.cxt.candidateTypes());
    const candidates = [...this.cxt.getCandidates(chosenType).keys()];
    const chosenName = this.choice(candidates);
    const stmt = this.findStmt(chosenType, chosenName);
    if (stmt) {
      if (stmt.tag === "Bind" || stmt.tag === "Decl") {
        // if statement returns value, delete all refs to value
        this.cascadingDelete(stmt).forEach((s) => {
          // COMBAK: fix
          // this.cxt.ops.push({ tag: "Delete", stmt: s });
        });
      } else {
        this.cxt.removeStmt(stmt);
        // COMBAK: fix
        // this.cxt.ops.push({ tag: "Delete", stmt });
      }
    }
  };

  /**
   * Given a statement which returns a value
   * that is staged to be deleted, iteratively find and delete any other
   * statements that would use the statement's returned variable
   * TODO: Refactor to a pure function and put in SubstanceAnalysis.ts!
   * @param dec either a Bind or Decl that is staged to be deleted
   */
  cascadingDelete = (dec: Bind | Decl): SubStmt[] => {
    const findArg = (s: ApplyPredicate, ref: Identifier | undefined) =>
      ref &&
      s.args.filter((a) => {
        return a.tag === ref.tag && a.value === ref.value;
      }).length > 0;
    const ids = [dec.tag === "Bind" ? dec.variable : dec.name]; // stack of variables to delete
    const removedStmts: SubStmt[] = [];
    log.debug("before cascading", this.cxt.prog.statements.length);
    while (ids.length > 0) {
      const id = ids.pop();
      log.debug("looking for instances of:", id?.value);
      // look for statements that take id as arg
      const toDelete = this.cxt.prog.statements.filter((s) => {
        if (s.tag === "Bind") {
          const expr = (s.expr as unknown) as ApplyPredicate;
          const willDelete = findArg(expr, id);
          // push its return value IF bind will be deleted
          if (willDelete) ids.push(s.variable);
          // delete if arg is found in either return type or args
          return willDelete || s.variable.value === id?.value;
        } else if (s.tag === "ApplyPredicate") {
          return findArg(s, id);
        } else if (s.tag === "Decl") {
          return s.name === id;
        }
      });
      log.debug(`stmts with id: ${id?.value}, num stmts: ${toDelete.length}`);
      // remove list of filtered statements
      toDelete.forEach((stmt) => {
        // remove Identifier from added IDs
        if (stmt.tag === "Decl") {
          this.cxt.removeID(stmt.type.name.value, stmt.name);
        }
        this.cxt.removeStmt(stmt);
        removedStmts.push(stmt);
      });
    }
    log.debug("final stmts", this.cxt.prog.statements.length);
    return removedStmts;
  };

  // TODO: add an option to distinguish between edited vs original statements?
  findStmt = (
    stmtType: DomainStmt["tag"],
    name: string
  ): SubStmt | undefined => {
    const stmts = this.cxt.prog.statements.filter((s) => {
      const subType = domainToSubType(stmtType);
      if (s.tag === "Bind") {
        const expr = s.expr;
        return expr.tag === subType && expr.name.value === name;
      } else if (s.tag === "Decl") {
        return s.tag === subType && s.type.name.value === name;
      } else {
        return s.tag === subType && s.name.value === name;
      }
    });
    if (stmts.length > 0) {
      const stmt = this.choice(stmts);
      return stmt;
    } else {
      log.debug(
        `Warning: cannot find a ${stmtType} statement with name ${name}.`
      );
      return undefined;
    }
  };

  generateType = (type: TypeDecl, cxt: SynthesisContext): Decl => {
    const typeCons = applyTypeDecl(type);
    const name = cxt.generateID(typeCons.name);
    const stmt: Decl = {
      tag: "Decl",
      nodeType: "SyntheticSubstance",
      children: [],
      type: typeCons,
      name,
    };
    return stmt;
  };

  generatePredicate = (
    pred: PredicateDecl,
    cxt: SynthesisContext
  ): WithStmts<ApplyPredicate> => {
    const { res, stmts }: WithStmts<SubPredArg[]> = this.generatePredArgs(
      pred.args,
      cxt
    );
    const p: ApplyPredicate = applyPredicate(pred, res);
    return { res: p, stmts };
  };

  generateFunction = (
    func: FunctionDecl,
    cxt: SynthesisContext
  ): WithStmts<Bind> => {
    const { res: args, stmts: decls }: WithStmts<SubExpr[]> = this.generateArgs(
      func.args,
      cxt
    );
    const rhs: ApplyFunction = applyFunction(func, args);
    // find the `TypeDecl` for the output type
    const outputType = func.output.type as TypeConstructor;
    const outputTypeDecl: TypeDecl | undefined = cxt.candidates.types.find(
      (decl, typeName) => typeName === outputType.name.value
    );
    if (outputTypeDecl) {
      // TODO: choose between generating vs. reusing
      const lhsDecl: Decl = this.generateType(outputTypeDecl, cxt);
      const lhs: Identifier = lhsDecl.name;
      const stmt: Bind = applyBind(lhs, rhs);
      return { res: stmt, stmts: [...decls, lhsDecl] };
    } else
      throw new Error(
        `${outputType.name.value} is not found in the candidate list`
      );
  };

  generateConstructor = (
    cons: ConstructorDecl,
    cxt: SynthesisContext
  ): WithStmts<Bind> => {
    const { res: args, stmts: decls }: WithStmts<SubExpr[]> = this.generateArgs(
      cons.args,
      cxt
    );
    const rhs: ApplyConstructor = applyConstructor(cons, args);
    const outputType = cons.output.type as TypeConstructor;
    const outputTypeDecl: TypeDecl | undefined = this.cxt.candidates.types.find(
      (decl, typeName) => typeName === outputType.name.value
    );
    if (outputTypeDecl) {
      // TODO: choose between generating vs. reusing
      const lhsDecl: Decl = this.generateType(outputTypeDecl, cxt);
      const lhs: Identifier = lhsDecl.name;
      const stmt: Bind = applyBind(lhs, rhs);
      return { res: stmt, stmts: [...decls, lhsDecl] };
    } else
      throw new Error(
        `${outputType.name.value} is not found in the candidate list`
      );
  };

  generateArgs = (args: Arg[], cxt: SynthesisContext): WithStmts<SubExpr[]> => {
    const res = args.map((arg) =>
      this.generateArg(arg, cxt, this.setting.argOption, this.setting.argReuse)
    );
    cxt.resetArgContext();
    return {
      res: res.map((r) => r.res).flat(),
      stmts: res.map((r) => r.stmts).flat(),
    };
  };

  generateArg = (
    arg: Arg,
    cxt: SynthesisContext,
    option: ArgOption,
    reuseOption: ArgReuse
  ): WithStmts<SubExpr> => {
    const argType: Type = arg.type;
    if (argType.tag === "TypeConstructor") {
      switch (option) {
        case "existing": {
          // TODO: clean up the logic
          const possibleIDs =
            reuseOption === "distinct"
              ? cxt.findIDs(argType.name.value, cxt.argCxt)
              : cxt.findIDs(argType.name.value);
          const existingID = this.choice(possibleIDs);
          log.debug(
            `generating an argument with possbilities ${possibleIDs.map(
              (i) => i.value
            )}`
          );
          if (!existingID) {
            return this.generateArg(arg, cxt, "generated", reuseOption);
          } else {
            cxt.addArg(existingID);
            return { res: existingID, stmts: [] };
          }
        }
        case "generated":
          const argTypeDecl = cxt.candidates.types.get(argType.name.value);
          if (argTypeDecl) {
            const decl = this.generateType(argTypeDecl, cxt);
            return { res: decl.name, stmts: [decl] };
          } else {
            throw new Error(
              `${argType.name.value} not found in the candidate list`
            );
          }
        case "mixed":
          return this.generateArg(
            arg,
            cxt,
            this.choice(["existing", "generated"]),
            reuseOption
          );
      }
    } else {
      throw new Error(`${argType.tag} not supported for argument generation`);
    }
  };

  generatePredArgs = (
    args: Arg[],
    cxt: SynthesisContext
  ): WithStmts<SubPredArg[]> => {
    const res = args.map((arg) =>
      this.generatePredArg(
        arg,
        cxt,
        this.setting.argOption,
        this.setting.argReuse
      )
    );
    return {
      res: res.map((r) => r.res).flat(),
      stmts: res.map((r) => r.stmts).flat(),
    };
  };

  generatePredArg = (
    arg: Arg,
    cxt: SynthesisContext,
    option: ArgOption,
    reuseOption: ArgReuse
  ): WithStmts<SubPredArg> => {
    const argType: Type = arg.type;
    if (argType.tag === "Prop") {
      // TODO: check if the candidates are correct
      const pred = this.choice(
        this.cxt.candidates.predicates.toArray().map(([, b]) => b)
      );
      return this.generatePredicate(pred, cxt);
    } else {
      return this.generateArg(arg, cxt, option, reuseOption);
    }
  };

  getTemplate = (): SubProg | undefined =>
    this.cxt.template
      ? appendStmt(this.cxt.template, autoLabelStmt)
      : undefined;
}

//#endregion

//#region Helpers

const declTypes: DomainStmt["tag"][] = [
  "ConstructorDecl",
  "FunctionDecl",
  "TypeDecl",
  "PredicateDecl",
];

const filterBySetting = <T>(
  decls: Map<string, T>,
  setting: MatchSetting
): Map<string, T> => {
  if (setting === "*") {
    return decls;
  } else {
    return decls.filter((_, key) => setting.includes(key));
  }
};

//#endregion
