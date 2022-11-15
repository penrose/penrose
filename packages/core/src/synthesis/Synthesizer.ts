import {
  appendStmt,
  applyBind,
  applyConstructor,
  applyFunction,
  applyPredicate,
  applyTypeDecl,
  ArgExpr,
  argMatches,
  ArgStmtDecl,
  autoLabelStmt,
  cascadingDelete,
  desugarAutoLabel,
  domainToSubType,
  findTypes,
  matchSignatures,
  nullaryTypeCons,
  SubStmtKind,
} from "analysis/SubstanceAnalysis";
import { subTypesOf } from "compiler/Domain";
import { prettyStmt, prettySubstance } from "compiler/Substance";
import consola, { LogLevel } from "consola";
import { dummyIdentifier } from "engine/EngineUtils";
import im from "immutable";
import { cloneDeep, compact, range, times, without } from "lodash";
import { createChoice } from "pandemonium/choice";
import { createRandom } from "pandemonium/random";
import seedrandom from "seedrandom";
import {
  Add,
  addMutation,
  checkAddStmt,
  checkAddStmts,
  checkChangeExprType,
  checkChangeStmtType,
  checkDeleteStmt,
  checkReplaceExprName,
  checkReplaceStmtName,
  checkSwapExprArgs,
  checkSwapInExprArgs,
  checkSwapInStmtArgs,
  checkSwapStmtArgs,
  Delete,
  deleteMutation,
  executeMutations,
  Mutation,
  MutationGroup,
  showMutations,
} from "synthesis/Mutation";
import { A, Identifier } from "types/ast";
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
  Func,
  SubExpr,
  SubPredArg,
  SubProg,
  SubRes,
  SubStmt,
  TypeConsApp,
} from "types/substance";

type RandomFunction = (min: number, max: number) => number;

const log = consola
  .create({ level: LogLevel.Debug })
  .withScope("Substance Synthesizer");

//#region Synthesizer setting types

type All = "*";
type ArgOption = "existing" | "generated" | "mixed";
type ArgReuse = "distinct" | "repeated";
export type MatchSetting = string[] | All;
export type DeclTypes = {
  [t in SubStmtKind]: MatchSetting;
};
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
export interface SynthesisContext {
  names: im.Map<string, number>;
  declaredIDs: im.Map<string, Identifier<A>[]>;
  generatedNames: im.Map<string, number>;
  argOption: ArgOption;
  argReuse: ArgReuse;
  env: Env;
  choice: <T>(array: Array<T>) => T;
}
export interface SynthesizedSubstance {
  prog: SubProg<A>;
  ops: Mutation[];
}

interface IDList {
  ids: Identifier<A>[];
}

export const initContext = (
  env: Env,
  argOption: ArgOption,
  argReuse: ArgReuse,
  randomSeed: string
): SynthesisContext => {
  const rng: seedrandom.prng = seedrandom(randomSeed);
  const ctx: SynthesisContext = {
    generatedNames: im.Map<string, number>(),
    argOption,
    argReuse,
    names: im.Map<string, number>(),
    declaredIDs: im.Map<string, Identifier<A>[]>(),
    choice: createChoice(rng),
    env,
  };
  return env.varIDs.reduce((c, id) => {
    const type = env.vars.get(id.value);
    // TODO: enforce the existence of var types
    return addID(c, type!.name.value, id);
  }, ctx);
};

export const filterContext = (
  ctx: SynthesisContext,
  setting: DeclTypes,
  subProg?: SubProg<A> // optionally filter by the declared Substance types too
): SynthesisContext => {
  const filterDecls = <T>(
    decls: im.Map<string, T>,
    setting: MatchSetting,
    substanceTypes?: string[]
  ): im.Map<string, T> => {
    const filteredBySetting: im.Map<string, T> =
      setting === "*" ? decls : decls.filter((_, key) => setting.includes(key));
    if (substanceTypes) {
      return filteredBySetting.filter((_, key) => substanceTypes.includes(key));
    } else {
      return filteredBySetting;
    }
  };

  if (subProg) {
    const subTypes = findTypes(subProg);
    return {
      ...ctx,
      env: {
        ...ctx.env,
        types: filterDecls(ctx.env.types, setting.type, subTypes.type),
        functions: filterDecls(
          ctx.env.functions,
          setting.function,
          subTypes.function
        ),
        predicates: filterDecls(
          ctx.env.predicates,
          setting.predicate,
          subTypes.predicate
        ),
        constructors: filterDecls(
          ctx.env.constructors,
          setting.constructor,
          subTypes.constructor
        ),
      },
    };
  } else {
    // TODO: dedup
    return {
      ...ctx,
      env: {
        ...ctx.env,
        types: filterDecls(ctx.env.types, setting.type),
        functions: filterDecls(ctx.env.functions, setting.function),
        predicates: filterDecls(ctx.env.predicates, setting.predicate),
        constructors: filterDecls(ctx.env.constructors, setting.constructor),
      },
    };
  }
};

export const showEnv = (env: Env): string =>
  [
    `types: ${[...env.types.keys()]}`,
    `predicates: ${[...env.predicates.keys()]}`,
    `functions: ${[...env.functions.keys()]}`,
    `constructors: ${[...env.constructors.keys()]}`,
  ].join("\n");

const getDecls = (
  ctx: SynthesisContext,
  type: DomainStmt<A>["tag"]
): im.Map<string, DomainStmt<A>> => {
  const { env } = ctx;
  switch (type) {
    case "TypeDecl":
      return env.types;
    case "ConstructorDecl":
      return env.constructors;
    case "FunctionDecl":
      return env.functions;
    case "PredicateDecl":
      return env.predicates;
    case undefined:
      return im.Map<string, DomainStmt<A>>();
  }
  throw new Error(`${type} is not found in the environment`);
};

// return types that are declared in the context
const nonEmptyDecls = (ctx: SynthesisContext): DomainStmt<A>["tag"][] =>
  declTypes.filter((type) => !getDecls(ctx, type).isEmpty());

/**
 * Add a newly declared ID.
 *
 * NOTE: this should be called whenever we add a `Decl`
 * statement from list of statements to keep them in sync.
 * @param typeStr the type associated with the ID
 * @param id the identifier that is being removed
 */
export const addID = (
  ctx: SynthesisContext,
  typeStr: string,
  id: Identifier<A>
): SynthesisContext => {
  const ids = ctx.declaredIDs.get(typeStr);
  if (ids) {
    return {
      ...ctx,
      declaredIDs: ctx.declaredIDs.set(typeStr, [...ids, id]),
    };
  } else {
    return {
      ...ctx,
      declaredIDs: ctx.declaredIDs.set(typeStr, [id]),
    };
  }
};

/**
 * Remove a declared ID.
 *
 * NOTE: this should be called whenever we delete a `Decl`
 * statement from list of statements to keep them in sync.
 * @param typeStr the type associated with the ID
 * @param id the identifier that is being removed
 */
export const removeID = (
  ctx: SynthesisContext,
  typeStr: string,
  id: Identifier<A>
): SynthesisContext => {
  const ids = ctx.declaredIDs.get(typeStr);
  if (ids) {
    // keep all IDs that aren't id
    const newIDs = ids.filter((otherID) => otherID.value !== id.value);
    return {
      ...ctx,
      declaredIDs: ctx.declaredIDs.set(typeStr, newIDs),
    };
  } else {
    log.warn(`Could not find any IDs for ${typeStr} ${id.value}`);
    return ctx;
  }
};

const findIDs = (
  ctx: SynthesisContext,
  typeStrs: string[],
  excludeList?: Identifier<A>[]
): Identifier<A>[] => {
  const possibleIDs: Identifier<A>[] = compact(
    typeStrs.flatMap((typeStr) => ctx.declaredIDs.get(typeStr))
  );
  const candidates = possibleIDs.filter((id) =>
    excludeList ? !excludeList.includes(id) : true
  );
  return candidates;
};

const generateID = (
  ctx: SynthesisContext,
  typeName: Identifier<A>
): WithContext<Identifier<A>> => {
  const typeStr = typeName.value;
  const prefix = typeStr[0].toLowerCase();
  // find the appropriate index for the generated ID
  let index;
  const lastIndex = ctx.generatedNames.get(prefix);
  if (lastIndex !== undefined) {
    index = lastIndex + 1;
  } else {
    index = 0;
  }
  const id: Identifier<A> = dummyIdentifier(
    `${prefix}${index}`,
    "SyntheticSubstance"
  );
  const newCtx = addID(ctx, typeStr, id);
  return {
    res: id,
    ctx: {
      ...newCtx,
      generatedNames: newCtx.generatedNames.set(prefix, index),
    },
  };
};

const autoLabel = (prog: SubProg<A>): SubProg<A> =>
  appendStmt(prog, autoLabelStmt);

//#endregion

//#region Main synthesizer
interface WithStmts<T> {
  res: T;
  ctx: SynthesisContext;
  stmts: SubStmt<A>[];
}

export interface WithContext<T> {
  res: T;
  ctx: SynthesisContext;
}

export class Synthesizer {
  env: Env;
  template: SubProg<A>;
  setting: SynthesizerSetting;
  // names: im.Map<string, number>;
  currentProg: SubProg<A>;
  currentMutations: Mutation[];
  rng: seedrandom.prng;
  private choice: <T>(array: Array<T>) => T;
  private random: RandomFunction;

  constructor(
    env: Env,
    setting: SynthesizerSetting,
    subRes?: SubRes,
    seed = "synthesizerSeed"
  ) {
    // If there is a template substance program, load in the relevant info
    if (subRes) {
      const [subEnv, env] = subRes;
      this.env = env;
      this.template = cloneDeep(subEnv.ast);
      log.debug(`Loaded template:\n${prettySubstance(this.template)}`);
    } else {
      this.env = env;
      this.template = {
        tag: "SubProg",
        statements: [],
        nodeType: "SyntheticSubstance",
      };
    }
    // initialize the current program as the template after pre-processing
    this.currentProg = desugarAutoLabel(cloneDeep(this.template), env);
    this.setting = setting;
    this.currentMutations = [];
    // use the seed to create random generation functions
    this.rng = seedrandom(seed);
    this.choice = createChoice(this.rng);
    this.random = createRandom(this.rng);
  }

  reset = (): void => {
    this.currentProg = desugarAutoLabel(cloneDeep(this.template), this.env);
    this.currentMutations = [];
  };

  showMutations = (): string => showMutations(this.currentMutations);

  updateProg = (prog: SubProg<A>): void => {
    this.currentProg = prog;
  };

  getTemplate = (): SubProg<A> | undefined =>
    // this.template ? appendStmt(this.template, autoLabelStmt) : undefined;
    this.template ?? undefined;

  /**
   * Top-level function for generating multiple Substance programs.
   * @param numProgs number of Substance programs to generate
   * @returns an array of Substance programs and some metadata (e.g. mutation operation record)
   */
  generateSubstances = (numProgs: number): SynthesizedSubstance[] =>
    times(numProgs, (n: number) => {
      const sub = this.generateSubstance();
      // DEBUG: report results
      log.info(
        `Synthesized program #${n}\n${prettySubstance(this.currentProg)}`
      );
      log.info("Operations:\n", this.showMutations());
      log.info("----------");
      // reset synthesizer after generating each Substance diagram
      this.reset();
      return sub;
    });

  generateSubstance = (): SynthesizedSubstance => {
    const numStmts = this.random(...this.setting.mutationCount);
    range(numStmts).reduce(
      (ctx: SynthesisContext, n: number): SynthesisContext => {
        const newCtx = this.mutateProgram(ctx);
        log.debug(
          `Mutation #${n} done. The program so far:\n${prettySubstance(
            this.currentProg
          )}`
        );
        return newCtx;
      },
      initContext(
        this.env,
        this.setting.argOption,
        this.setting.argReuse,
        this.rng().toString() // HACK: generate a new random seed deterministically per context
      )
    );
    // add autolabel statement
    // TODO: find out what to label
    // this.updateProg(autoLabel(this.currentProg));
    return {
      prog: this.currentProg,
      ops: this.currentMutations,
    };
  };

  /**
   * Find a list of possible mutations for the current Substance program stored in the context, and execute one of the mutations.
   */
  mutateProgram = (ctx: SynthesisContext): SynthesisContext => {
    const addCtx = filterContext(ctx, this.setting.add);
    const addOps = this.enumerateAdd(addCtx);
    // NOTE: filter deletes by the template Substance program too
    const deleteCtx = filterContext(ctx, this.setting.delete, this.template);
    const deleteOps = this.enumerateDelete(deleteCtx);
    // NOTE: filter edits by the template Substance program too
    const editCtx = filterContext(ctx, this.setting.edit, this.template);
    const editOps = this.enumerateUpdate(editCtx);
    const mutations: MutationGroup[] = [addOps, deleteOps, ...editOps].filter(
      (ops) => ops.length > 0
    );
    if (mutations.length > 0) {
      log.debug(
        `Possible mutations: ${mutations.map(showMutations).join("\n")}`
      );
      const mutationGroup: MutationGroup = this.choice(mutations);
      log.debug(`Picked mutation group: ${showMutations(mutationGroup)}`);
      // TODO: check if the ctx used is correct
      const { res: prog, ctx: newCtx } = executeMutations(
        mutationGroup,
        this.currentProg,
        ctx
      );
      this.currentMutations.push(...mutationGroup);
      this.updateProg(prog);
      return newCtx;
    } else {
      log.debug("No mutations found.");
      return ctx;
    }
  };

  findMutations = (
    stmt: SubStmt<A>,
    ctx: SynthesisContext
  ): MutationGroup[] => {
    log.debug(`Finding mutations for ${prettyStmt(stmt)}`);
    // NOTE: we can also enumerate mutations for a given statement instead of greedily finding them
    // const ops = enumerateMutations(stmt, this.currentProg, ctx);
    const ops: (Mutation | undefined)[] = [
      checkSwapStmtArgs(stmt, (p: ApplyPredicate<A>) => {
        const indices = range(0, p.args.length);
        const elem1 = this.choice(indices);
        const elem2 = this.choice(without(indices, elem1));
        return [elem1, elem2];
      }),
      checkSwapExprArgs(stmt, (f: ArgExpr<A>) => {
        const indices = range(0, f.args.length);
        const elem1 = this.choice(indices);
        const elem2 = this.choice(without(indices, elem1));
        return [elem1, elem2];
      }),
      checkReplaceStmtName(stmt, (p: ApplyPredicate<A>) => {
        const matchingNames: string[] = matchSignatures(p, ctx.env).map(
          (decl) => decl.name.value
        );
        const options = without(matchingNames, p.name.value);
        if (options.length > 0) {
          return this.choice(options);
        } else return undefined;
      }),
      checkReplaceExprName(stmt, (e: ArgExpr<A>) => {
        const matchingNames: string[] = matchSignatures(e, ctx.env).map(
          (decl) => decl.name.value
        );
        const options = without(matchingNames, e.name.value);
        if (options.length > 0) {
          return this.choice(options);
        } else return undefined;
      }),
      checkSwapInStmtArgs(
        stmt,
        ctx,
        (
          options: im.Map<string, Identifier<A>[]>
        ): Identifier<A> | undefined => {
          const varId = this.choice([...options.keys()]);
          const swapOptions = options.get(varId);
          return swapOptions ? this.choice(swapOptions) : undefined;
        },
        (p: ApplyPredicate<A>) => {
          const indices = range(0, p.args.length);
          return this.choice(indices);
        }
      ),
      checkSwapInExprArgs(
        stmt,
        ctx,
        (
          options: im.Map<string, Identifier<A>[]>
        ): Identifier<A> | undefined => {
          const varId = this.choice([...options.keys()]);
          const swapOptions = options.get(varId);
          return swapOptions ? this.choice(swapOptions) : undefined;
        },
        (p: ApplyFunction<A> | ApplyConstructor<A> | Func<A>) => {
          const indices = range(0, p.args.length);
          return this.choice(indices);
        }
      ),
      checkChangeStmtType(
        stmt,
        ctx,
        (oldStmt: ApplyPredicate<A>, ctx: SynthesisContext) => {
          const options = argMatches(oldStmt, ctx.env);
          if (options.length > 0) {
            const pick = this.choice(options);
            const { res, stmts, ctx: newCtx } = generateArgStmt(
              pick,
              ctx,
              oldStmt.args
            );
            const deleteOp: Delete = deleteMutation(oldStmt, newCtx);
            const addOps: Add[] = stmts.map((s) => addMutation(s, newCtx));
            return {
              newStmt: res,
              additionalMutations: [deleteOp, ...addOps],
            };
          } else return undefined;
        }
      ),
      checkChangeExprType(
        stmt,
        ctx,
        (oldStmt: Bind<A>, oldExpr: ArgExpr<A>, ctx: SynthesisContext) => {
          const options = argMatches(oldStmt, ctx.env);
          if (options.length > 0) {
            const pick = this.choice(options);
            const { res, stmts, ctx: newCtx } = generateArgStmt(
              pick,
              ctx,
              oldExpr.args
            );
            let toDelete: SubStmt<A>[];
            // remove old statement
            if (
              res.tag === "Bind" &&
              res.variable.type !== oldStmt.variable.type
            ) {
              // old bind was replaced by a bind with diff type
              toDelete = cascadingDelete(oldStmt, this.currentProg); // remove refs to the old bind
            } else {
              toDelete = [oldStmt];
            }
            const deleteOps: Delete[] = toDelete.map((s) => deleteMutation(s));
            const addOps: Add[] = stmts.map((s) => addMutation(s, newCtx));
            return {
              newStmt: res,
              additionalMutations: [...deleteOps, ...addOps],
            };
          } else return undefined;
        }
      ),
    ];
    const mutations = compact(ops);
    log.debug(
      `Available mutations for ${prettyStmt(stmt)}:\n${showMutations(
        mutations
      )}`
    );
    return mutations.map((m) => [m]);
  };

  /**
   * Pick a random statement in the Substance program and enumerate all the applicable mutations.
   * @returns a list of mutation groups, each representing a series of `Update` mutations
   */
  enumerateUpdate = (ctx: SynthesisContext): MutationGroup[] => {
    log.debug(`Picking a statement to edit...`);
    // pick a kind of statement to edit
    const chosenType = this.choice(nonEmptyDecls(ctx));
    // get all possible types within this kind
    const candidates = [...getDecls(ctx, chosenType).keys()];
    // choose a name
    const chosenName = this.choice(candidates);
    log.debug(
      `Chosen name is ${chosenName}, candidates ${candidates}, chosen type ${chosenType}`
    );
    const stmt = this.findStmt(chosenType, chosenName);
    if (stmt !== undefined) {
      // find all available edit mutations for the given statement
      const mutations = this.findMutations(stmt, ctx);
      log.debug(
        `Possible update mutations for ${prettyStmt(
          stmt
        )} are:\n${mutations.map(showMutations).join("\n")}`
      );
      return mutations;
    } else return [];
  };

  /**
   * From the configuration, pick one Substance construct to generate, and return the new construct along with all other related constructs as a group of `Add` mutations.
   * @returns a group of `Add` mutations
   */
  enumerateAdd = (ctx: SynthesisContext): MutationGroup => {
    const chosenType = this.choice(nonEmptyDecls(ctx));
    log.debug(`Picking a statement to add from ${nonEmptyDecls(ctx)}`);
    let possibleOps: MutationGroup | undefined;
    log.debug(`Adding statement of ${chosenType} type`);
    if (chosenType === "TypeDecl") {
      const op = checkAddStmt(
        this.currentProg,
        ctx,
        (ctx: SynthesisContext) => {
          const type: TypeDecl<A> = this.choice(
            ctx.env.types.toArray().map(([, b]) => b)
          );
          return generateDecl(type, ctx);
        }
      );
      possibleOps = op ? [op] : undefined;
    } else if (chosenType === "PredicateDecl") {
      possibleOps = checkAddStmts(
        this.currentProg,
        ctx,
        (ctx: SynthesisContext) => {
          const pred = this.choice(
            ctx.env.predicates.toArray().map(([, b]) => b)
          );
          const { res, stmts, ctx: newCtx } = generatePredicate(pred, ctx);
          return {
            res: [...stmts, res],
            ctx: newCtx,
          };
        }
      );
    } else if (chosenType === "FunctionDecl") {
      possibleOps = checkAddStmts(
        this.currentProg,
        ctx,
        (ctx: SynthesisContext) => {
          const func = this.choice(
            ctx.env.functions.toArray().map(([, b]) => b)
          );
          const { res, stmts, ctx: newCtx } = generateFunction(func, ctx);
          return {
            res: [...stmts, res],
            ctx: newCtx,
          };
        }
      );
    } else if (chosenType === "ConstructorDecl") {
      possibleOps = checkAddStmts(
        this.currentProg,
        ctx,
        (ctx: SynthesisContext) => {
          const cons = this.choice(
            ctx.env.constructors.toArray().map(([, b]) => b)
          );
          const { res, stmts, ctx: newCtx } = generateConstructor(cons, ctx);
          return {
            res: [...stmts, res],
            ctx: newCtx,
          };
        }
      );
    }
    if (possibleOps) {
      log.debug(`Found mutations for add:\n${showMutations(possibleOps)}`);
      return possibleOps;
    } else return [];
  };

  /**
   * From the configuration, pick one Substance statement to delete, and return one or more `Delete` mutations depending on if there will be cascading delete.
   * @returns a group of `Delete` mutations
   */
  enumerateDelete = (ctx: SynthesisContext): MutationGroup => {
    log.debug("Deleting statement");
    const chosenType = this.choice(nonEmptyDecls(ctx));
    const candidates = [...getDecls(ctx, chosenType).keys()];
    const chosenName = this.choice(candidates);
    log.debug(
      `Chosen name is ${chosenName}, candidates ${candidates}, chosen type ${chosenType}`
    );
    const stmt = this.findStmt(chosenType, chosenName);
    let possibleOps: Mutation[] = [];
    if (stmt) {
      if (stmt.tag === "Bind" || stmt.tag === "Decl") {
        // if statement returns value, delete all refs to value
        cascadingDelete(stmt, this.currentProg).forEach((s) => {
          const del = checkDeleteStmt(this.currentProg, s);
          if (del) {
            possibleOps.push(del);
          }
        });
      } else {
        const del = checkDeleteStmt(this.currentProg, stmt);
        possibleOps = del ? [del] : [];
      }
    }
    if (possibleOps.length > 0) {
      log.debug(`Found mutations for delete:\n${showMutations(possibleOps)}`);
    }
    return possibleOps;
  };

  // TODO: add an option to distinguish between edited vs original statements?
  findStmt = (
    stmtType: DomainStmt<A>["tag"],
    name: string
  ): SubStmt<A> | undefined => {
    const stmts = this.currentProg.statements.filter((s) => {
      const subType = domainToSubType(stmtType);
      if (s.tag === "Bind") {
        const expr = s.expr;
        log.debug(
          `filtering stmt ${prettyStmt(s)} to find ${name} of ${subType}`,
          expr
        );
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
      log.debug(`Available statements:\n${prettySubstance(this.currentProg)}`);
      return undefined;
    }
  };
}

//#endregion

//#region Statement generator
// TODO: add configuration options for `choice`

export const generateArgStmt = (
  decl: ArgStmtDecl<A>,
  ctx: SynthesisContext,
  args?: SubExpr<A>[] | SubPredArg<A>[]
): WithStmts<Bind<A> | ApplyPredicate<A>> => {
  // NOTE: if arguments are supplied explicitly, the caller must have the right types for the arguments.
  switch (decl.tag) {
    case "PredicateDecl":
      return generatePredicate(decl, ctx, args);
    case "FunctionDecl":
      return generateFunction(decl, ctx, args as SubExpr<A>[]);
    case "ConstructorDecl":
      return generateConstructor(decl, ctx, args as SubExpr<A>[]);
  }
};

const generateDecl = (
  type: TypeDecl<A>,
  ctx: SynthesisContext
): WithContext<Decl<A>> => {
  const typeCons = applyTypeDecl(type);
  const { res: name, ctx: newCtx } = generateID(ctx, typeCons.name);
  const stmt: Decl<A> = {
    tag: "Decl",
    nodeType: "SyntheticSubstance",
    type: typeCons,
    name,
  };
  return {
    res: stmt,
    ctx: newCtx,
  };
};

const generateDeclFromType = (
  typeCons: TypeConsApp<A>,
  ctx: SynthesisContext
): WithContext<Decl<A>> => {
  const { res: name, ctx: newCtx } = generateID(ctx, typeCons.name);
  const stmt: Decl<A> = {
    tag: "Decl",
    nodeType: "SyntheticSubstance",
    type: typeCons,
    name,
  };
  return {
    res: stmt,
    ctx: newCtx,
  };
};

const generatePredicate = (
  pred: PredicateDecl<A>,
  ctx: SynthesisContext,
  args?: SubPredArg<A>[]
): WithStmts<ApplyPredicate<A>> => {
  log.debug(`Generating predicate of ${pred.name.value} type`);
  if (!args) {
    const {
      res,
      stmts,
      ctx: newCtx,
    }: WithStmts<SubPredArg<A>[]> = generatePredArgs(pred.args, ctx);
    const p: ApplyPredicate<A> = applyPredicate(pred, res);
    return { res: p, stmts, ctx: newCtx };
  } else {
    return {
      res: applyPredicate(pred, args),
      stmts: [],
      ctx,
    };
  }
};

const generateFunction = (
  func: FunctionDecl<A>,
  ctx: SynthesisContext,
  predefinedArgs?: SubExpr<A>[]
): WithStmts<Bind<A>> => {
  let args: SubExpr<A>[];
  let decls: SubStmt<A>[];
  if (!predefinedArgs) {
    const res: WithStmts<SubExpr<A>[]> = generateArgs(func.args, ctx);
    args = res.res;
    decls = res.stmts;
  } else {
    args = predefinedArgs;
    decls = [];
  }
  const rhs: ApplyFunction<A> = applyFunction(func, args);
  // find the `TypeDecl` for the output type
  const outputType = func.output.type as TypeConstructor<A>;
  // NOTE: the below will bypass the config and generate a new decl using the output type, search first in `ctx` to follow the config more strictly.
  // TODO: choose between generating vs. reusing
  const { res: lhsDecl, ctx: newCtx } = generateDeclFromType(
    nullaryTypeCons(outputType.name),
    ctx
  );
  const lhs: Identifier<A> = lhsDecl.name;
  const stmt: Bind<A> = applyBind(lhs, rhs);
  return { res: stmt, stmts: [...decls, lhsDecl], ctx: newCtx };
};

const generateConstructor = (
  cons: ConstructorDecl<A>,
  ctx: SynthesisContext,
  predefinedArgs?: SubExpr<A>[]
): WithStmts<Bind<A>> => {
  let args: SubExpr<A>[];
  let decls: SubStmt<A>[];
  if (!predefinedArgs) {
    const res: WithStmts<SubExpr<A>[]> = generateArgs(cons.args, ctx);
    args = res.res;
    decls = res.stmts;
  } else {
    args = predefinedArgs;
    decls = [];
  }
  const rhs: ApplyConstructor<A> = applyConstructor(cons, args);
  const outputType = cons.output.type as TypeConstructor<A>;
  // NOTE: the below will bypass the config and generate a new decl using the output type, search first in `ctx` to follow the config more strictly.
  const { res: lhsDecl, ctx: newCtx } = generateDeclFromType(
    nullaryTypeCons(outputType.name),
    ctx
  );
  const lhs: Identifier<A> = lhsDecl.name;
  const stmt: Bind<A> = applyBind(lhs, rhs);
  return { res: stmt, stmts: [...decls, lhsDecl], ctx: newCtx };
};

const generateArgs = (
  args: Arg<A>[],
  ctx: SynthesisContext
): WithStmts<SubExpr<A>[]> => {
  const resWithCtx: WithStmts<SubExpr<A>[]> & IDList = args.reduce(
    ({ res, stmts, ids }: WithStmts<SubExpr<A>[]> & IDList, arg) => {
      const {
        res: newArg,
        stmts: newStmts,
        ids: usedIDs,
        ctx: newCtx,
      } = generateArg(arg, ctx, ctx.argOption, ctx.argReuse, ids);
      return {
        res: [...res, newArg],
        stmts: [...stmts, ...newStmts],
        ids: [...ids, ...usedIDs],
        ctx: newCtx,
      };
    },
    { res: [], stmts: [], ids: [], ctx }
  );
  return {
    res: resWithCtx.res,
    stmts: resWithCtx.stmts,
    ctx: resWithCtx.ctx,
  };
};

const generateArg = (
  arg: Arg<A>,
  ctx: SynthesisContext,
  option: ArgOption,
  reuseOption: ArgReuse,
  usedIDs: Identifier<A>[]
): WithStmts<SubExpr<A>> & IDList => {
  const argType: Type<A> = arg.type;
  if (argType.tag === "TypeConstructor") {
    switch (option) {
      case "existing": {
        // TODO: clean up the logic
        const argTypeName = argType.name.value;
        const possibleIDs =
          reuseOption === "distinct"
            ? findIDs(
                ctx,
                [argTypeName, ...subTypesOf(argType, ctx.env).map((t) => t)],
                usedIDs
              )
            : findIDs(ctx, [
                argTypeName,
                ...subTypesOf(argType, ctx.env).map((t) => t),
              ]);
        const existingID = ctx.choice(possibleIDs);
        log.debug(
          `generating an argument with possbilities ${possibleIDs.map(
            (i) => i.value
          )}`
        );
        if (!existingID) {
          log.debug(
            `no existing ID found for ${argType.name.value}, generating a new value instead`
          );
          return generateArg(arg, ctx, "generated", reuseOption, usedIDs);
        } else {
          return {
            res: existingID,
            ctx,
            stmts: [],
            ids: [...usedIDs, existingID],
          };
        }
      }
      case "generated": {
        const argTypeDecl = ctx.env.types.get(argType.name.value);
        if (argTypeDecl) {
          const { res: decl, ctx: newCtx } = generateDecl(argTypeDecl, ctx);
          return {
            res: decl.name,
            stmts: [decl],
            ids: [...usedIDs, decl.name],
            ctx: newCtx,
          };
        } else {
          throw new Error(
            `${argType.name.value} not found in the candidate list`
          );
        }
      }
      case "mixed":
        return generateArg(
          arg,
          ctx,
          ctx.choice(["existing", "generated"]),
          reuseOption,
          usedIDs
        );
    }
  } else {
    throw new Error(`${argType.tag} not supported for argument generation`);
  }
};

const generatePredArgs = (
  args: Arg<A>[],
  ctx: SynthesisContext
): WithStmts<SubPredArg<A>[]> => {
  const resWithCtx = args.reduce(
    ({ res, stmts, ids, ctx }: WithStmts<SubPredArg<A>[]> & IDList, arg) => {
      const {
        res: newArg,
        stmts: newStmts,
        ids: usedIDs,
        ctx: newCtx,
      } = generatePredArg(arg, ctx, ctx.argOption, ctx.argReuse, ids);
      return {
        res: [...res, newArg],
        stmts: [...stmts, ...newStmts],
        ids: [...ids, ...usedIDs],
        ctx: newCtx,
      };
    },
    {
      res: [],
      stmts: [],
      ids: [],
      ctx,
    }
  );
  return {
    res: resWithCtx.res,
    stmts: resWithCtx.stmts,
    ctx: resWithCtx.ctx,
  };
};

const generatePredArg = (
  arg: Arg<A>,
  ctx: SynthesisContext,
  option: ArgOption,
  reuseOption: ArgReuse,
  usedIDs: Identifier<A>[]
): WithStmts<SubPredArg<A>> & IDList => {
  const argType: Type<A> = arg.type;
  if (argType.tag === "Prop") {
    const pred = ctx.choice(ctx.env.predicates.toArray().map(([, b]) => b));
    const { res, stmts, ctx: newCtx } = generatePredicate(pred, ctx);
    return { res, stmts, ids: usedIDs, ctx: newCtx };
  } else {
    return generateArg(arg, ctx, option, reuseOption, usedIDs);
  }
};

//#endregion

//#region Helpers

const declTypes: DomainStmt<A>["tag"][] = [
  "ConstructorDecl",
  "FunctionDecl",
  "TypeDecl",
  "PredicateDecl",
];

//#endregion
