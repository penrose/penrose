import { prettyStmt, prettySubstance } from "compiler/Substance";
import { max } from "engine/Autodiff";
import { dummyIdentifier } from "engine/EngineUtils";
import { Map } from "immutable";
import { times } from "lodash";
import { random, choice } from "pandemonium";
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
  SubstanceEnv,
  SubStmt,
  TypeConsApp,
} from "types/substance";

type All = "*";
type ArgOption = "existing" | "generated" | "mixed";

type MatchSetting = string[] | All;

interface Candidates {
  types: Map<string, TypeDecl>;
  functions: Map<string, FunctionDecl>;
  predicates: Map<string, PredicateDecl>;
  constructors: Map<string, ConstructorDecl>;
}
interface DeclTypes {
  type: MatchSetting;
  predicate: MatchSetting;
  constructor: MatchSetting;
  function: MatchSetting;
}
export interface SynthesizerSetting {
  mutationCount: [number, number];
  argOption: ArgOption;
  weights: {
    type: number;
    predicate: number;
    constructor: number;
  };
  add: DeclTypes;
  delete: DeclTypes;
}

class SynthesisContext {
  names: Map<string, number>;
  declaredIDs: Map<string, Identifier[]>;
  prog: SubProg;
  numStmts: number;
  maxStmts: number;
  candidates: Candidates;
  subRes: SubRes | undefined;

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
    };
  }

  loadTemplate = () => {
    // If there is a template substance program, load in the relevant info
    if (this.subRes) {
      const [subEnv, env] = this.subRes;
      this.prog = subEnv.ast;
      env.vars.forEach((type, id) => this.addID(type.name.value, id));
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

  printCandidates = () => {
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
    }
    throw new Error(`${type} is not supported by the synthesizer`);
  };

  candidateTypes = (): DomainStmt["tag"][] =>
    declTypes.filter((type) => !this.getCandidates(type).isEmpty());

  // append a statement to the generated program
  appendStmt = (stmt: SubStmt) => {
    this.prog = {
      ...this.prog,
      statements: [...this.prog.statements, stmt],
    };
    this.numStmts++;
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

  reset = (maxStmts: number) => {
    this.maxStmts = maxStmts;
    this.names = Map();
    this.declaredIDs = Map();
    this.prog = {
      tag: "SubProg",
      statements: [],
    };
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

  pickID = (typeStr: string): Identifier | undefined => {
    const possibleIDs = this.declaredIDs.get(typeStr);
    if (possibleIDs) return choice([...possibleIDs]);
    else return undefined;
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
}

export class Synthesizer {
  env: Env;
  cxt: SynthesisContext;
  setting: SynthesizerSetting;

  constructor(env: Env, setting: SynthesizerSetting, subRes?: SubRes) {
    this.env = env;
    this.cxt = new SynthesisContext(subRes);
    this.setting = setting;
  }

  generateSubstances = (numProgs: number): SubProg[] =>
    times(numProgs, () => {
      const sub = this.generateSubstance();
      return sub;
    });

  generateSubstance = (): SubProg => {
    const numStmts = random(...this.setting.mutationCount);
    this.cxt.reset(numStmts);
    times(numStmts, () => this.mutateProgram());
    return this.cxt.prog;
  };

  mutateProgram = (): void => {
    const ops = ["add", "delete"];
    const op = choice(ops);
    if (op === "add") this.addStmt();
    else if (op === "delete") this.deleteStmt();
  };

  // NOTE: every synthesizer that 'generateStatement' calls is expected to append its result to the AST, instead of just returning it. This is because certain lower-level functions are allowed to append new statements (e.g. 'generateArg'). Otherwise, we could write this module as a combinator.
  addStmt = (): void => {
    this.cxt.findCandidates(this.env, this.setting.add);
    const chosenType = choice(this.cxt.candidateTypes());
    switch (chosenType) {
      case "TypeDecl":
        this.generateType();
        return;
      case "PredicateDecl":
        this.generatePredicate();
        return;
      case "FunctionDecl":
        this.generateFunction();
        return;
      case "ConstructorDecl":
        this.generateConstructor();
        return;
    }
  };

  deleteStmt = (): void => {
    this.cxt.findCandidates(this.env, this.setting.delete);
    const chosenType = choice(this.cxt.candidateTypes());
    const candidates = [...this.cxt.getCandidates(chosenType).keys()];
    const chosenName = choice(candidates);
    switch (chosenType) {
      case "TypeDecl": {
        const stmt = this.findStmt("Decl", chosenName);
        if (stmt) this.cxt.removeStmt(stmt);
      }
      case "PredicateDecl": {
        const stmt = this.findStmt("ApplyPredicate", chosenName);
        if (stmt) this.cxt.removeStmt(stmt);
      }
      case "FunctionDecl": {
        const stmt = this.findStmt("ApplyFunction", chosenName);
        if (stmt) this.cxt.removeStmt(stmt);
      }
      case "ConstructorDecl": {
        const stmt = this.findStmt("ApplyConstructor", chosenName);
        if (stmt) this.cxt.removeStmt(stmt);
      }
    }
  };

  findStmt = (
    stmtType: "Decl" | "ApplyPredicate" | "ApplyFunction" | "ApplyConstructor",
    name: string
  ): SubStmt | undefined => {
    // console.log("finding", stmtType, name);
    const stmts = this.cxt.prog.statements.filter((s) => {
      return s.tag === stmtType && s.name.value === name;
    });
    if (stmts.length > 0) {
      const stmt = choice(stmts);
      return stmt;
    } else {
      // console.log(
      //   `Warning: cannot find a ${stmtType} statement with name ${name}. Current program:\n${prettySubstance(
      //     this.cxt.prog
      //   )}`
      // );
    }
  };

  generateType = (typeName?: Identifier): Decl => {
    // pick a type
    let typeCons: TypeConsApp;
    if (typeName) {
      typeCons = nullaryTypeCons(typeName);
    } else {
      const type: TypeDecl = choice(
        this.cxt.candidates.types.toArray().map(([, b]) => b)
      );

      typeCons = applyTypeDecl(type);
    }
    const stmt: Decl = {
      tag: "Decl",
      nodeType: "SyntheticSubstance",
      children: [],
      type: typeCons,
      name: this.cxt.generateID(typeCons.name),
    };
    this.cxt.appendStmt(stmt);
    return stmt;
  };

  generatePredicate = (): ApplyPredicate => {
    const pred: PredicateDecl = choice(
      this.cxt.candidates.predicates.toArray().map(([, b]) => b)
    );
    const args: SubPredArg[] = this.generatePredArgs(pred.args);
    const stmt: ApplyPredicate = applyPredicate(pred, args);
    this.cxt.appendStmt(stmt);
    return stmt;
  };

  generateFunction = (): Bind => {
    const func: FunctionDecl = choice(
      this.cxt.candidates.functions.toArray().map(([, b]) => b)
    );
    const args: SubExpr[] = this.generateArgs(func.args);
    const rhs: ApplyFunction = applyFunction(func, args);
    const outputType = func.output.type as TypeConstructor;
    // TODO: choose between generating vs. reusing
    const lhs: Identifier = this.generateType(outputType.name).name;
    const stmt: Bind = applyBind(lhs, rhs);
    this.cxt.appendStmt(stmt);
    return stmt;
  };

  generateConstructor = (): Bind => {
    const cons: ConstructorDecl = choice(
      this.cxt.candidates.constructors.toArray().map(([, b]) => b)
    );
    const args: SubExpr[] = this.generateArgs(cons.args);
    const rhs: ApplyConstructor = applyConstructor(cons, args);
    const outputType = cons.output.type as TypeConstructor;
    // TODO: choose between generating vs. reusing
    const lhs: Identifier = this.generateType(outputType.name).name;
    const stmt: Bind = applyBind(lhs, rhs);
    this.cxt.appendStmt(stmt);
    return stmt;
  };

  generateArgs = (args: Arg[]): SubExpr[] =>
    args.map((arg) => this.generateArg(arg, this.setting.argOption));

  generateArg = (arg: Arg, option: ArgOption): SubExpr => {
    const argType: Type = arg.type;
    if (argType.tag === "TypeConstructor") {
      switch (option) {
        case "existing": {
          const existingID = this.cxt.pickID(argType.name.value);
          if (!existingID) {
            return this.generateArg(arg, "generated");
          } else {
            return existingID;
          }
        }
        case "generated":
          this.generateType(argType.name);
          return this.generateArg(arg, "existing");
        case "mixed":
          return this.generateArg(arg, choice(["existing", "generated"]));
      }
    } else {
      throw new Error(`${argType.tag} not supported for argument generation`);
    }
  };

  generatePredArgs = (args: Arg[]): SubPredArg[] =>
    args.map((arg) => this.generatePredArg(arg, this.setting.argOption));

  generatePredArg = (arg: Arg, option: ArgOption): SubPredArg => {
    const argType: Type = arg.type;
    if (argType.tag === "Prop") {
      return this.generatePredicate();
    } else {
      return this.generateArg(arg, option);
    }
  };
}

const applyConstructor = (
  decl: ConstructorDecl,
  args: SubExpr[]
): ApplyConstructor => {
  const { name } = decl;
  return {
    tag: "ApplyConstructor",
    name,
    nodeType: "SyntheticSubstance",
    children: [],
    args,
  };
};

const applyFunction = (decl: FunctionDecl, args: SubExpr[]): ApplyFunction => {
  const { name } = decl;
  return {
    tag: "ApplyFunction",
    name,
    nodeType: "SyntheticSubstance",
    children: [],
    args,
  };
};

const applyPredicate = (
  decl: PredicateDecl,
  args: SubPredArg[]
): ApplyPredicate => {
  const { name } = decl;
  return {
    tag: "ApplyPredicate",
    name,
    nodeType: "SyntheticSubstance",
    children: [],
    args,
  };
};

// TODO: generate arguments as well
const applyTypeDecl = (decl: TypeDecl): TypeConsApp => {
  const { name } = decl;
  return nullaryTypeCons(name);
};

const applyBind = (variable: Identifier, expr: SubExpr): Bind => ({
  tag: "Bind",
  children: [],
  nodeType: "SyntheticSubstance",
  variable,
  expr,
});

const nullaryTypeCons = (name: Identifier): TypeConsApp => ({
  tag: "TypeConstructor",
  name,
  args: [],
});

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
