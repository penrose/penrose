import { dummyIdentifier } from "engine/EngineUtils";
import { Map } from "immutable";
import { times } from "lodash";
import { type } from "os";
import { choice } from "pandemonium";
import { Identifier } from "types/ast";
import {
  Arg,
  Env,
  PredicateDecl,
  Type,
  TypeConstructor,
  TypeDecl,
} from "types/domain";
import {
  ApplyPredicate,
  Decl,
  SubPredArg,
  SubProg,
  SubStmt,
  TypeConsApp,
} from "types/substance";

type ArgOption = "existing" | "generated" | "mixed";
interface SynthesizerSetting {
  lengthRange: [number, number];
  argOption: ArgOption;
  weights: {
    type: number;
    predicate: number;
    constructor: number;
  };
}

class SynthesisContext {
  names: Map<string, number>;
  declaredIDs: Map<string, Identifier[]>;
  prog: SubProg;

  constructor() {
    this.names = Map();
    this.declaredIDs = Map();
    this.prog = {
      tag: "SubProg",
      statements: [],
    };
  }

  // append a statement to the generated program
  appendStmt = (stmt: SubStmt) => {
    this.prog = {
      ...this.prog,
      statements: [...this.prog.statements, stmt],
    };
  };

  reset = () => {
    this.names = Map();
    this.prog = {
      tag: "SubProg",
      statements: [],
    };
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

  constructor(env: Env, setting: SynthesizerSetting) {
    this.env = env;
    this.cxt = new SynthesisContext();
    this.setting = setting;
  }

  generateSubstances = (numProgs: number): SubProg[] =>
    times(numProgs, () => {
      const sub = this.generateSubstance();
      this.cxt.reset();
      return sub;
    });

  generateSubstance = (): SubProg => {
    const numStmts = 10; // COMBAK: parametrize or randomize
    times(numStmts, () => this.generateStmt());
    return this.cxt.prog;
  };

  generateStmt = (): void => {
    const stmtTypes = ["Decl", "Predicate"];
    const chosenType = choice(stmtTypes);
    switch (chosenType) {
      case "Decl":
        this.generateType();
      case "Predicate":
        this.generatePredicate();
    }
  };

  generateType = (typeName?: Identifier): Decl => {
    // pick a type
    let typeCons: TypeConsApp;
    if (typeName) {
      typeCons = nullaryTypeCons(typeName);
    } else {
      const type: TypeDecl = choice(this.env.types.toArray().map(([, b]) => b));
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
      this.env.predicates.toArray().map(([, b]) => b)
    );
    const args: SubPredArg[] = this.generateArgs(pred.args);
    const stmt: ApplyPredicate = applyPredicate(pred, args);
    this.cxt.appendStmt(stmt);
    return stmt;
  };

  generateArgs = (args: Arg[]): SubPredArg[] =>
    args.map((arg) => this.generateArg(arg, this.setting.argOption));

  generateArg = (arg: Arg, option: ArgOption): SubPredArg => {
    // TODO: handle other kinds of args
    const argType: Type = arg.type;
    if (argType.tag !== "TypeConstructor") {
      throw new Error(`${argType.tag} not supported for argument generation`);
    }
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
  };
}

// const argType = (arg: Arg) => {
//   const typeDecl: Type = arg.type;
// };

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

// const applyTypeCons = (typeCons: TypeConstructor): TypeConsApp => ({
//   ...typeCons,
//   args: typeCons.args.map(arg => )
// })

// TODO: generate arguments as well
const applyTypeDecl = (decl: TypeDecl): TypeConsApp => {
  const { name } = decl;
  return nullaryTypeCons(name);
};

const nullaryTypeCons = (name: Identifier): TypeConsApp => ({
  tag: "TypeConstructor",
  name,
  args: [],
});

// { names         :: Names
// , declaredTypes :: M.Map String [Name] -- | Map from type name to a list of names with the type
// , prog          :: SubProg -- | AST of the generated program
// , initProg      :: SubProg -- | AST of an input Substance program
// , gen           :: StdGen -- | A random generator
// , setting       :: Setting -- | Synthesizer settings
// , argContext    :: ArgContext -- | Context for generating arguments. Needs to be reinitialized for generating each set of arguments
// } deriving (Show)
