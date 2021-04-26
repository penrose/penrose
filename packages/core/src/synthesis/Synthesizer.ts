import { dummyIdentifier } from "engine/EngineUtils";
import { Map } from "immutable";
import { sample, times } from "lodash";
import { type } from "os";
import { Identifier } from "types/ast";
import { Env, TypeDecl } from "types/domain";
import { Decl, SubProg, SubStmt, TypeConsApp } from "types/substance";

class SynthesisContext {
  names: Map<string, number>;
  declaredIDs: Map<string, Identifier[]>;

  constructor() {
    this.names = Map();
    this.declaredIDs = Map();
  }

  reset = () => {
    this.names = Map();
  };

  addID = (typeStr: string, id: Identifier) => {
    const ids = this.declaredIDs.get(typeStr);
    if (ids) {
      this.declaredIDs = this.declaredIDs.set(typeStr, [...ids, id]);
    } else {
      this.declaredIDs = this.declaredIDs.set(typeStr, [id]);
    }
  };

  generateID = (type: TypeConsApp): Identifier => {
    const typeStr = type.name.value;
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

  constructor(env: Env) {
    this.env = env;
    this.cxt = new SynthesisContext();
  }

  generateSubstances = (numProgs: number): SubProg[] =>
    times(numProgs, () => {
      const sub = this.generateSubstance();
      this.cxt.reset();
      return sub;
    });

  generateSubstance = (): SubProg => {
    const numStmts = 10; // COMBAK: parametrize or randomize
    const stmts: SubStmt[] = times(numStmts, () => this.generateType());
    return {
      tag: "SubProg",
      statements: stmts,
    };
  };

  generateType = (): Decl => {
    // pick a type
    // TODO: handle null case
    const type: TypeDecl = sample(this.env.types.toArray().map(([, b]) => b))!;
    const typeCons: TypeConsApp = applyType(type);
    return {
      tag: "Decl",
      nodeType: "SyntheticSubstance",
      children: [],
      type: typeCons,
      name: this.cxt.generateID(typeCons),
    };
  };
}

const applyType = (decl: TypeDecl): TypeConsApp => {
  const { name } = decl;
  return {
    tag: "TypeConstructor",
    name,
    args: [],
  };
};

// { names         :: Names
// , declaredTypes :: M.Map String [Name] -- | Map from type name to a list of names with the type
// , prog          :: SubProg -- | AST of the generated program
// , initProg      :: SubProg -- | AST of an input Substance program
// , gen           :: StdGen -- | A random generator
// , setting       :: Setting -- | Synthesizer settings
// , argContext    :: ArgContext -- | Context for generating arguments. Needs to be reinitialized for generating each set of arguments
// } deriving (Show)
