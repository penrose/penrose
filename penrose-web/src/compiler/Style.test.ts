// Must be run from penrose-web for loading files

import * as _ from "lodash";
import * as stateJSON from "__tests__/orthogonalVectors.json";
import { selEnvs, possibleSubsts, correctSubsts } from "compiler/StyleTestData";
import * as S from "compiler/Style";

import * as fs from "fs";
import * as path from "path";

import { SubstanceEnv, LabelMap, checkPredicate, checkVar, checkExpr, subtypeOf } from "compiler/Substance";
import { Env, isDeclaredSubtype, checkTypeConstructor } from "./Domain";
import { Result, ok, err, unsafelyUnwrap, isErr } from "utils/Error";

const clone = require("rfdc")({ proto: false, circles: false });

// TODO: Reorganize and name tests by compiler stage

// Load file in format "domain-dir/file.extension"
const loadFile = (examplePath: string): string => {
  const file = path.join("../examples/", examplePath);
  const prog = fs.readFileSync(file, "utf8");
  return prog;
};

const loadFiles = (paths: string[]): string[] => {
  return paths.map(loadFile);
};

// Load dsl, sub, sty (in that order)
const loadProgs = (triple: [string, string, string]): [Env, SubstanceEnv, SubProg, StyProg] => {
  return S.loadProgs(loadFiles(triple) as [string, string, string]);
};

describe("Compiler", () => {

  // Each possible substitution should be full WRT its selector
  test("substitution: S.fullSubst true", () => {
    for (let i = 0; i < selEnvs.length; i++) {
      for (let j = 0; j < possibleSubsts[i].length; j++) {
        expect(S.fullSubst(selEnvs[i], possibleSubsts[i][j] as Subst)).toEqual(true);
      }
    }
  });

  test("substitution: S.fullSubst false", () => {
    // Namespace shouldn't have matches
    const ps0: Subst = { "test": "A" };
    expect(S.fullSubst(selEnvs[0], ps0)).toEqual(false);

    // Selector should have real substitution
    const ps1 = { "v": "x1", "U": "X" }; // missing "w" match
    expect(S.fullSubst(selEnvs[6], ps1)).toEqual(false);
  });

  test("substitution: S.uniqueKeysAndVals true", () => {
    // This subst has unique keys and vals
    expect(S.uniqueKeysAndVals({ "a": "V", "c": "z" })).toEqual(true);
  });

  test("substitution: S.uniqueKeysAndVals false", () => {
    // This subst doesn't have unique keys and vals
    expect(S.uniqueKeysAndVals({ "a": "V", "c": "V" })).toEqual(false);
  });

  // For the 6th selector in the LA Style program, substituting in this substitution into the relational expressions yields the correct result (where all vars are unique)
  test("substitute unique vars in selector", () => {
    const subst: Subst = { v: "x1", U: "X", w: "x2" };
    const rels: RelationPattern[] = selEnvs[6].header.contents.where.contents; // This is selector #6 in the LA Style program
    // `rels` stringifies to this: `["In(v, U)", "Unit(v)", "Orthogonal(v, w)"]`
    const relsSubStr = rels.map(rel => S.substituteRel(subst, rel)).map(S.ppRel);
    const answers = ["In(x1, X)", "Unit(x1)", "Orthogonal(x1, x2)"];

    for (const [res, expected] of _.zip(relsSubStr, answers)) {
      expect(res).toEqual(expected);
    }
  });

  // For the 6th selector in the LA Style program, substituting in this substitution into the relational expressions yields the correct result (where two vars are non-unique, `x2`)
  test("substitute non-unique vars in selector", () => {
    const subst: Subst = { v: "x2", U: "X", w: "x2" };
    const rels: RelationPattern[] = selEnvs[6].header.contents.where.contents; // This is selector #6 in the LA Style program
    // `rels` stringifies to this: `["In(v, U)", "Unit(v)", "Orthogonal(v, w)"]`
    const relsSubStr = rels.map(rel => S.substituteRel(subst, rel)).map(S.ppRel);
    const answers = ["In(x2, X)", "Unit(x2)", "Orthogonal(x2, x2)"];

    for (const [res, expected] of _.zip(relsSubStr, answers)) {
      expect(res).toEqual(expected);
    }
  });

  // Compiler finds the right substitutions for LA Style program
  // Note that this doesn't test subtypes
  test("finds the right substitutions for LA Style program", () => {
    // This code is cleaned up from `S.compileStyle`; runs the beginning of compiler checking from scratch
    const triple: [string, string, string] = [
      "linear-algebra-domain/linear-algebra.dsl",
      "linear-algebra-domain/twoVectorsPerp-unsugared.sub",
      "linear-algebra-domain/linear-algebra-paper-simple.sty",
    ];

    const [varEnv, subEnv, subProg, styProgInit]: [Env, SubstanceEnv, SubProg, StyProg] = loadProgs(triple);

    S.disambiguateFunctions(varEnv, subProg);
    const selEnvs = S.checkSelsAndMakeEnv(varEnv, styProgInit.blocks);

    const selErrs: StyErrors = _.flatMap(selEnvs, e => e.warnings.concat(e.errors));

    if (selErrs.length > 0) {
      const err = `Could not compile. Error(s) in Style while checking selectors`;
      console.log([err].concat(selErrs));
      expect(false).toEqual(true);
    }

    const subss = S.findSubstsProg(varEnv, subEnv, subProg, styProgInit.blocks, selEnvs); // TODO: Use `eqEnv`

    if (subss.length !== correctSubsts.length) {
      expect(false).toEqual(true);
    }

    for (const [res, expected] of _.zip(subss, correctSubsts)) {
      expect(res).toEqual(expected);
    }
  });

  // There are no AnonAssign statements, i.e. they have all been substituted out (proxy test for `S.nameAnonStatements` working)
  test("There are no anonymous statements", () => {
    const triple: [string, string, string] = [
      "linear-algebra-domain/linear-algebra.dsl",
      "linear-algebra-domain/twoVectorsPerp-unsugared.sub",
      "linear-algebra-domain/linear-algebra-paper-simple.sty",
    ];

    const [varEnv, subEnv, subProg, styProgInit]: [Env, SubstanceEnv, SubProg, StyProg] = loadProgs(triple);
    S.disambiguateFunctions(varEnv, subProg);
    const selEnvs = S.checkSelsAndMakeEnv(varEnv, styProgInit.blocks);
    const selErrs: StyErrors = _.flatMap(selEnvs, e => e.warnings.concat(e.errors));

    if (selErrs.length > 0) {
      const err = `Could not compile. Error(s) in Style while checking selectors`;
      console.log([err].concat(selErrs));
      expect(false).toEqual(true);
    }

    const styProg: StyProg = S.nameAnonStatements(styProgInit);

    for (const hb of styProg.blocks) {
      for (const stmt of hb.block.statements) {
        expect(stmt.tag).not.toEqual("AnonAssign");
      }
    }
  });

  const sum = (acc: number, n: number, i: number): Either<String, number> => i > 2 ? S.Left("error") : S.Right(acc + n);

  test("S.foldM none", () => {
    expect(S.foldM([], sum, -1)).toEqual(S.Right(-1));
  });

  test("S.foldM right", () => {
    expect(S.foldM([1, 2, 3], sum, -1)).toEqual(S.Right(5));
  });

  test("S.foldM left", () => {
    expect(S.foldM([1, 2, 3, 4], sum, -1)).toEqual(S.Left("error"));
  });

  const xs = ['a', 'b', 'c'];
  test("numbered", () => {
    expect(S.numbered(xs)).toEqual([['a', 0], ['b', 1], ['c', 2]]);
  });

  // TODO: There are no tests directly for the substitution application part of the compiler, though I guess you could walk the AST (making the substitution-application code more generic to do so) and check that there are no Style variables anywhere? Except for, I guess, namespace names?

  // 

});
