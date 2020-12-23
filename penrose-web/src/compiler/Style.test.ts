import * as _ from "lodash";
import * as stateJSON from "__tests__/orthogonalVectors.json";
import * as styJSON from "compiler/asts/linear-algebra-paper-simple.ast.json";
import { selEnvs, possibleSubsts, correctSubsts } from "compiler/StyleTestData"; // TODO: Check correctness
import { compileStyle, fullSubst, uniqueKeysAndVals } from "compiler/Style"; // COMBAK: Use this import properly

const clone = require("rfdc")({ proto: false, circles: false });

// TODO: Reorganize and name tests by compiler stage

describe("Compiler", () => {

  // Each possible substitution should be full WRT its selector
  test("substitution: fullSubst true", () => {
    for (let i = 0; i < selEnvs.length; i++) {
      for (let j = 0; j < possibleSubsts[i].length; j++) {
        expect(fullSubst(selEnvs[i], possibleSubsts[i][j] as Subst)).toEqual(true);
      }
    }
  });

  test("substitution: fullSubst false", () => {
    // Namespace shouldn't have matches
    const ps0: Subst = { "test": "A" };
    expect(fullSubst(selEnvs[0], ps0)).toEqual(false);

    // Selector should have real substitution
    const ps1 = { "v": "x1", "U": "X" }; // missing "w" match
    expect(fullSubst(selEnvs[6], ps1)).toEqual(false);
  });

  test("substitution: uniqueKeysAndVals true", () => {
    // This subst has unique keys and vals
    expect(uniqueKeysAndVals({ "a": "V", "c": "z" })).toEqual(true);
  });

  test("substitution: uniqueKeysAndVals false", () => {
    // This subst doesn't have unique keys and vals
    expect(uniqueKeysAndVals({ "a": "V", "c": "V" })).toEqual(false);
  });

});
