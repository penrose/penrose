import "global-jsdom/register"; // must be first
import _ from "lodash";
import {
  PenroseState,
  compile,
  compileDomain,
  compileSubstance,
  optimize,
  showError,
  sub,
  toSVG,
} from "@penrose/core";
import { compileStyle } from "@penrose/core/dist/compiler/Style";
import prettier from "prettier";
import im from "immutable";
import * as fs from "fs";
import { CompiledSubStmt } from "@penrose/core/dist/types/substance";
import {
  prettyPredicate,
  prettyStmt,
} from "@penrose/core/dist/compiler/Substance";
const rawDsl = fs.readFileSync("dom.domain", "utf8");
const rawSty = fs.readFileSync("sty.style", "utf8");
const rawSub1 = fs.readFileSync("sub1.substance", "utf8");
const rawSub2 = fs.readFileSync("sub2.substance", "utf8");

const variation = "helloworld";

const dsl = compileDomain(rawDsl);

if (dsl.isErr()) {
  throw new Error(`dsl compilation error: ${dsl.error}`);
}

const sub1 = compileSubstance(rawSub1, dsl.value);

if (sub1.isErr()) {
  throw new Error(`sub1 compilation error: ${sub1.error}`);
}

const sub2 = compileSubstance(rawSub2, dsl.value);

if (sub2.isErr()) {
  throw new Error(`sub2 compilation error: ${sub2.error}`);
}

// render sub1

const state1 = await compileStyle(
  variation,
  rawSty,
  [],
  sub1.value[0],
  dsl.value,
);

if (state1.isErr()) {
  throw new Error(`sub1 style compilation error: ${state1.error}`);
}
const optimized1 = optimize(state1.value);

if (optimized1.isErr()) {
  throw new Error(`sub1 opt error: ${optimized1.error}`);
}

const canvas1 = (
  await toSVG(optimized1.value, async (path: string) => "", "animation", true)
).outerHTML;

const diagram1 = await prettier.format(canvas1, { parser: "html" });

fs.writeFileSync("diag1.svg", diagram1);

const sub1Stmts = sub1.value[0].ast.statements.map(prettyStmt);
const sub2Stmts = sub2.value[0].ast.statements.map(prettyStmt);

const intersect = sub1Stmts.filter((s) =>
  sub2Stmts.some((s2) => _.isEqual(s, s2)),
);

console.log(intersect);
