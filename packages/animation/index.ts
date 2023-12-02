import "global-jsdom/register"; // must be first
import _ from "lodash";
import {
  compileDomain,
  compileSubstance,
  optimize,
  showError,
  toSVG,
} from "@penrose/core";
import prettier from "prettier";
import * as fs from "fs";
import { prettyStmt } from "@penrose/core/dist/compiler/Substance";
import { makeFreezer } from "./freezing/freezer.js";
import { freezeShapes } from "./freezing/freezing.js";
import { compileStyle } from "./utils/labeling.js";
const rawDsl = fs.readFileSync("dom.domain", "utf8");
const rawSty = fs.readFileSync("sty.style", "utf8");
const rawSub1 = fs.readFileSync("sub1.substance", "utf8");
const rawSub2 = fs.readFileSync("sub2.substance", "utf8");

const variation = "helloworld12345";

const dsl = compileDomain(rawDsl);

if (dsl.isErr()) {
  throw new Error(`dsl compilation error: ${showError(dsl.error)}`);
}

const sub1 = compileSubstance(rawSub1, dsl.value);

if (sub1.isErr()) {
  throw new Error(`sub1 compilation error: ${showError(sub1.error)}`);
}

const sub2 = compileSubstance(rawSub2, dsl.value);

if (sub2.isErr()) {
  throw new Error(`sub2 compilation error: ${showError(sub2.error)}`);
}

// render sub1

const state1 = await compileStyle(
  variation,
  rawSty,
  [],
  sub1.value[0],
  dsl.value,
);

const optimized1 = optimize(state1);

if (optimized1.isErr()) {
  throw new Error(`sub1 opt error: ${showError(optimized1.error)}`);
}

const canvas1 = (
  await toSVG(optimized1.value, async (path: string) => "", "animation")
).outerHTML;

const diagram1 = await prettier.format(canvas1, { parser: "html" });

fs.writeFileSync("diag1.svg", diagram1);

// compile sub2
const state2 = await compileStyle(
  variation,
  rawSty,
  [],
  sub2.value[0],
  dsl.value,
);

const locker = makeFreezer(optimized1.value, state2);

const sub1Stmts = sub1.value[0].ast.statements.map(prettyStmt);
const sub2Stmts = sub2.value[0].ast.statements.map(prettyStmt);

const intersect = sub1Stmts.filter((s) =>
  sub2Stmts.some((s2) => _.isEqual(s, s2)),
);

freezeShapes(optimized1.value.shapes, state2.shapes, intersect, locker);

const optimized2 = optimize(state2);
if (optimized2.isErr()) {
  throw new Error(`sub2 opt error: ${showError(optimized2.error)}`);
}

//const optimized2 = state2;
const canvas2 = (
  await toSVG(optimized2.value, async (path: string) => "", "animation")
).outerHTML;

const diagram2 = await prettier.format(canvas2, { parser: "html" });

fs.writeFileSync("diag2.svg", diagram2);
