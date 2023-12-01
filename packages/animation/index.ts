import "global-jsdom/register"; // must be first
import _ from "lodash";
import {
  compileDomain,
  compileSubstance,
  optimize,
  toSVG,
} from "@penrose/core";
import { compileStyle } from "@penrose/core/dist/compiler/Style";
import prettier from "prettier";
import * as fs from "fs";
import { prettyStmt } from "@penrose/core/dist/compiler/Substance";
import { makeLockerInState } from "./locker.js";
import { lockShapes } from "./utils.js";
const rawDsl = fs.readFileSync("dom.domain", "utf8");
const rawSty = fs.readFileSync("sty.style", "utf8");
const rawSub1 = fs.readFileSync("sub1.substance", "utf8");
const rawSub2 = fs.readFileSync("sub2.substance", "utf8");

const variation = "helloworld12345";

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

if (state2.isErr()) {
  throw new Error(`sub2 style compilation error: ${state2.error}`);
}

const locker = makeLockerInState(optimized1.value, state2.value);

const sub1Stmts = sub1.value[0].ast.statements.map(prettyStmt);
const sub2Stmts = sub2.value[0].ast.statements.map(prettyStmt);

const intersect = sub1Stmts.filter((s) =>
  sub2Stmts.some((s2) => _.isEqual(s, s2)),
);

lockShapes(optimized1.value.shapes, state2.value.shapes, intersect, locker);
function printArr(names: any[]) {
  names.forEach((x) => console.log(x));
}
printArr(state2.value.inputs);
printArr(state2.value.varyingValues);

const optimized2 = optimize(state2.value);
if (optimized2.isErr()) {
  throw new Error(`sub2 opt error: ${optimized2.error}`);
}

//const optimized2 = state2;
const canvas2 = (
  await toSVG(optimized2.value, async (path: string) => "", "animation")
).outerHTML;

const diagram2 = await prettier.format(canvas2, { parser: "html" });

fs.writeFileSync("diag2.svg", diagram2);
console.log("=====");
printArr(optimized2.value.varyingValues);
