import "global-jsdom/register"; // must be first
import _ from "lodash";
import {
  Num,
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
import { Shape } from "@penrose/core/dist/shapes/Shapes";
import * as ad from "@penrose/core/dist/types/ad";
import { ArgVal, Value } from "@penrose/core/dist/types/value";
import { val } from "@penrose/core/dist/utils/Util";
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

const sub1Stmts = sub1.value[0].ast.statements.map(prettyStmt);
const sub2Stmts = sub2.value[0].ast.statements.map(prettyStmt);

const intersect = sub1Stmts.filter((s) =>
  sub2Stmts.some((s2) => _.isEqual(s, s2)),
);

const buildOverrideMap = (
  shape: Shape<number>,
): Map<string, ArgVal<number>> => {
  const m = new Map<string, ArgVal<number>>();

  const name = shape.name.contents;
  // handle passthrough
  for (const [propKey, value] of shape.passthrough) {
    const key = `${name}.${propKey}`;
    m.set(key, { tag: "Val", contents: value });
  }

  // handle other properties
  for (const [propKey, value] of Object.entries(shape)) {
    if (
      propKey === "shapeType" ||
      propKey === "meta" ||
      propKey === "passthrough"
    ) {
      continue;
    }

    const k = `${name}.${propKey}`;
    const v = value as Value<number>;
    m.set(k, { tag: "Val", contents: v });
  }

  return m;
};

let override = new Map<string, ArgVal<number>>();
for (const shape of optimized1.value.computeShapes(
  optimized1.value.varyingValues,
)) {
  if (
    shape.meta.causedBy.some((stmt) => intersect.includes(prettyStmt(stmt)))
  ) {
    const overrideForShape = buildOverrideMap(shape);
    override = new Map([...override, ...overrideForShape]);
  }
}

// render sub2

const state2 = await compileStyle(
  variation,
  rawSty,
  [],
  sub2.value[0],
  dsl.value,
  override,
);

if (state2.isErr()) {
  throw new Error(`sub1 style compilation error: ${state2.error}`);
}
const optimized2 = optimize(state2.value);

if (optimized2.isErr()) {
  throw new Error(`sub1 opt error: ${optimized2.error}`);
}

const canvas2 = (
  await toSVG(optimized2.value, async (path: string) => "", "animation")
).outerHTML;

const diagram2 = await prettier.format(canvas2, { parser: "html" });

fs.writeFileSync("diag2.svg", diagram2);
