/**
 * @vitest-environment jsdom
 */

import exampleSubstance from "@penrose/examples/dist/word-cloud/example.substance";
import wordCloudDomain from "@penrose/examples/dist/word-cloud/word-cloud.domain";
import wordCloudStyle from "@penrose/examples/dist/word-cloud/word-cloud.style";
import { defineWebWorkers } from "@vitest/web-worker/pure";
import { v4 as uuid } from "uuid";
import { describe, it } from "vitest";
import OptimizerWorker, { UpdateInfo } from "./OptimizerWorker";
import { LayoutStats } from "./common";

const domain = wordCloudDomain;
const style = wordCloudStyle;
const substance = exampleSubstance;

const fuzz = async (ops: number, expect: any) => {
  defineWebWorkers({ clone: "none" });

  let state: string | undefined;
  let id: string | undefined;
  let receivedFinish = false;
  let variation = uuid();
  let layoutStats: LayoutStats = [];

  const totalSteps = () => {
    return layoutStats.reduce((acc, { steps }) => acc + steps, 0);
  };

  const optimizer = new OptimizerWorker();

  const onOptimizerFinish = (info: UpdateInfo) => {
    layoutStats = info.stats;
    state = optimizer.getState();
    receivedFinish = true;
    expect(state === "Compiled");
  };

  const compile = async () => {
    id = await optimizer.compile(domain, style, substance, variation);
    expect(state === "Compiled");
  };

  const startOptimizing = async () => {
    const { onStart, onFinish } = await optimizer.startOptimizing();
    onFinish.then(onOptimizerFinish);
    await onStart;

    state = optimizer.getState();
    if (!receivedFinish) {
      expect(state === "Optimizing");
    } else {
      expect(state === "Compiled");
    }
  };

  const interruptOptimizing = async () => {
    await optimizer.interruptOptimizing();

    state = optimizer.getState();
    expect(state === "Compiled");
  };

  // broken by jsdom, but should be called on compiled and optimizing
  const resample = async () => {
    variation = uuid();
    if (id === undefined) {
      expect(false && "id was undefined");
      return;
    }

    const { onStart, onFinish } = await optimizer.resample(id, variation);
    onFinish.then(onOptimizerFinish);
    await onStart;

    state = optimizer.getState();
    if (!receivedFinish) {
      expect(state === "Optimizing");
    } else {
      expect(state === "Compiled");
    }
  };

  const pollForUpdate = async () => {
    const info = await optimizer.pollForUpdate();
    if (info !== null) {
      layoutStats = info.stats;
    }

    const origState = state;
    state = optimizer.getState();
    expect(state === "Compiled" || state === "Optimizing");
    expect(!(origState === "Compiled" && state === "Optimizing"));
  };

  const computeShapes = async () => {
    const max = totalSteps();
    if (max === 0) return;

    const step = Math.floor(Math.random() * max);
    await optimizer.computeShapesAtIndex(step);

    const origState = state;
    state = optimizer.getState();
    expect(state === "Compiled" || state === "Optimizing");
    expect(!(origState === "Compiled" && state === "Optimizing"));
  };

  const legalMethods = new Map<string, (() => Promise<void>)[]>([
    ["Init", [compile]],
    ["Compiled", [startOptimizing, pollForUpdate, computeShapes]],
    ["Optimizing", [interruptOptimizing, pollForUpdate, computeShapes]],
  ]);

  await optimizer.waitForInit();

  for (let i = 0; i < ops; i++) {
    state = optimizer.getState();
    console.log(`${state} ${i}`);
    const choices = legalMethods.get(state);
    expect(state !== "undefined");
    if (choices === undefined) return;
    const index = Math.floor(Math.random() * choices.length);
    await choices[index]();
  }

  optimizer.terminate();
};

describe.concurrent("OptimizerWorker", () => {
  it("fuzz 100", async ({ expect }) => {
    await fuzz(100, expect);
  });
});
