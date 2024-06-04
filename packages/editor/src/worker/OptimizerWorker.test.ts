// @vitest-environment jsdom

import { defineWebWorkers } from "@vitest/web-worker/pure";
import { v4 as uuid } from "uuid";
import { describe, it } from "vitest";
import OptimizerWorker, { UpdateInfo } from "./OptimizerWorker";
import { LayoutStats } from "./common";

// copied from `packages/examples/src/set-theory-domain/setTheory.domain`
const domain = `type Set

predicate Disjoint(Set s1, Set s2)
predicate Intersecting(Set s1, Set s2)
predicate Subset(Set s1, Set s2)
`;

// copied from `packages/examples/src/set-theory-domain/venn.style`
const style = `canvas {
  width = 800
  height = 700
}

forall Set x {
  x.icon = Circle {
    strokeWidth : 0
  }

  x.text = Equation {
    string : x.label
    fontSize : "25px"
  }

  ensure contains(x.icon, x.text)
  encourage sameCenter(x.text, x.icon)
  x.textLayering = x.text above x.icon
}

forall Set x; Set y
where Subset(x, y) {
  ensure disjoint(y.text, x.icon, 10)
  ensure contains(y.icon, x.icon, 5)
  x.icon above y.icon
}

forall Set x; Set y
where Disjoint(x, y) {
  ensure disjoint(x.icon, y.icon)
}

forall Set x; Set y
where Intersecting(x, y) {
  ensure overlapping(x.icon, y.icon)
  ensure disjoint(y.text, x.icon)
  ensure disjoint(x.text, y.icon)
}
`;

const substance = `Set A, B, C, D, E, F, G

Subset(B, A)
Subset(C, A)
Subset(D, B)
Subset(E, B)
Subset(F, C)
Subset(G, C)

Disjoint(E, D)
Disjoint(F, G)
Disjoint(B, C)

AutoLabel All
`;

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
    const { id: id_, warnings } = await optimizer.compile(domain, style, substance, variation);
    id = id_;
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
