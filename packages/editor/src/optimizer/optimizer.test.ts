// @vitest-environment jsdom

import { defineWebWorkers } from "@vitest/web-worker/pure";
import { describe, expect, it, test } from "vitest";
import Optimizer from "./optimizer";

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

describe("compiling", () => {
  defineWebWorkers({ clone: "none" });

  it("can compile", async () => {
    const optimizer = await Optimizer.create();
    const result = await optimizer.compile(domain, style, substance, "");
    console.log(result);
  });
});