import { examples } from "@penrose/examples";

export const error = {
  domain: `typeppp Set`,
  substancce: `Set A + B`,
  style: `
  Set a {

  }
  `,
};
export const oneSet = {
  domain: `
type Set
`,
  substance: `
Set A
AutoLabel All
`,
  style: `
canvas {
  width = 500
  height = 500
}
forall Set X {
  X.shape = Circle { strokeWidth : 0 }
  X.text  = Equation { string: X.label }
  ensure contains(X.shape, X.text)
  ensure maxSize(X.shape, canvas.width / 2)
}
`,
  variation: "",
};

export const continuousMap = {
  substance: examples["set-theory-domain"]["continuousmap.sub"],
  style: examples["set-theory-domain"]["continuousmap.sty"],
  domain: examples["set-theory-domain"]["functions.dsl"],
  variation: "",
};

export const vectorWedge = {
  variation: "ArtemisCrane740",
  domain: examples["exterior-algebra"]["exterior-algebra.dsl"],
  substance: examples["exterior-algebra"]["vector-wedge.sub"],
  style: examples["exterior-algebra"]["exterior-algebra.sty"],
};

export const vectorsPerp = {
  variation: "MyrtleApe55311",
  domain: examples["linear-algebra-domain"]["linear-algebra.dsl"],
  substance: examples["linear-algebra-domain"]["twoVectorsPerp-unsugared.sub"],
  style: examples["linear-algebra-domain"]["linear-algebra-paper-simple.sty"],
};
