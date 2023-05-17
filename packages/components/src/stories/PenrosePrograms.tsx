import vectorWedge from "@penrose/examples/dist/exterior-algebra/vector-wedge-exterior-algebra";
import vectorsPerp from "@penrose/examples/dist/linear-algebra-domain/two-vectors-perp-vectors-dashed";
import continuousMap from "@penrose/examples/dist/set-theory-domain/continuousmap-continuousmap";

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

export { continuousMap, vectorWedge, vectorsPerp };
