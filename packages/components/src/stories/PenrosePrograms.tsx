import exteriorAlgebraDomain from "@penrose/examples/dist/exterior-algebra/exterior-algebra.domain";
import exteriorAlgebraStyle from "@penrose/examples/dist/exterior-algebra/exterior-algebra.style";
import vectorWedgeSubstance from "@penrose/examples/dist/exterior-algebra/vector-wedge.substance";
import linearAlgebraStyle from "@penrose/examples/dist/linear-algebra-domain/linear-algebra-paper-simple.style";
import linearAlgebraDomain from "@penrose/examples/dist/linear-algebra-domain/linear-algebra.domain";
import twoVectorsPerpSubstance from "@penrose/examples/dist/linear-algebra-domain/twoVectorsPerp-unsugared.substance";
import continuousMapStyle from "@penrose/examples/dist/set-theory-domain/continuousmap.style";
import continuousMapSubstance from "@penrose/examples/dist/set-theory-domain/continuousmap.substance";
import functionsDomain from "@penrose/examples/dist/set-theory-domain/functions.domain";
import setTheory from "@penrose/examples/dist/set-theory-domain/setTheory.domain";
import venn from "@penrose/examples/dist/set-theory-domain/venn.style";

export const error = {
  domain: `typeppp Set`,
  substancce: `Set A + B`,
  style: `
  Set a {

  }
  `,
};
export const oneSet = {
  domain: setTheory,
  substance: `
Set A
AutoLabel All
`,
  style: venn,
  variation: "",
};

export const continuousMap = {
  substance: continuousMapSubstance,
  style: continuousMapStyle,
  domain: functionsDomain,
  variation: "",
};

export const vectorWedge = {
  variation: "ArtemisCrane740",
  domain: exteriorAlgebraDomain,
  substance: vectorWedgeSubstance,
  style: exteriorAlgebraStyle,
};

export const vectorsPerp = {
  variation: "MyrtleApe55311",
  domain: linearAlgebraDomain,
  substance: twoVectorsPerpSubstance,
  style: linearAlgebraStyle,
};
