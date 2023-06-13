import { Var, variable } from "@penrose/core";
import { Vectors } from "@penrose/solids";
import { render } from "solid-js/web";

const vec = (x: number, y: number): [Var, Var] => [variable(x), variable(y)];

export default async () => {
  const a1 = vec(1, 0.5);
  const a2 = vec(0.5, 1);
  const v = vec(2, 3);
  const elem = document.createElement("div");
  render(() => Vectors({ a1, a2, v }), elem);
  return (elem.firstChild?.firstChild as SVGSVGElement).outerHTML;
};
