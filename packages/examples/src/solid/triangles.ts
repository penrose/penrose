import { Triangles } from "@penrose/solids";
import { render } from "solid-js/web";

export default async () => {
  const elem = document.createElement("div");
  render(() => Triangles({ seed: "", theta: 0 }), elem);
  return elem.innerHTML;
};
