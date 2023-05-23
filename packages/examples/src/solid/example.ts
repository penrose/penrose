import { example } from "@penrose/solids";
import { render } from "solid-js/web";

export default async () => {
  const elem = document.createElement("div");
  render(example, elem);
  return elem.innerHTML;
};
