import { Triangles } from "@penrose/solids";
import { render } from "solid-js/web";

export default async () => {
  const elem = document.createElement("div");
  await new Promise((f) => {
    const onFinish = () => {
      f(undefined);
    };
    render(() => Triangles({ seed: "foobar", theta: 0, onFinish }), elem);
  });
  return elem.innerHTML;
};
