import {
  DiagramBuilder,
  Renderer,
  canvas,
  constraints,
  useDiagram,
} from "@penrose/bloom";
import * as prismjs from "prismjs";
import "prismjs/components/prism-typescript.js";
import "prismjs/themes/prism.css";
import { useCallback, useState } from "react";
import Editor from "react-simple-code-editor";

const preamble = `
const { DiagramBuilder, Renderer, canvas, useDiagram, constraints } = bloom;
const db = new DiagramBuilder(canvas(800, 400), "", 10000);
const { type, forall, circle, ensure } = db;`;

const postamble = `
return db.build();`;

const circleDiagramInput = `
const Tiddlywink = type();

const tw1 = Tiddlywink();
const tw2 = Tiddlywink();
const tw3 = Tiddlywink();

// try adjusting these parameters
forall({ tw: Tiddlywink }, ({ tw }) => {
  tw.icon = circle({
    r: 30,
    strokeColor: [0, 0, 0, 1],
    strokeWidth: 3,
    drag: true,
  });
});

forall({ tw1: Tiddlywink, tw2: Tiddlywink }, ({ tw1, tw2 }) => {
  // uncomment this |
  //                v
  // ensure(constraints.touching(tw1.icon, tw2.icon));
});`;

const build = async (circleInput: string) => {
  const bloom: Record<string, any> = {};
  bloom["DiagramBuilder"] = DiagramBuilder;
  bloom["Renderer"] = Renderer;
  bloom["canvas"] = canvas;
  bloom["useDiagram"] = useDiagram;
  bloom["constraints"] = constraints;
  return new Function("bloom", preamble + circleInput + postamble)(bloom);
};

export default function CirclesDiagram() {
  const [circleInput, setCircleInput] = useState(circleDiagramInput);
  const diagram = useDiagram(
    useCallback(() => build(circleInput), [circleInput]),
  );

  return (
    <div
      style={{
        marginTop: "2em",
        fontFamily: "monospace",
      }}
    >
      <Editor
        value={circleInput}
        onValueChange={setCircleInput}
        highlight={(value: string) =>
          prismjs.highlight(value, prismjs.languages.ts, "ts")
        }
        padding={"1em"}
        className={"editor"}
      />
      {diagram && (
        <div
          style={{
            marginTop: "2em",
            height: "20em",
          }}
        >
          <Renderer diagram={diagram} />
        </div>
      )}
    </div>
  );
}
