import MarkdownIt from "markdown-it";
import mdMJ from "markdown-it-mathjax3";
import { highlight, languages } from "prismjs";
import "prismjs/components/prism-typescript";
import "prismjs/themes/prism.css";
import { useCallback, useState } from "react";
import Editor from "react-simple-code-editor";
import { DiagramBuilder, canvas, useDiagram } from "../../lib";
import Renderer from "../../lib/react/Renderer.tsx";
import CirclePackingDiagram from "./CirclePackingDiagram.tsx";
import "./blog.css";

const md = MarkdownIt().use(mdMJ);

const P = (props: { children: string }) => (
  <p dangerouslySetInnerHTML={{ __html: md.render(`${props.children}`) }}></p>
);

const userDiagramSubstance = `
const Circle = type();

const circle1 = Circle();
const circle2 = Circle();
const circle3 = Circle();
`;

const userDiagramStyle = `
forall({ c: Circle }, ({ c }) => {
  c.icon = circle({
    r: 30,
    drag: true,
  });
});
`;

export default function BloomBlog() {
  const [substance, setSubstance] = useState(userDiagramSubstance);
  const [style, setStyle] = useState(userDiagramStyle);

  const diagram1 = useDiagram(
    useCallback(async () => {
      const db = new DiagramBuilder(canvas(400, 400), "");
      const { type, forall, circle } = db;
      eval(substance + userDiagramStyle);
      return await db.build();
    }, [substance]),
  );

  const diagram2 = useDiagram(
    useCallback(async () => {
      const db = new DiagramBuilder(canvas(400, 400), "");
      const { type, forall, circle } = db;
      eval(substance + style);
      return await db.build();
    }, [substance, style]),
  );

  return (
    <div
      style={{
        maxWidth: "min(100%, 40em)",
        marginLeft: "auto",
        marginRight: "auto",
        fontFamily: "Arial",
      }}
    >
      <h1>Bloom</h1>
      Declarative interactive diagramming with differentiable JS
      <br />
      <br />
      <em>Powered by Penrose</em>
      <P>---</P>
      We are excited to announce Bloom, an open-source TypeScript library for
      interactive diagram creation. Bloom uses simple, readable constructs and
      differentiable programming to generate reactive and extensible diagrams.
      Try dragging the circles below!
      <div
        style={{
          height: "20em",
        }}
      >
        <CirclePackingDiagram />
      </div>
      <h3>Declarative</h3>
      Bloom encourages you to describe your diagram's underlying objects,
      relationships, and interactions before worrying about styling. Try adding
      a new circle below:
      <div
        style={{
          marginTop: "1em",
          fontFamily: "monospace",
        }}
      >
        <Editor
          value={substance}
          onValueChange={setSubstance}
          highlight={(value) => highlight(value, languages.ts, "ts")}
          padding={"1em"}
        />
        {diagram1 && (
          <div
            style={{
              height: "20em",
            }}
          >
            <Renderer diagram={diagram1} />
          </div>
        )}
      </div>
      <P>
        `Circle` is a new `type`, or class or diagram objects, and calling
        `Circle()` instantiates a `substance` of type `Circle`. To actually draw
        these substances, Bloom provides simple selectors:
      </P>
      <div
        style={{
          marginTop: "1em",
          fontFamily: "monospace",
        }}
      >
        <Editor
          value={style}
          onValueChange={setStyle}
          highlight={(value) => highlight(value, languages.ts, "ts")}
          padding={"1em"}
        />
        {diagram2 && (
          <div
            style={{
              height: "20em",
            }}
          >
            <Renderer diagram={diagram2} />
          </div>
        )}
      </div>
    </div>
  );
}
