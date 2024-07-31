import MarkdownIt from "markdown-it";
import mdMJ from "markdown-it-mathjax3";
import { highlight, languages } from "prismjs";
import "prismjs/components/prism-typescript";
import "prismjs/themes/prism.css";
import { useCallback, useState } from "react";
import Editor from "react-simple-code-editor";
import { DiagramBuilder, canvas, useDiagram } from "../../lib";
import Renderer from "../../lib/react/Renderer.tsx";
import EigenvectorsDiagram from "../examples/eigen.tsx";
import { sets } from "../examples/sets.ts";
import CirclePackingDiagram from "./CirclePackingDiagram.tsx";
import "./blog.css";
import { reflection } from "./reflection.ts";

const md = MarkdownIt().use(mdMJ);

const P = (props: { children: string }) => (
  <p dangerouslySetInnerHTML={{ __html: md.render(`${props.children}`) }}></p>
);

const circleDiagramInput = `const Tiddlywink = type();

const tw1 = Tiddlywink();
const tw2 = Tiddlywink();
const tw3 = Tiddlywink();

/* for (let i = 0; i < 100; ++i) {
  Tiddlywink();
} */`;

const reflectionDiagramInput = `const r1y = ray1.normVec[1];
const r2y = ray2.normVec[1];
ensure(constraints.equal(r1y, mul(-1, r2y)));`;

export default function BloomBlog() {
  const [circleInput, setCircleInput] = useState(circleDiagramInput);

  const [reflectionInput, setReflectionInput] = useState(
    reflectionDiagramInput,
  );

  const circleDiagram = useDiagram(
    useCallback(async () => {
      const db = new DiagramBuilder(canvas(800, 400), "");
      const { type, forall, circle } = db;
      eval(
        circleInput +
          `
        forall({ tw: Tiddlywink }, ({ tw }) => {
          tw.icon = circle({
            r: 30,
            strokeColor: [0, 0, 0, 1],
            strokeWidth: 3,
            drag: true,
          });
        });
      `,
      );
      return await db.build();
    }, [circleInput]),
  );
  const setsDiagram = useDiagram(useCallback(sets, []));

  const reflectionDiagram = useDiagram(
    useCallback(() => reflection(reflectionInput), [reflectionInput]),
  );

  return (
    <div
      style={{
        maxWidth: "min(100%, 50em)",
        marginLeft: "auto",
        marginRight: "auto",
        fontFamily: "Arial",
        fontSize: "large",
      }}
    >
      <h1>Bloom</h1>
      Optimization-driven diagram creation with differentiable JS
      <br />
      <br />
      <em>Powered by Penrose</em>
      <P>---</P>
      We are excited to announce Bloom, an open-source JavaScript library for
      interactive diagram creation. Bloom uses optimization to satisfy
      constraints and evolve your diagram, so you can focus on creating
      extensible, rewarding experiences. Try interacting with the diagrams
      below:
      <div
        style={{
          display: "flex",
          flexDirection: "row",
          width: "100%",
          height: "50em",
          justifyContent: "space-evenly",
        }}
      >
        <div
          style={{
            display: "flex",
            flexDirection: "column",
            width: "100%",
          }}
        >
          <CirclePackingDiagram />
          {setsDiagram && <Renderer diagram={setsDiagram} />}
        </div>
        <EigenvectorsDiagram />
      </div>
      <h3>Declarative</h3>
      <P>
        Bloom encourages you to describe your diagram's underlying objects,
        relationships, and interactions using ergonomic and reusable system
        types and selectors. Trying adding a couple (or a lot!) more circles to
        the diagram below:
      </P>
      <div
        style={{
          marginTop: "2em",
          fontFamily: "monospace",
        }}
      >
        <Editor
          value={circleInput}
          onValueChange={setCircleInput}
          highlight={(value) => highlight(value, languages.ts, "ts")}
          padding={"1em"}
          className={"editor"}
        />
        {circleDiagram && (
          <div
            style={{
              marginTop: "2em",
              height: "20em",
            }}
          >
            <Renderer diagram={circleDiagram} />
          </div>
        )}
      </div>
      <h3>Optimization-Driven</h3>
      Graphical calculations can be tedious and error-prone. But usually, only a
      small amount of information is needed to fully specify a diagram. Bloom
      lets you specify constraints and objectives for your diagram, and
      optimizes to satisfy them.
      <br />
      <br />
      <P>
        In the following, we would like a ray to bounce specularly off the
        mirror, no matter where the user drags each endpoint. We _could_
        calculate the intersection such that the $y$ coordinate of the ray is
        exactly reversed, but why not let the optimizer do the work?
      </P>
      <div
        style={{
          marginTop: "2em",
          fontFamily: "monospace",
        }}
      >
        <Editor
          value={reflectionInput}
          onValueChange={setReflectionInput}
          highlight={(value) => highlight(value, languages.ts, "ts")}
          padding={"1em"}
          className={"editor"}
        />
        {reflectionDiagram && (
          <div
            style={{
              marginTop: "2em",
              height: "20em",
            }}
          >
            <Renderer diagram={reflectionDiagram} />
          </div>
        )}
      </div>
    </div>
  );
}
