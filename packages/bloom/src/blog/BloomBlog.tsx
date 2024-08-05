import MarkdownIt from "markdown-it";
import mdMJ from "markdown-it-mathjax3";
import { highlight, languages } from "prismjs";
import "prismjs/components/prism-typescript";
import "prismjs/themes/prism.css";
import { useCallback, useState } from "react";
import Editor from "react-simple-code-editor";
import { DiagramBuilder, canvas, useDiagram } from "../../lib";
import constraints from "../../lib/core/constraints.ts";
import Renderer from "../../lib/react/Renderer.tsx";
import EigenvectorsDiagram from "../examples/eigen.tsx";
import RaysComponent from "../examples/rays.tsx";
import { sets } from "../examples/sets.ts";
import CirclePackingDiagram from "./CirclePackingDiagram.tsx";
import "./blog.css";
import { reflection } from "./reflection.ts";

const md = MarkdownIt().use(mdMJ);

const circleDiagramInput = `const Tiddlywink = type();

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

const reflectionDiagramInput = `const r1y = ray1.normVec[1];
const r2y = ray2.normVec[1];
ensure(constraints.equal(r1y, mul(-1, r2y)));`;

const P = (props: { children: string }) => (
  <p dangerouslySetInnerHTML={{ __html: md.render(`${props.children}`) }}></p>
);

export default function BloomBlog() {
  const [circleInput, setCircleInput] = useState(circleDiagramInput);

  const [reflectionInput, setReflectionInput] = useState(
    reflectionDiagramInput,
  );

  const circleDiagram = useDiagram(
    useCallback(async () => {
      const db = new DiagramBuilder(canvas(800, 200), "", 100);
      const { type, forall, circle, ensure } = db;
      constraints; // to force import for eval
      eval(circleInput);
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
        maxWidth: "min(100%, 40em)",
        marginLeft: "auto",
        marginRight: "auto",
        fontFamily: '"Trebuchet MS", sans-serif',
        fontSize: "large",
        lineHeight: "1.5em",
      }}
    >
      <h1>Bloom</h1>
      Optimization-driven diagram creation with differentiable JS
      <br />
      <br />
      <em>Powered by Penrose</em>
      <div
        style={{
          height: "0.1em",
          marginTop: "1em",
          marginBottom: "1em",
          backgroundColor: "darkgray",
        }}
      />
      We are excited to announce Bloom, an open-source JavaScript library for
      interactive diagram creation. Bloom uses optimization to satisfy
      constraints and evolve your diagram, so you can focus on creating engaging
      experiences. Try interacting with the diagrams below:
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          justifyContent: "space-evenly",
        }}
      >
        <EigenvectorsDiagram />
      </div>
      <h3>Overview</h3>
      Interactive diagrams are a powerful tool for teaching, learning, and
      exploring. But creating them is often a time-consuming and ad-hoc process,
      requiring endless event-handler juggling or even a small engine around
      each diagram.
      <br />
      <br />
      Bloom provides an alternative: first, describe your diagram using a
      library of shape declarations and differentiable constraints, objectives,
      and operations. Then, Bloom compiles it into efficient WebAssembly, and
      optimizes the diagram to satisfy your constraints live during user
      interaction.
      <br />
      <br />
      Below is a short Bloom program drawing a couple of circles. Try
      uncommenting the <code>disjoint</code> constraint, and watch what happens!
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
      All of the diagrams in this post are interactive: try dragging things
      around!
      <h3>Optimization-Driven Diagramming</h3>
      Much of the language we use to describe diagrams involves either
      constraints we would like to satisfy, or objectives we would like to
      maximize. For example, we might specify that two polygons must be
      “touching”, or that two circles are “near” to each other. Penrose, a
      system for generating diagrams from plaintext, lets the user describe
      their diagrams using an extensive library of these descriptions, and then
      optimizes the diagram to satisfy them. Bloom builds on this, allowing you
      to describe these diagrams directly in JavaScript, and continuously
      optimizing while the user interacts with objects in your scene.
      <br />
      <br />
      <P>
        In the following, we would like a ray to bounce specularly off the
        mirror, no matter where the user drags each endpoint. We _could_
        calculate the _exact_ intersection such that the $y$ coordinate of the
        ray is reversed, but why not let the optimizer do the work?
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
      Here’s another example: to prevent objects from overlapping as we drag
      them around, you would typically need to specify a complex interaction of
      forces, update rules, and collision detection. With Bloom, you can let the
      optimizer handle this with the <code>disjoint</code> constraint:
      <div
        style={{
          height: "30em",
          marginTop: "1em",
          fontFamily: "monospace",
        }}
      >
        <CirclePackingDiagram />
      </div>
      Bloom also exposes the{" "}
      <a href={"https://penrose.cs.cmu.edu/docs/ref/style/functions"}>
        extensive library
      </a>{" "}
      of numerical and shape-based constraints, objectives, and functions from
      Penrose.
      <h3>Declarative, Ergonomic Syntax</h3>
      Diagramming in Bloom closely mirrors the process in Penrose:
      <ol>
        <li>
          Define the types of objects (“substances”) and predicates between
          objects you’re going to use:
          <div
            style={{
              marginTop: "2em",
              marginBottom: "2em",
              fontFamily: "monospace",
            }}
          >
            <Editor
              value={`const Line = type();
const Point = type();
const IntersectAt = predicate();`}
              onValueChange={() => {}}
              highlight={(value) => highlight(value, languages.ts, "ts")}
              padding={"1em"}
              className={"editor"}
              readOnly={true}
            />
          </div>
        </li>
        <li>
          Instantiate these substances and predicates according to what should
          show up in your diagram:
          <div
            style={{
              marginTop: "2em",
              marginBottom: "2em",
              fontFamily: "monospace",
            }}
          >
            <Editor
              value={`const line1 = Line();
const line2 = Line();
const intersection = Point();

IntersectAt(line1, line2, intersection);`}
              onValueChange={() => {}}
              highlight={(value) => highlight(value, languages.ts, "ts")}
              padding={"1em"}
              className={"editor"}
              readOnly={true}
            />
          </div>
        </li>
        <li>
          Define styling rules for substances, groups of them, and predicates:
          <div
            style={{
              marginTop: "2em",
              marginBottom: "2em",
              fontFamily: "monospace",
            }}
          >
            <Editor
              value={`forall({ l: Line }, ({ l }) => {
  l.icon = line();
}

forall({ p: Point }, ({ p }) => {
  p.icon = circle({
    r: 5,
    fillColor: [0, 0, 0, 1],
  });
});

forallWhere(
  { l1: Line, l2: Line, p: Point },
  ({ l1, l2, p }) => IntersectAt.test(l1, l2, p),
  ({ l1, l2, p }) => {
  ensure(constraints.collinearOrdered(
    l1.icon.start, p.icon.center, l1.icon.end
  ));
  ensure(constraints.collinearOrdered(
    l2.icon.start, p.icon.center, l2.icon.end
  ));
});`}
              onValueChange={() => {}}
              highlight={(value) => highlight(value, languages.ts, "ts")}
              padding={"1em"}
              className={"editor"}
              readOnly={true}
            />
          </div>
        </li>
      </ol>
      Unlike in Penrose, there is no enforced separation between these steps.
      The whole world of JavaScript is yours to exploit! You can conditionally
      define substances, use loops in styling selectors, and much more.
      <br />
      <br />
      We recommend sticking to the process above, however : )
      <h3>Misc Features</h3>
      <h4>React Integration</h4>
      Bloom integrates tightly with React, providing rendering components and
      custom hooks to streamline diagram development. Compiled Bloom diagrams
      also provide a programmatic API for live site integration:
      <h4>Differentiable Programming</h4>
      Bloom leverages the differentiable programming model of Penrose, allowing
      efficient and stable calculation of gradients for optimization. We expose
      the both the computation library of Penrose and the autodifferentiation
      library of RoseJS.
      <h3>Getting started</h3>
      Check out our docs!
      <RaysComponent />
    </div>
  );
}
