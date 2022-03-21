import Link from "@docusaurus/Link";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import Layout from "@theme/Layout";
import clsx from "clsx";
import * as React from "react";
//import DemoWrapper from "../components/DemoWrapper";
import styles from "./index.module.css";

function HomepageHeader() {
  const { siteConfig } = useDocusaurusContext();
  return (
    <header className={clsx("hero hero--primary", styles.heroBanner)}>
      <div className="container">
        <h1 className="hero__title">{siteConfig.title}</h1>
        <p className="hero__subtitle">{siteConfig.tagline}</p>
        <div className={styles.buttons}>
          <Link
            className="button button--secondary button--lg"
            to="/docs/tutorial/welcome"
          >
            Tutorial
          </Link>
          <Link
            className="button button--tertiary button--lg"
            to="pathname:///try/"
          >
            Try
          </Link>
        </div>
      </div>
    </header>
  );
}

export default function Home() {
  const { siteConfig } = useDocusaurusContext();

  const trio = {
    dsl: "type Set",
    sub: "Set A\nAutoLabel All",
    sty: `canvas {
  width = 500
  height = 500
}
  Set X {
  X.shape = Circle { strokeWidth : 0 }
  X.text  = Equation { string: X.label }
  ensure contains(X.shape, X.text)
  ensure maxSize(X.shape, canvas.width / 2)
}
`,
  };
  const examples = [
    { variation: "foo", ...trio },
    { variation: "bar", ...trio },
    { variation: "baz", ...trio },
  ];

  return (
    <Layout
      title={`Home`}
      description="Create beautiful diagrams just by typing math notation in plain text."
    >
      <HomepageHeader />
      <main style={{ padding: "1em", width: "1000px", margin: "auto" }}>
        {/* Temporarily commented out & will re-visit
        <h1>Example</h1>
        <p>Here's Penrose running in your browser:</p>
        <DemoWrapper
          style={{ margin: "auto" }}
          examples={examples}
          width="400px"
        />
        */}
      </main>
    </Layout>
  );
}
