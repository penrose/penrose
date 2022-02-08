import Link from "@docusaurus/Link";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import { Demo } from "@penrose/components";
import Layout from "@theme/Layout";
import clsx from "clsx";
import * as React from "react";
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
            to="https://panes.penrose.ink"
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

  return (
    <Layout
      title={`Home`}
      description="Create beautiful diagrams just by typing math notation in plain text."
    >
      <HomepageHeader />
      <main>
        {/* <HomepageFeatures /> */}

        <Demo
          sub={"Set A"}
          sty={`
  canvas {
    width = 400
    height = 400
  }
  Set X { X.shape = Circle{} }
  `}
          dsl={"type Set"}
          variation={"foo"}
        />
      </main>
    </Layout>
  );
}
