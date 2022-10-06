import Link from "@docusaurus/Link";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import { examples } from "@penrose/examples";
import Layout from "@theme/Layout";
import clsx from "clsx";
import * as React from "react";
import DemoWrapper from "../components/DemoWrapper";
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

  const demo = [
    {
      dsl: examples["geometry-domain"]["geometry.dsl"],
      sub: examples["geometry-domain"]["teaser.sub"],
      sty: examples["geometry-domain"]["euclidean-teaser.sty"],
      variation: "WhisperPeafowl34044",
    },
    {
      dsl: examples["set-theory-domain"]["functions.dsl"],
      sub: examples["set-theory-domain"]["continuousmap.sub"],
      sty: examples["set-theory-domain"]["continuousmap.sty"],
      variation: "",
    },
    {
      dsl: examples["set-theory-domain"]["setTheory.dsl"],
      sub: examples["set-theory-domain"]["tree.sub"],
      sty: examples["set-theory-domain"]["venn.sty"],
      variation: "PlumvilleCapybara104",
    },
    {
      dsl: examples["lagrange-bases"]["lagrange-bases.dsl"],
      sub: examples["lagrange-bases"]["example.sub"],
      sty: examples["lagrange-bases"]["lagrange-bases.sty"],
      variation: "RainmakerMarten88256",
    },
  ];

  return (
    <Layout
      title={`Home`}
      description="Create beautiful diagrams just by typing math notation in plain text."
    >
      <HomepageHeader />
      <main style={{ padding: "3em 1em 3em", width: "80%", margin: "auto" }}>
        <p>
          Penrose is a platform that enables people to{" "}
          <b>
            create beautiful diagrams just by typing mathematical notation in
            plain text.
          </b>{" "}
          The goal is to make it easy for non-experts to create and explore
          high-quality diagrams and provide deeper insight into challenging
          technical concepts. We aim to democratize the process of creating
          visual intuition.
        </p>
        <p>
          <b>
            Check out our <a href="/siggraph20.html">SIGGRAPH '20 paper</a> and{" "}
            <a href="https://vimeo.com/416822487">video</a> on Penrose!
          </b>
        </p>
        <p>
          <b>
            Penrose is an early-stage system that is actively in development.
          </b>{" "}
          Feel free to <a href="mailto:penrose-team@cs.cmu.edu">get in touch</a>
          .
        </p>

        <h1>Example</h1>
        <p>Here's Penrose running in your browser:</p>
        <DemoWrapper style={{ margin: "auto" }} examples={demo} width="400px" />
      </main>
    </Layout>
  );
}
