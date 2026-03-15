/// <reference types="vite/client" />
import type { Diagram } from "@penrose/bloom";
import { Renderer } from "@penrose/bloom";
import { useEffect, useState } from "react";
import { createRoot } from "react-dom/client";

import "./gallery.css";
import type { Meta } from "./index.js";
import registry from "./registry.js";

type TsxEntry = {
  id: string;
  name: string;
};

type BuildDiagram = () => Promise<Diagram>;

const app = document.querySelector<HTMLDivElement>("#app");

if (!app) {
  throw new Error("Missing #app container");
}

const buildRegistry: Record<string, BuildDiagram> = {
};

const galleryModules = import.meta.glob("./bloom/gallery/**/*.tsx") as Record<
  string,
  () => Promise<{ buildDiagram?: BuildDiagram }>
>;

let buildQueue: Promise<void> = Promise.resolve();

const queueDiagramBuild = async (
  buildDiagram: BuildDiagram,
): Promise<Diagram> => {
  const nextDiagram = buildQueue.then(buildDiagram);
  buildQueue = nextDiagram.then(
    () => undefined,
    () => undefined,
  );
  return await nextDiagram;
};

const getBloomTsxExamples = (): TsxEntry[] =>
  [...registry.entries()]
    .filter(
      (
        entry,
      ): entry is [
        string,
        Meta & { trio: false; tsx: true; f: () => Promise<string> },
      ] => {
        const [id, meta] = entry;
        return (
          id.startsWith("bloom/") &&
          meta.trio === false &&
          "tsx" in meta &&
          meta.tsx
        );
      },
    )
    .map(([id, meta]) => ({
      id,
      name: meta.name ?? id,
    }))
    .sort((a, b) => a.name.localeCompare(b.name));

const examples = getBloomTsxExamples();

const loadBuildDiagram = async (id: string): Promise<BuildDiagram> => {
  const staticBuild = buildRegistry[id];
  if (staticBuild) {
    return staticBuild;
  }

  const galleryLoader = galleryModules[`./${id}.tsx`];
  if (!galleryLoader) {
    throw new Error("No active renderer is registered for this example.");
  }

  const mod = await galleryLoader();
  if (typeof mod.buildDiagram !== "function") {
    throw new Error("No active renderer is registered for this example.");
  }

  return mod.buildDiagram;
};

const useQueuedDiagram = (id: string) => {
  const [diagram, setDiagram] = useState<Diagram | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;
    let currentDiagram: Diagram | null = null;

    setDiagram(null);
    setError(null);

    loadBuildDiagram(id)
      .then((buildDiagram) => queueDiagramBuild(buildDiagram))
      .then((nextDiagram) => {
        if (cancelled) {
          nextDiagram.discard();
          return;
        }
        currentDiagram = nextDiagram;
        setDiagram(nextDiagram);
      })
      .catch((err) => {
        if (!cancelled) {
          setError(err instanceof Error ? err.message : String(err));
        }
      });

    return () => {
      cancelled = true;
      currentDiagram?.discard();
    };
  }, [id]);

  return { diagram, error };
};

function ExampleCard(props: { example: TsxEntry }) {
  const { diagram, error } = useQueuedDiagram(props.example.id);

  return (
    <article className="card" data-example-id={props.example.id}>
      <div className="card__meta">
        <h2>{props.example.name}</h2>
        <p>{props.example.id}</p>
      </div>
      <div className="card__canvas">
        {error ? (
          <p className="status status--error">Failed to render: {error}</p>
        ) : !diagram ? (
          <p className="status">Rendering...</p>
        ) : (
          <Renderer diagram={diagram} />
        )}
      </div>
    </article>
  );
}

function App() {
  return (
    <main className="page">
      <header className="hero">
        <p className="eyebrow">packages/examples</p>
        <h1>Bloom TSX Gallery</h1>
        <p className="subtitle">
          Rendering every registry entry with <code>tsx: true</code> under{" "}
          <code>bloom/</code>.
        </p>
        <p className="count">
          {examples.length} example{examples.length === 1 ? "" : "s"}
        </p>
      </header>
      <section className="gallery">
        {examples.map((example) => (
          <ExampleCard key={example.id} example={example} />
        ))}
      </section>
    </main>
  );
}

createRoot(app).render(<App />);
