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
const EXAMPLE_PARAM = "example";

const app = document.querySelector<HTMLDivElement>("#app");

if (!app) {
  throw new Error("Missing #app container");
}

const buildRegistry: Record<string, BuildDiagram> = {};

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
        Meta & { trio: false; bloom: true; f: () => Promise<string> },
      ] => {
        const [id, meta] = entry;
        return (
          id.startsWith("bloom/") &&
          meta.trio === false &&
          "bloom" in meta &&
          meta.bloom
        );
      },
    )
    .map(([id, meta]) => ({
      id,
      name: meta.name ?? id,
    }))
    .sort((a, b) => a.name.localeCompare(b.name));

const examples = getBloomTsxExamples();

const findExample = (id: string | null): TsxEntry | null => {
  if (!id) {
    return null;
  }
  return examples.find((example) => example.id === id) ?? null;
};

const getSelectedExampleId = (): string | null =>
  findExample(new URLSearchParams(window.location.search).get(EXAMPLE_PARAM))
    ?.id ?? null;

const setSelectedExampleId = (id: string | null) => {
  const url = new URL(window.location.href);
  if (id) {
    url.searchParams.set(EXAMPLE_PARAM, id);
  } else {
    url.searchParams.delete(EXAMPLE_PARAM);
  }
  window.history.pushState({}, "", url);
};

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

function ExampleCard(props: {
  example: TsxEntry;
  inspected: boolean;
  onInspect: (id: string | null) => void;
}) {
  const { diagram, error } = useQueuedDiagram(props.example.id);

  return (
    <article
      className={`card${props.inspected ? " card--focused" : ""}`}
      data-example-id={props.example.id}
    >
      <div className="card__meta">
        <div className="card__heading">
          <h2>{props.example.name}</h2>
          <button
            className="card__action"
            type="button"
            onClick={() =>
              props.onInspect(props.inspected ? null : props.example.id)
            }
          >
            {props.inspected ? "Back to gallery" : "Inspect"}
          </button>
        </div>
        <p>{props.example.id}</p>
      </div>
      <div
        className={`card__canvas${
          props.inspected ? " card__canvas--focused" : ""
        }`}
      >
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
  const [selectedId, setSelectedId] = useState<string | null>(() =>
    getSelectedExampleId(),
  );

  useEffect(() => {
    const syncSelection = () => {
      setSelectedId(getSelectedExampleId());
    };

    window.addEventListener("popstate", syncSelection);
    return () => {
      window.removeEventListener("popstate", syncSelection);
    };
  }, []);

  const selectedExample = findExample(selectedId);
  const visibleExamples = selectedExample ? [selectedExample] : examples;
  const inspectExample = (id: string | null) => {
    setSelectedExampleId(id);
    setSelectedId(id);
  };

  return (
    <main className="page">
      <header className="hero">
        <p className="eyebrow">packages/examples</p>
        <h1>Bloom TSX Gallery</h1>
        <p className="subtitle">
          {selectedExample ? (
            <>
              Inspecting <code>{selectedExample.id}</code>.
            </>
          ) : (
            <>
              Rendering every registry entry with <code>tsx: true</code> under{" "}
              <code>bloom/</code>.
            </>
          )}
        </p>
        <div className="hero__controls">
          <label className="picker">
            <span>Inspect</span>
            <select
              value={selectedId ?? ""}
              onChange={(event) => inspectExample(event.target.value || null)}
            >
              <option value="">All examples</option>
              {examples.map((example) => (
                <option key={example.id} value={example.id}>
                  {example.name}
                </option>
              ))}
            </select>
          </label>
          <p className="count">
            {selectedExample
              ? "1 example in focus"
              : `${examples.length} examples`}
          </p>
        </div>
      </header>
      <section
        className={`gallery${selectedExample ? " gallery--focused" : ""}`}
      >
        {visibleExamples.map((example) => (
          <ExampleCard
            key={example.id}
            example={example}
            inspected={example.id === selectedId}
            onInspect={inspectExample}
          />
        ))}
      </section>
    </main>
  );
}

createRoot(app).render(<App />);
