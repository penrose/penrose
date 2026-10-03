# Elementary Topology website

A standalone book website. Its custom VitePress theme contains no Penrose documentation navigation. The original-page reader, transcribed chapters, interactive figures, extra illustrations, and mathematical library share the existing book source in `packages/docs-site`; prose and figure implementations are not duplicated.

From the repository root, after the Bloom dependencies have been built:

```sh
npm --prefix packages/topology-book run dev
npm --prefix packages/topology-book run build
npm --prefix packages/topology-book run preview
```

Development uses `http://127.0.0.1:5200`, preview uses `http://127.0.0.1:5201`, and the static output is `packages/topology-book/.vitepress/dist`. For an alternate preview port, run the repository's VitePress binary directly from this package:

```sh
cd packages/topology-book
../../node_modules/.bin/vitepress preview --host 127.0.0.1 --port 5202
```

Stop and restart the preview after rebuilding: VitePress preview caches the build's asset manifest.

The build refreshes compiled figure loaders, regenerates source-page transcription links, and mirrors the shared book's public assets into this package's ignored `public` directory. Native SVGs and source programs retain `/elementary-topology/` URLs. Imported scan pages are copied only if they already exist locally; the PDF and scans are never committed. To enable the original-page reader, use the existing private importer documented in `docs/elementary-topology/README.md` before building. The HTML chapters and native figures remain available without a scan.

Routes start at `/reader`, `/chapter-01/` through `/chapter-11/`, `/front-matter/`, `/back-matter/`, `/further-illustrations`, and `/api`. The standalone Markdown compiler adapts legacy book-reader links to these routes. Source gaps stay explicit: the supplied scan lacks thirty-one printed page positions.
