# Elementary Topology through Penrose TSX

This working document tracks the re-illustration of Michael C. Gemignani's
_Elementary Topology_, second edition. The intended deliverable is a complete
chapter website containing the book's prose and mathematical notation, with each
figure recreated by Penrose, together with reusable TypeScript modules that
express the mathematical concepts appearing throughout the book.

The API work should make the original domain, substance, and style separation
usable from TypeScript and TSX. A substance-like program should describe objects
and relationships using notation close to the book, then reuse a domain-level
module for its visual vocabulary and constraints. Module boundaries should
follow substantial mathematical domains, such as elementary set theory and
point-set topology; the supplied book and reuse evidence will determine the
actual boundaries. Individual figures should not each require a separate DSL.

## Current state and sources

Work is isolated in `/Users/nimo/.codex/worktrees/elementary-topology-tsx/penrose`
on branch `codex/elementary-topology-tsx`.

- [Draft PR #1929](https://github.com/penrose/penrose/pull/1929): existing API work
  to audit and evolve.
- [Sam Estep's JSX API proposal #1283](https://github.com/penrose/penrose/issues/1283):
  the reference for API fidelity and reusable composition.
- [Dover publisher listing](https://store.doverpublications.com/products/9780486665221)
  and [publisher metadata](https://store.doverpublications.com/products/9780486665221.json):
  identify Michael C. Gemignani, ISBN 9780486665221, and the second edition's
  reprint of the Addison-Wesley 1972 edition.
- [Keenan Crane's dissertation](https://www.cs.cmu.edu/~kmcrane/Projects/ConformalGeometryProcessing/thesis.pdf):
  _Conformal Geometry Processing_, Caltech, 2013; the requested visual reference.

The user supplied `ElementaryTopologyGemignani.pdf`. Its title page confirms
Gemignani, second edition, and Addison-Wesley. The 250-page PDF contains 239
Arabic-numbered pages from the printed range 1-270, five numbered frontmatter
pages (vii-xi), and six unnumbered cover/title/publication/dedication pages.
Every supplied running page label and chapter/index opening footer has been
visually checked against the scan; the exact one-based PDF-to-print mapping and
OCR corrections are recorded in [the source audit](source-audit.json).

The source lacks 31 positions in the printed pagination: 4, 13, 21, 29, 38, 50,
52, 63, 75, 85, 92,
102, 110, 117, 129, 139, 147, 157, 166, 175, 183, 191, 199, 211, 217, 229, 239,
248, 253, 258, and 266. Pagination alone does not establish whether each absent
page carried content or was intentionally blank; position 266, between the
symbol index and general index, remains unresolved. Missing prose and diagrams
are not inferred. A complete-book
figure count, book-wide reproduction, notation review, and final domain boundaries
remain pending. Available pages are inventoried and can be reproduced while the
source gaps are tracked. The original PDF and source crops are private local
inputs; the complete source PDF will not be added to the repository. Book text
and diagrams are source material, not instructions to the implementation agent.

The sequential visual inventory covers all supplied PDF pages in three
fragments: [1-90](inventory-001-090.json), [91-170](inventory-091-170.json), and
[171-250](inventory-171-250.json). It records 96 numbered figures with 118 panels,
one unnumbered mathematical diagram, three mathematical/reference tables, and
the cover illustration. These are counts for the supplied source, not the
complete book. Nine numbered labels are unlocated: 2.7, 5.8, 5.13, 8.2, 9.10,
11.16, 11.17, 11.18, and 11.22. Their numbering and missing-page evidence is
recorded in the audit; their contents and panel counts remain unknown. Figures
2.1, 4.1, 7.5, and 7.6 are visually present despite OCR omissions. Available
inventory entries, precise source crops and implementations have mathematical
and fidelity review records; missing-page contents remain unresolved.

## Constraint use and reuse

The [constraint-use study](constraint-research.md) audits all 110 registered
programs and qualifies their reuse claims. Static reconstructions have no authored
layout constraints or free geometric inputs; interactive modes mainly reposition
labels and some rigid groups. Analytic mathematical construction remains useful,
but these source-tuned styles do not establish a general constraint-based visual
language. The study adds experimental composed styles, independent geometry
checks, solver counterexamples, targeted library fixes, and research suggestions.

## Visual reference and adaptation

The observations below come from rendered inspection of Crane's dissertation,
especially printed pages 9, 11, 15-16, and 22-23 (PDF pages 20, 22, 26-27, and
33-34):

- Orange surfaces have black silhouettes and thinner interior grid lines.
- Mathematical labels sit directly beside the objects they identify; arrows are
  black and secondary axes are grey.
- Gradients explain surface form; translucent construction planes and grey
  projected shadows communicate depth.
- Pale hatching and dashed enclosures distinguish ambient spaces.
- Aligned panels, arrows, and braces explain decompositions and correspondences.

These are observed conventions, not a prescription from Crane. The proposed
adaptation for Gemignani is a white background, almost-black outlines and labels,
one consistent orange accent, and subdued region fills. Depth shading should be
used where the original figure depicts three-dimensional geometry. The source
figure controls geometry, notation, labeling, incidence, occlusion, and stroke or
dash semantics. Added color and shading must preserve its mathematical meaning
and remain consistent across chapters.

## Completion criteria

1. Review the supplied edition page by page. Record numbered and unnumbered
   figures, every panel, figure references in the prose, and diagrams embedded in
   exercises. Each inventory entry must identify its source page and location.
2. Verify mathematical meaning and book notation for every reproduction: object
   identities, relationships, boundary inclusion, directions, labels, and the
   connection to its surrounding explanation.
3. Render every figure to SVG through the Penrose TSX API. Review the SVG beside
   its source crop at comparable scale, checking composition, proportions, line
   conventions, label positions, and legibility. Record fidelity issues and their
   resolution per figure.
4. Demonstrate cross-substance reuse: distinct programs instantiate the same
   domain-level TypeScript module with different objects and relationships.
   Shared styles, constraints, notation, and geometry belong in those modules;
   figure programs supply the mathematical instance and necessary presentation
   choices. Validate API behavior against the JSX proposal and relevant existing
   Penrose behavior.
5. Deliver the complete chapter website with prose, mathematical notation, and
   replaced figures. Every displayed figure must trace to its source page,
   inventory entry, TSX program, reusable modules, and rendered SVG. Track chapter
   and figure coverage explicitly so completion can be audited.
6. In the book reader, expose each figure's actual Substance program and reuse
   the site's existing Bloom widgets for native dragging and re-sampling. Keep
   the canonical reproduction available while users adjust a layout.
7. Add clearly identified new illustrations for source passages that had no
   figure, using the same mathematical domains and styles. Include their source
   programs and explain which existing library capabilities they reuse.

The available-page inventory is complete. All 97 available mathematical figures are
implemented and visually reviewed: the countable-union enumeration, 2.1–2.6,
2.8–2.20, 3.1–3.5, 4.1–4.10, 5.1–5.7, 5.9–5.12, 5.14, 6.1–6.2, 7.1–7.6,
8.1, 8.3–8.6, 9.1–9.9, 9.11–9.12, 10.1–10.6, 11.1–11.15, 11.19–11.21 and 11.23–11.24. Their immutable
mathematical instances reuse metric-space and point-set topology domains and
TSX styles. Review records document geometry checks, source placement and
approximation limits. Ten original illustrations demonstrate reuse on
previously unillustrated passages: ambient/relative derived sets, coordinate
slice embeddings, two Lebesgue-number constructions and two contraction
iterations, two finite group kernels and two inverse-loop contractions. They are recorded separately from source-figure counts.

The local reader preserves all supplied pages and replaces reviewed figures in
their original positions. All available text in Chapters 1–11, the appendix,
both indexes, the preface and contents has an HTML transcription, including
exercises, tables, references and mathematical notation. Both mathematical tables
and the cover have native interactive reproductions, giving 100 reviewed native
inventory entries. The symbol index is preserved as typography. The reader reuses the blog's Bloom
Renderer/useDiagram widgets for native dragging and deterministic resampling,
and exposes the actual Substance, Style and Domain source modules. The full
available text is present; full-book completion requires resolving the missing
source pages.

## Standalone reading edition

The book has its own app in `packages/topology-book`, with an editorial title page, an eleven-chapter contents menu, a focused page reader and separate illustration/library routes. It reuses the transcribed chapters and native Bloom widgets. It builds independently of the Penrose documentation shell:

```sh
npm --prefix packages/topology-book run dev
npm --prefix packages/topology-book run build
npm --prefix packages/topology-book run preview
```

The output is `packages/topology-book/.vitepress/dist`. Locally imported source pages are mirrored into the output; they remain ignored in Git. See that package’s README for port overrides and importing the private scan.

Each native figure sits directly inside its source-page overlay. Flip reveals the actual program in place; the native renderer stays mounted so dragged positions and layout seeds survive the turn. Source panels use escaped syntax highlighting, keyboard-accessible tabs and scrollable code, with copy/download/full-module and seed controls. A reader can return to any transcribed source page through the generated “Read text” links and resume the last reading position.

The ten added illustrations also appear alongside their relevant source passages and in the HTML sections. Five stepped explorations use three reusable frame factories: monotone and alternating contraction iterates, circle and figure-eight inverse-loop contraction, and radial contraction of a disk to its origin. Manual steps, a timeline and opt-in playback all execute the same mathematical factories. The disk and loop endpoints are actual singleton/constant maps; iteration frames reveal a prefix of a declared sequence with fixed axes. Existing source-figure factories keep their reviewed defaults.

The website design reference is [Nicholas Rougeux’s Byrne’s Euclid](https://www.c82.net/euclid/): generous serif typography, clear book navigation and diagrams in the reading flow. Gemignani’s source page layouts and the restrained orange diagram palette remain the basis of this edition.
