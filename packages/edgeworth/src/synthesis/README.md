# `synthesis` module

This module exports the `Synthesizer` class, which contains the core functionalities of the Penrose synthesizer. Packages such as `@penrose/synthesizer` depend on `Synthesizer`. The module also exports the `SynthesizerSettings` interface, which is necessary for initializing the synthesizer.

## How `Synthesizer` works

- `Synthesizer` is an object that maintains its state in its `cxt` field.
- `cxt` is a `SynthesisContext` object, which maintains the context when synthesizing **a single Substance program**. `Synthesizer` explicitly `reset`s the context every time it generates a new Substance program.
  - `SynthesizerContext` keeps a Substance AST as `Synthesizer` synthesizes a new Substance program. `Synthesizer` is responsible for generating the necessary data for mutating the AST (e.g. a statement to add/delete/edit), and `SynthesizerContext` will perform the actual operations and do the necessary bookkeeping (e.g. recording mutation operations).
- When interacting with the Substance AST, both classes use pure functions from `analysis/SubstanceAnalysis` to create or transform AST nodes.
