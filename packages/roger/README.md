# `@penrose/roger`: a CLI for Penrose

This package is a command-line application that depends on `@penrose/core` and
processes Penrose diagrams. See [the docs on the Penrose website][website docs].

Usage:

```
roger <command>

Commands:
  roger trio [trio..]    Generate a diagram from a Penrose trio.
  roger trios [trios..]  Create diagrams from multiple .trio.json files.
  roger watch            Watch the current folder for files & changes (must end
                         in .substance, .style, .domain)
  roger shapedefs        Generate a JSON file that contains all shape definition
                         s in the Penrose system.

Options:
  --version  Show version number                                       [boolean]
  --help     Show help                                                 [boolean]
```

## Getting started

- Follow the instruction in [`CONTRIBUTING.md`](/CONTRIBUTING.md) to install Penrose.
- Run `roger trio packages/examples/src/set-theory-domain/tree-venn.trio.json` from the repo root. The output SVG will appear in the console.

## Using `roger` for local development

- If you are developing a module in `core`, you can run `yarn start` in the project root direcory, which will continuously watch your changes in `core` and update your build.
- Check the console before you run `roger` to make sure your changes in `core` are not causing any errors.
- If the build is successful, `roger` will now be using the most recent version of `core` when batch-processing Penrose programs.

[website docs]: https://penrose.cs.cmu.edu/docs/ref/using#command-line-interface-roger
