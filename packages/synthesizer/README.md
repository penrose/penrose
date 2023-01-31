# `@penrose/synthesizer`: a program synthesizer for Penrose

This package is a command-line application that depends on `@penrose/core` and synthesizes Substance programs based on user-specification or by examples.

Usage:

```
Penrose Synthesizer.

Usage:
  penrose-synthesizer <domain> [options]

Options:
  --substance=SUB      A Substance program that will be included in every synthesized program
  --style=STY          A Style program that the synthesized programs work with
  --path=PATH          Output path for the generated programs (Trailing slash not needed)
                       [default: synthesized-progs]
  --registry=REGISTRY  Path for the generated registry
                       [default: registry.json]
  --num-programs=NUM   The number of programs to generate
                       [default: 1]
  --synth-setting=SET  A JSON file containing parameters for the synthesizer
```

## Getting started

- Follow the instruction in the [wiki page](https://github.com/penrose/penrose/wiki/Building-and-running) to install Penrose.
- Run one of the examples below to try out the synthesizer.

## Examples (w/ batch-rendering via `@penrose/automator`)

Examples are currently encoded as scripts in `package.json`. The source files are stored in `__tests__`.

- `yarn set-example`
- `yarn collinear-example`

To synthesize Substance programs in the set theory domain:

- Run `yarn set-example` in this directory.
  - The full command is: `yarn start __tests__/setTheory.domain --substance=__tests__/template.substance --style=__tests__/venn.style --path=../automator/progs --synth-setting=__tests__/set-settings.json --num-programs=30`. It takes in a Penrose trio and generates 30 programs according to a JSON configuration.
  - This command will print out all synthesized programs and mutation operations applied to each program.
  - The output is written to `../automator/progs`
- Navigate to `packages/automator` and run `yarn generate-site`, which is a shorthand for `yarn render progs browser`
- Finally, open `browser/index.html` to view the results. You can view the cross-instance energy and mutation operations in the "Synthesis" tab for each diagram.

## Development setup

Since the majority of the synthesis functionality are implemented as a module in `@penrose/core` right now, you will likely need to keep building `core` while developing the synthesizer.

- Run `yarn start` in the project root directory.
- Check the console before you run `synthesizer` to make sure your changes in `core` are not causing any errors.
- If the build is successful, `synthesizer` will now be using the most recent version of `core` when synthesizing Penrose programs.
