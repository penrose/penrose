# @penrose/core

See the [wiki](https://github.com/penrose/penrose/wiki) for more details about the system in general.

Run `npm run install-cli` to add `roger` to your PATH.

## Parsing

We use [nearley.js](https://nearley.js.org/), a parser generator library, for parsing. Therefore, the build script will first generate the parser modules to `src/parser` at the beginning.

## Exposing new parts of the module

Export any new module parts by editing `src/module.tsx` and `index.d.ts`.

## 💂 Roger

To regenerate the README, run in `roger/`:

`npx oclif-dev readme`
