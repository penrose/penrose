# How to generate TypeScript types

* Run `stack install` to build the Penrose main library and the `tsdef` module for conversion to TS types.
* In this directory, run `tsdef > types.d.ts`.
* If `tsdef` is not found, run `stack install tsdef`.
* All the exported TS types will be defined in `types.d.ts`.


