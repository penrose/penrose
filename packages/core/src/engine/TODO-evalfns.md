# TODO

(for evalFns)

- add a new API function (where?) for `evalFns`
- write `genFn` based on `genOptProblem`

  - write a new version of `evalEnergyOnCustom` without the weight node stuff, and only on a single objective or constraint
  - change existing `energyAndGradCompiled` so it accepts Maybe<WeightInfo>

- write a `genFns` wrapper that looks at all opt functions in the state and caches their info in the state, in an { `fnName(args)` => { energy, gradient } } map
- write `evalFns` on multiple functions (and their gradients) that uses the cache if it exists
- run + test this
