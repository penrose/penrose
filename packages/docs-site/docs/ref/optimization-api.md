# The Optimization API

The optimization API exposes low-level functionalities to construct and solve optimization problems. [Learn how to create an optimization problem](./constraints).

## `variable`

A `variable` represents a variable in an optimization problem, i.e. a number that the optimizer can change during optimization. `variable` takes a `val: number` as its initial value.

## `problem`

`problem` takes a description of an optimization problem defined as a list of constraints and/or an objective function. After setting up a problem, call `start()` before running the optimizer. `start()` returns:

- A `run` function with an optional `until` stopping condition. If `until` is not provided, the optimizer will run until it converges on a solution.
- A `converged` boolean flag for checking if the optimizer converged.

Examples:

Minimize $x$ such that $(x - 5)^2$, with an initial condition of $x = 10$.

```ts
import { variable, pow, sub, problem } from "@penrose/core";
const x = variable(10);
const problem = await problem({ constraints: [pow(sub(x, 5), 2)] });
const { vals } = problem.start({}).run({});
console.log(problem.converged); // true
console.log(x); // a value closer to 5
```

## All the arithematic functions

:::info
Coming soon!
:::
