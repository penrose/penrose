# The Language API

Penrose provides convinience functions for easier integration of Penrose languages in web applications. Check out our docs on [React](./react) and [SolidJS](./solid) integration.

For lower-level integration, check out the [optimization API](./optimization-api).

## Example

Below is an example of compiling, optimizing, and rendering a diagram in Penrose programmatically. Here we define a simple trio of Substance, Style, and Domain programs in `trio.js`.

::: code-group

```javascript [script.js]
import { compile, optimize, toSVG, showError } from "@penrose/core";
import trio from "./trio.js";

const compiled = await compile(trio);
// handle compilation errors
if (compiled.isErr()) {
  throw new Error(showError(compiled.error));
}
const converged = optimize(compiled.value);
// handle optimization errors
if (converged.isErr()) {
  throw new Error(showError(converged.error));
}
// render the diagram state as an SVG
const rendered = await toSVG(converged.value, async () => undefined);
const container = document.getElementById("diagram");
container.appendChild(rendered);
```

```javascript [trio.js]
const domain = `
type Set
predicate IsSubset(Set s1, Set s2)
`;
const style = `
canvas {
  width = 800
  height = 700
}

forall Set x {
  x.icon = Circle {
    strokeWidth : 0
  }

  x.text = Equation {
    string : x.label
    fontSize : "32px"
  }

  ensure contains(x.icon, x.text)
  encourage sameCenter(x.text, x.icon)
  x.textLayering = x.text above x.icon
}

forall Set x; Set y
where IsSubset(x, y) {
  ensure disjoint(y.text, x.icon, 10)
  ensure contains(y.icon, x.icon, 5)
  x.icon above y.icon
}
`;
const substance = `
Set A, B, C, D, E, F, G

IsSubset(B, A)
IsSubset(C, A)
IsSubset(D, B)
IsSubset(E, B)
IsSubset(F, C)
IsSubset(G, C)

Not(Intersecting(E, D))
Not(Intersecting(F, G))
Not(Intersecting(B, C))

AutoLabel All
`;

export default { domain, substance, style, variation: "test" };
```

```html [index.html]
<!doctype html>
<html>
  <body>
    <div id="diagram"></div>
  </body>
</html>
```

:::

## Reference

This section describes the public API for Penrose; there are other things
exported, but those are not currently considered part of the public API, so they
may change. In contrast, any breaking change to these particular items must be
accompanied by a bump to the Penrose major version, so you can rely on them via
[SemVer][].

### `PenroseState`

This type holds all the data for a Penrose diagram that has already been
compiled. You can pass it to `step` or `stepTimes` or `optimize` to get a new
`PenroseState`, or you can display it to the user via `toSVG`.

### `diagram`

This is a convenience function which encapsulates usage of `PenroseState`: it
just takes in a Penrose trio, an HTML element to attach the diagram to, and a
function for resolving paths to embedded SVG images.

Example:

```html
<!doctype html>
<html>
  <body>
    <div id="diagram"></div>
  </body>
</html>
```

```javascript
import { fetchResolver } from "@penrose/components";
import { diagram } from "@penrose/core";
import trio from "./trio.js";

await diagram(trio, document.getElementById("diagram"), fetchResolver);
```

### `compile`

This function takes a Penrose trio and returns a `PenroseState`; see the example
at the top of this page. In particular it returns a [`Promise`][promise] of a
[`Result`][true-myth/result] of a `PenroseState`; refer to those links to find
more detail on how to use those generic types. The error case of the `Result` is
a `PenroseError`; see below.

### `optimize`

This function takes a `PenroseState` and fully optimizes it, then returns a
[`Result`][true-myth/result] of a `PenroseState`, where the error case is a
`PenroseError`. See the example at the top of this page.

### `step`

This function takes a `PenroseState` and some options, currently the only one of
which is a callback; Penrose will keep optimizing `until` this callback returns
`false`, then return a [`Result`][true-myth/result] of a `PenroseState`, where
the error case is a `PenroseError`.

Example:

```javascript
import { PenroseState, step } from "@penrose/core";

const stepMillis = (state, millis) => {
  let elapsed = false;
  setTimeout(() => {
    elapsed = true;
  }, millis);
  return step(state, { until: () => elapsed });
};
```

### `stepTimes`

This function takes a `PenroseState` and some options, currently the only one of
which is a callback; Penrose will keep optimizing `until` this callback returns
`false`, then return a [`Result`][true-myth/result] of a `PenroseState`, where
the error case is a `PenroseError`.

### `isOptimized`

This function takes in a `PenroseState` and returns `true` iff its layout is
already fully optimized.

Example:

```javascript
import { compile, isOptimized, showError } from "@penrose/core";

const example = async (trio) => {
  const compiled = await compile(trio);
  if (compiled.isErr()) throw Error(showError(compiled.error));
  console.log(isOptimized(compiled.value)); // false
  const optimized = optimize(trio);
  console.log(isOptimized(optimized.value)); // true
  return optimized;
};
```

### `PenroseError`

This type represents an error returned from Penrose. To consume it, see
`showError` below.

### `showError`

```javascript
import { compile, showError } from "@penrose/core";

const compiled = await compile({
  substance: "howdy pardner",
  style: "yeehaw",
  domain: "this diagram ain't big enough for the two of us",
  variation: "super varied",
});

if (compiled.isErr()) {
  // spoiler alert: it is indeed Err
  console.error(compiled.error);
}
```

### `toSVG`

This function renders a `PenroseState` as an `SVGSVGElement`.

[continuous map]: https://penrose.cs.cmu.edu/try/?examples=set-theory-domain%2Fcontinuousmap
[promise]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise
[semver]: https://semver.org/
[true-myth/result]: https://github.com/true-myth/true-myth/tree/v4.1.1#result-with-a-functional-style
