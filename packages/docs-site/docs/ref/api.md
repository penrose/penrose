# The Language API

Penrose provides convinience functions for easier integration of Penrose languages in web applications. Check out our docs on [React](./react) and [SolidJS](./solid) integration.

For lower-level integration, check out the [optimization API](./optimization-api).

## Example

Below is an example of compiling, optimizing, and rendering a diagram in Penrose programmatically. Here we use the [continuous map]() example from `@penrose/examples`.

::: code-group

```javascript [script.js]
import { compile, optimize, toSVG, showError } from "@penrose/core";
import continuousMap from "@penrose/examples/dist/set-theory-domain/continuousmap.trio";

const trio = {
  domain: continuousMap.domain,
  substance: continuousMap.substance,
  style: continuousMap.style[0].contents,
  variation: continuousMap.variation,
};
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
const rendered = await toSVG(converged.value, continuousMap[0].resolver);
const container = document.getElementById("diagram");
container.appendChild(rendered);
```

```html [index.html]
<!DOCTYPE html>
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
<!DOCTYPE html>
<html>
  <body>
    <div id="diagram"></div>
  </body>
</html>
```

```javascript
import { fetchResolver } from "@penrose/components";
import { diagram } from "@penrose/core";
import continuousMap from "@penrose/examples/dist/set-theory-domain/continuousmap.trio";

await diagram(continuousMap, document.getElementById("diagram"), fetchResolver);
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
