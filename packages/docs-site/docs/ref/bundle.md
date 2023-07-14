# Using Penrose with a Bundler

First install the package:

::: code-group

```shell [Npm]
npm i @penrose/core
```

```shell [Yarn]
yarn add @penrose/core
```

:::

`core` contains some browser-only dependencies (e.g. MathJax for rendering equations). If using Node, we recommend using [`global-jsdom`](https://www.npmjs.com/package/global-jsdom) to shim them.

```ts
import "global-jsdom/register"; // must come before the Penrose import
import * as Penrose from "@penrose/core";
```

Use [JavaScript modules](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules) to import functions from `@penrose/core`. Use either a CDN that supports ES modules (e.g. [JSPM](https://jspm.org/)) or a bundler (e.g. [vite](https://vitejs.dev/)). For example, run the following to set up starter project with vite:

```shell
yarn create vite
cd vite-project
yarn add @penrose/core
yarn add -D vite-plugin-top-level-await # `@penrose/optimizer` uses top-level await and this vite plugin adds support for that
```

In `vite.config.js`:

```js
import { defineConfig } from "vite";
import topLevelAwait from "vite-plugin-top-level-await";

export default defineConfig({ plugins: [topLevelAwait()] });
```

In `index.html`:

```html
<!DOCTYPE html>
<html>
  <body>
    <div id="penrose-diagram"></div>
    <script type="module" src="/main.js"></script>
  </body>
</html>
```

In `main.js`:

```js
import * as Penrose from "@penrose/core";
Penrose.diagram(
  {
    substance: `
  Set A, B
  IsSubset(A, B)
  `,
    style: `
  canvas {
      width = 400
      height = 400
  }
  forall Set s {
      s.shape = Circle {}
      ensure lessThan(20, s.shape.r)
  }
  forall Set s1, s2
  where IsSubset(s1, s2) {
      ensure contains(s2.shape, s1.shape)
      s2.shape above s1.shape
  }
  `,
    domain: `type Set
  predicate IsSubset(Set, Set)`,
  },
  document.getElementById("penrose-diagram")
);
```

Finally, run the following to see the output:

```shell
yarn dev
```
