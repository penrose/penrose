# @penrose/core

`@penrose/core` contains the core of the Penrose platform, including the compilation pipeline and the SVG renderer.

See the [website](https://penrose.cs.cmu.edu) for more details about the system in general, and see [@penrose/components](https://www.npmjs.com/package/@penrose/components) for React components wrapping around this core library.

## Getting started

If using Penrose in a web or Node application, run `yarn add @penrose/core` to install the package.

`core` contains some browser-only dependencies (e.g. MathJax for rendering equations). If using Node, we recommend using [`global-jsdom`](https://www.npmjs.com/package/global-jsdom) to shim them.

```ts
import "global-jsdom/register"; // must come before the Penrose import
import * as Penrose from "@penrose/core";
```

In a HTML document, use [JavaScript modules](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules) to import functions from `@penrose/core` through a CDN. For example, here we use [JSPM](https://jspm.org/) to generate an import map for dependencies and draw a simple diagram with Penrose:

```html
<!DOCTYPE html>
<html>
  <body>
    <!--
    JSPM Generator Import Map
    Edit URL: https://generator.jspm.io/#U2VhYGBkDM0rySzJSU1hcChIzSvKL07VT84vSnUw0jPUMwAAQT2FDyIA
  -->
    <script type="importmap">
      {
        "imports": {
          "@penrose/core": "https://ga.jspm.io/npm:@penrose/core@2.1.0/dist/index.js"
        },
        "scopes": {
          "https://ga.jspm.io/": {
            "@datastructures-js/heap": "https://ga.jspm.io/npm:@datastructures-js/heap@3.2.0/index.js",
            "@datastructures-js/queue": "https://ga.jspm.io/npm:@datastructures-js/queue@4.2.3/index.js",
            "@penrose/optimizer": "https://ga.jspm.io/npm:@penrose/optimizer@2.1.0/index.js",
            "consola": "https://ga.jspm.io/npm:consola@2.15.3/dist/consola.browser.js",
            "crypto": "https://ga.jspm.io/npm:@jspm/core@2.0.0/nodelibs/browser/crypto.js",
            "immutable": "https://ga.jspm.io/npm:immutable@4.2.2/dist/immutable.es.js",
            "lodash": "https://ga.jspm.io/npm:lodash@4.17.21/lodash.js",
            "mathjax-full/js/": "https://ga.jspm.io/npm:mathjax-full@3.2.2/js/",
            "mhchemparser/dist/mhchemParser.js": "https://ga.jspm.io/npm:mhchemparser@4.1.1/dist/mhchemParser.js",
            "moo": "https://ga.jspm.io/npm:moo@0.5.2/moo.js",
            "nearley": "https://ga.jspm.io/npm:nearley@2.20.1/lib/nearley.js",
            "pandemonium/choice": "https://ga.jspm.io/npm:pandemonium@2.4.1/choice.js",
            "pandemonium/random": "https://ga.jspm.io/npm:pandemonium@2.4.1/random.js",
            "poly-partition": "https://ga.jspm.io/npm:poly-partition@1.0.2/lib/index.js",
            "seedrandom": "https://ga.jspm.io/npm:seedrandom@3.0.5/index.js",
            "true-myth": "https://ga.jspm.io/npm:true-myth@4.1.1/dist/cjs/index.js"
          }
        }
      }
    </script>

    <!-- ES Module Shims: Import maps polyfill for modules browsers without import maps support (all except Chrome 89+) -->
    <script
      async
      src="https://ga.jspm.io/npm:es-module-shims@1.5.1/dist/es-module-shims.js"
      crossorigin="anonymous"
    ></script>

    <script type="module">
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
    </script>
    <div id="penrose-diagram" />
  </body>
</html>
```

## Exported functions

For the full list of exported functions, refer to the [documentation site](https://penrose.github.io/penrose/typedoc/modules.html).
