# Using Penrose with Vanilla JS

Both the [Language API](./api) and [Optimization API](./optimization-api) are exported from `@penrose/core`, currently released as an [ECMAScript module] (ESM). If you are making a web page without a build tool, you can use one of the CDNs with built-in ESM support. Here's an example of using [JSPM], where we use the [JSPM Generator] to create the HTML boilerplate for importing our dependencies.

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>Untitled</title>
    <meta name="viewport" content="width=device-width, initial-scale=1" />
  </head>
  <body>
    <!--
    JSPM Generator Import Map
    Edit URL: https://generator.jspm.io/#U2VhYGBkDM0rySzJSU1hcChIzSvKL07VT84vSnUw1jPQM9BNzCnISNQzAAA0lZvKKgA
  -->
    <script type="importmap">
      {
        "imports": {
          "@penrose/core": "https://ga.jspm.io/npm:@penrose/core@3.0.0-alpha.0/dist/index.js"
        },
        "scopes": {
          "https://ga.jspm.io/": {
            "@datastructures-js/queue": "https://ga.jspm.io/npm:@datastructures-js/queue@4.2.3/index.js",
            "@penrose/optimizer": "https://ga.jspm.io/npm:@penrose/optimizer@3.0.0-alpha.1/index.js",
            "consola": "https://ga.jspm.io/npm:consola@2.15.3/dist/consola.browser.js",
            "crypto": "https://ga.jspm.io/npm:@jspm/core@2.0.1/nodelibs/browser/crypto.js",
            "immutable": "https://ga.jspm.io/npm:immutable@4.3.1/dist/immutable.es.js",
            "lodash": "https://ga.jspm.io/npm:lodash@4.17.21/lodash.js",
            "mathjax-full/js/": "https://ga.jspm.io/npm:mathjax-full@3.2.2/js/",
            "mhchemparser/dist/mhchemParser.js": "https://ga.jspm.io/npm:mhchemparser@4.2.1/dist/mhchemParser.js",
            "moo": "https://ga.jspm.io/npm:moo@0.5.2/moo.js",
            "nearley": "https://ga.jspm.io/npm:nearley@2.20.1/lib/nearley.js",
            "poly-partition": "https://ga.jspm.io/npm:poly-partition@1.0.2/lib/index.js",
            "seedrandom": "https://ga.jspm.io/npm:seedrandom@3.0.5/index.js",
            "true-myth": "https://ga.jspm.io/npm:true-myth@4.1.1/dist/cjs/index.js"
          }
        }
      }
    </script>

    <!-- ES Module Shims: Import maps polyfill for olrder browsers without import maps support (eg Safari 16.3) -->
    <script
      async
      src="https://ga.jspm.io/npm:es-module-shims@1.7.3/dist/es-module-shims.js"
      crossorigin="anonymous"
    ></script>

    <script type="module">
      import {
        compileTrio,
        prepareState,
        stepUntilConvergence,
        RenderStatic,
        showError,
      } from "@penrose/core";
      const trio = {
        substance: `
          Set A
          Label A $e=mc^2$
        `,
        style: `canvas {
          width = 150
          height = 150
        }
        forall Set A {
          center = (0, 0)
          Circle { 
            center: center
            r: 50
         }
          Equation { 
            center: center
            string: A.label
          }
        }
        `,
        domain: `type Set`,
        variation: `test`,
      };
      const compiled = await compileTrio(trio);
      if (compiled.isErr()) console.error(showError(compiled.error));
      const prepared = await prepareState(compiled.value);
      const optimized = stepUntilConvergence(prepared);
      if (optimized.isErr()) console.error(showError(optimized.error));
      document
        .getElementById("penrose")
        .appendChild(await RenderStatic(optimized.value));
    </script>
    <div id="penrose"></div>
  </body>
</html>
```

To run this example, copy the code above into an HTML file (e.g. `index.html`) and run an local HTTP server to view the page:

```shell
npx http-server .
```

You can also check out this example live [here](pathname:///vanilla-js-demo.html).

<!--@include: pathname:///vanilla-js-demo.html-->

[ECMAScript module]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules
[JSPM]: https://jspm.org/
[JSPM Generator]: https://generator.jspm.io/
