<script setup lang="ts">
  import {data } from "./vanilla-demo.data.js"
</script>

# Using Penrose with Vanilla JS

Both the [Language API](./api) and [Optimization API](./optimization-api) are exported from `@penrose/core`, currently released as an [ECMAScript module] (ESM). If you are making a web page without a build tool, you can use one of the CDNs with built-in ESM support. Here's an example of using [JSPM], where we use the [JSPM Generator] to create the HTML boilerplate for importing our dependencies.

```html-vue
{{ data }}
```

To run this example, copy the code above into an HTML file (e.g. `index.html`) and run an local HTTP server to view the page:

```shell
npx http-server .
```

You can also check out this example live [here](pathname:///vanilla-js-demo.html).

## Experimental bundled ESM

::: info
This feature is experimental as of `v3.2.0` and is subject to changes.
:::

The default ESM module requires a CDN or a bundler to download all the dependencies of `core`. This experimental release format is an ESM module that includes all dependencies in one ESM module. To import `@penrose/core` from the bundle:

```ts
import { compile, optimize } from "@penrose/core/bundle";
```

::: danger
`@penrose/optimizer` uses [top-level `await`] and the bundled `core` module currently use [IIFE] around it to provide a seemingly synchronous API. However, be aware that any function imported from `@penrose/core/bundle` might be `undefined` when used immediately after `import`ing. To work around this, do explicit checks on whether the functions are loaded before using them:

```ts
import { compile, optimize } from "@penrose/core/bundle";
if (compile && optimize) {
  // actually call `compile` and `optimize`
}
```

:::

<!--@include: pathname:///vanilla-js-demo.html-->

[top-level `await`]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/await#top_level_await
[ECMAScript module]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules
[JSPM]: https://jspm.org/
[JSPM Generator]: https://generator.jspm.io/
[IIFE]: https://developer.mozilla.org/en-US/docs/Glossary/IIFE
