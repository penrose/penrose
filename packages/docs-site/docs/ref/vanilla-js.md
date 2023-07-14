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

<!--@include: pathname:///vanilla-js-demo.html-->

[ECMAScript module]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules
[JSPM]: https://jspm.org/
[JSPM Generator]: https://generator.jspm.io/
