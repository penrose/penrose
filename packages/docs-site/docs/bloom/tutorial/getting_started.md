# Getting Started

---

Welcome! We’re excited you’ve decided to try Bloom. This series of tutorials will walk you through everything you need
to start building optimization-driven interactive diagrams in JavaScript.

### First Steps

For the purposes of the tutorial, let's start a new HTML/JS project. Create a new directory and add both an `index.html`
and `main.js` file:

```bash
mkdir my-bloom-project
cd my-bloom-project
touch index.html main.js
```

Next, open `index.html` in your favorite text editor and add the standard boilerplate:

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>My Bloom Project</title>
  </head>
  <body>
    <script type="module" src="main.js"></script>
  </body>
</html>
```

It's important that you include the `type="module"` attribute in the script tag. This tells the browser to treat the
JavaScript file as an ES6 module (support for which is now widespread), which is necessary to import Bloom.

Now open `main.js` and add import Bloom:

```javascript
import * as bloom from "https://penrose.cs.cmu.edu/bloom.min.js";
```

You may prefer instead to download the Bloom library and serve it locally. Let's finish by building a blank
diagram:

```javascript
import * as bloom from "https://penrose.cs.cmu.edu/bloom.min.js";

const db = new bloom.DiagramBuilder(bloom.canvas(400, 400), "abcd", 1);

// Diagramming goes here!

const diagram = await db.build();
```

In the next chapter, you'll learn how to create a diagram with `DiagramBuilder`, and display it in the browser.

### NPM Package

If you're bundling your project with Webpack, Rollup, or developing with Vite, you can also install Bloom as an NPM
package:

```bash
npm install @penrose/bloom
```

The NPM package also includes native TypeScript and React support.
