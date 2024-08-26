# Getting Started

---

Welcome! We’re excited you’ve decided to try Bloom. These series of tutorials will walk you through everything you need
to start making beautiful interactive diagrams directly in React. This first chapter will lead you through creating a
React app and installing Bloom. If you already have a React app set up, just install the npm package `@penrose/bloom` with
your favorite package manager and move on to the next tutorial.

These tutorials expect that you have some proficiency with JavaScript and React, plus a teeny bit of linear algebra,
though we’ll keep that to a minimum. They _do not_ assume you have any experience with Penrose, though if you do,
you’ll likely find the concepts familiar.

### Creating a React App

You'll need Node.js $\ge$ 18 and NPM $ge$ 8 before getting started, which you can find
[here](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm). Next, create new React app with the following command:

```bash
npm create vite@latest my-bloom-app -- --template react-ts
cd my-bloom-app
npm install
```

You should obviously change `my-bloom-app` to whatever makes sense for you. Finally, you can install the Bloom package
and its dependencies. We _highly_ recommend using TypeScript with Bloom, which is why we used the `react-ts`
template above.

```bash
npm install @penrose/bloom
```

### Creating a component

At this point you'll probably want to remove all of the template nonsense from your directory:

(please make sure you're in the root of your project directory before running this command!)

```bash
rm -rf public/ src/*.css src/App.tsx src/assets/
```

Now create a new `App.tsx` file in the `src/` directory with the following content:

```typescript
import { Renderer } from "@penrose/bloom";

const App = () => {
  return <Renderer diagram={null} />;
};

export default App;
```

If everything is set up correctly, running `npm run dev` should now render, well, nothing. In the next chapter,
we'll start building our first diagram. From here, we'll assume some proficiency with React, so if you need a refresher
you check out their excellent tutorial [here](https://react.dev/learn).
