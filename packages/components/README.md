# `@penrose/components`: a React component library for Penrose

`@penrose/components` includes a collection of React components for building Penrose clients (e.g. `@penrose/editor` and `@penrose/synthesizer-ui`).

## Getting started

In your application, make sure you have `react` and `react-dom` as dependencies. `@penrose/components` has both packages as peer dependencies and does not bundle them. To install:

```shell
yarn add @penrose/components
```

For a minimal example, try using the `Simple` component in your React application:

```ts
import { Embed } from "@penrose/components";

const domain = `
type Set
`;

const substance = `
Set A
AutoLabel All
`;

const style = `
canvas {
  width = 500
  height = 500
}
forall Set X {
  X.shape = Circle { }
  X.text  = Text { string: X.label }
  ensure contains(X.shape, X.text)
  ensure maxSize(X.shape, canvas.width / 2)
}
`;

const App = () => (
  <Embed domain={domain} substance={substance} style={style} variation={""} />
);

export default App;
```

For a full list of exported components and examples, refer to the [storybook](https://penrose.github.io/penrose/storybook/).
