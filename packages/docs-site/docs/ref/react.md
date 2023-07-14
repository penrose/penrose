# Using Penrose with React

[`@penrose/components`](https://www.npmjs.com/package/@penrose/components) is a React component library for Penrose. To install:

::: code-group

```shell [Npm]
npm i @penrose/components
```

```shell [Yarn]
yarn add @penrose/components
```

:::

Also, make sure you have `react` and `react-dom` as dependencies.
`@penrose/components` has both packages as peer dependencies and does not bundle
them:

::: code-group

```shell [Npm]
npm i react react-dom
```

```shell [Yarn]
yarn add react react-dom
```

:::

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
}
`;

const App = () => (
  <Embed domain={domain} substance={substance} style={style} variation={""} />
);

export default App;
```

For a full list of exported components and examples, refer to the [storybook](https://penrose.github.io/penrose/storybook/).
