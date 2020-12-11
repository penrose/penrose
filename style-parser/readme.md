A simple demo of how to use nearley with typescript.

```sh
git clone --depth=1 https://github.com/cmcaine/nearley-ts-demo.git
npm install
npm test
```

## For your own project

Quickstart for your own project:

```
npm install Hardmath123/nearley typescript @types/nearley
```

tsc needs the `es6` or higher lib and to use the `@types` dir:

```js
// tsconfig.json
{
  "compilerOptions": {
    // Required for Map
    "lib": ["es6"],
    
    // Required for @types/nearley
    "typeRoots": ["node_modules/@types"],
  },
}
```

Your grammars need the `@preprocessor typescript` directive and you should import them like so:


```ts
import * as nearley from 'nearley'
import * as bracketexpr_grammar from './bracketexpr'
```
