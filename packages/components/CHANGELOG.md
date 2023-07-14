# Change Log

All notable changes to this project will be documented in this file.
See [Conventional Commits](https://conventionalcommits.org) for commit guidelines.

## [v3.0.0-beta.0](https://github.com/penrose/penrose/compare/v2.3.0...v3.0.0-beta.0) (2023-07-14)

### :warning: BREAKING CHANGE

- clean up `core` exports and synthesizer modules (#1367)
- combine `automator` and `roger` (#1387)
- consolidate shape types (#1337)
- more readable `core` language API (#1527)
- put each trio in its own JSON file (#1393)

### :rocket: New Feature

- Substance as spreadsheet for timeline diagrams ([#1419](https://github.com/penrose/penrose/issues/1419)) ([8c54f24](https://github.com/penrose/penrose/commit/8c54f24dfac463396422cd9837e2a55ee57f8787))
- Substance variable collection ([#1390](https://github.com/penrose/penrose/issues/1390)) ([c2d2467](https://github.com/penrose/penrose/commit/c2d2467a8f094695e0786a4464e556bfc2bda1a8))
- add multiple choice component to `synthesizer-ui` ([#1344](https://github.com/penrose/penrose/issues/1344)) ([8767e36](https://github.com/penrose/penrose/commit/8767e360ba8cf92210b1177ba2f07586d945d613))
- additional constructor declaration syntax in Domain and Substance ([#1472](https://github.com/penrose/penrose/issues/1472)) ([ff1a7d7](https://github.com/penrose/penrose/commit/ff1a7d70a643d0cc19d4f14c7b962207f28c46b7))
- cropping picks smaller viewbox in gallery and `editor` ([#1518](https://github.com/penrose/penrose/issues/1518)) ([b59854a](https://github.com/penrose/penrose/commit/b59854a69bd40e0d543fa16d6875150fb57207a9))
- error and warning markings in IDE editor ([#1513](https://github.com/penrose/penrose/issues/1513)) ([c91a6c9](https://github.com/penrose/penrose/commit/c91a6c9794cc26a2e9134fe60e02f5d33e572a5b))
- function warnings ([#1498](https://github.com/penrose/penrose/issues/1498)) ([3e68a94](https://github.com/penrose/penrose/commit/3e68a94ab933a841d882ea26c26a58b5728629bf))
- homepage gallery component ([#1464](https://github.com/penrose/penrose/issues/1464)) ([130b2b7](https://github.com/penrose/penrose/commit/130b2b77904b926863c7348b7de085a792117ca9))
- updated examples visible in gallery ([#1505](https://github.com/penrose/penrose/issues/1505)) ([230d534](https://github.com/penrose/penrose/commit/230d5344502a1b6506f03614f8b74c2035f50f71))

### :bug: Bug Fix

- remove width and height attributes in `editor` SVG export ([#1410](https://github.com/penrose/penrose/issues/1410)) ([3f68541](https://github.com/penrose/penrose/commit/3f685410b3543e59c77fd8c88893acb18ddfec2d))
- repeated state updates in `Gridbox` of `components` ([#1430](https://github.com/penrose/penrose/issues/1430)) ([7c5657c](https://github.com/penrose/penrose/commit/7c5657ccdb7a29a15c9800a4a57fe6e13ca60075))

### :nail_care: Polish

- bump TypeScript version to 5.0 ([#1395](https://github.com/penrose/penrose/issues/1395)) ([b4ae329](https://github.com/penrose/penrose/commit/b4ae3298c9a03926ca690c63f368adcaa031b56d))
- clean up `core` exports and synthesizer modules ([#1367](https://github.com/penrose/penrose/issues/1367)) ([cf24aaa](https://github.com/penrose/penrose/commit/cf24aaad28c3589d5770e75669f3e6e66d19d2aa))
- combine `automator` and `roger` ([#1387](https://github.com/penrose/penrose/issues/1387)) ([678c6e5](https://github.com/penrose/penrose/commit/678c6e528d20d6cbbfd3a04f1fcad656e72bdc6e))
- consolidate shape types ([#1337](https://github.com/penrose/penrose/issues/1337)) ([0d69c97](https://github.com/penrose/penrose/commit/0d69c9709d68f4dd4f8cc6a7773740fa6f872ccf))
- more readable `core` language API ([#1527](https://github.com/penrose/penrose/issues/1527)) ([22c8fc6](https://github.com/penrose/penrose/commit/22c8fc68f225974a353df244832b3b1c90e5f0e0))
- pull out base `tsconfig.json` ([#1392](https://github.com/penrose/penrose/issues/1392)) ([e6c5f55](https://github.com/penrose/penrose/commit/e6c5f5524837fe4c970713f05bbed821b9cda411))
- put each trio in its own JSON file ([#1393](https://github.com/penrose/penrose/issues/1393)) ([803d7fc](https://github.com/penrose/penrose/commit/803d7fc20199262f833c2f60606ed1b778c92b72))

### :memo: Documentation

- homepage tweaks ([#1515](https://github.com/penrose/penrose/issues/1515)) ([0fa290e](https://github.com/penrose/penrose/commit/0fa290e3264c8c20c768a9ef509a768ee10d5030))
- start a blog ([#1325](https://github.com/penrose/penrose/issues/1325)) ([6669567](https://github.com/penrose/penrose/commit/6669567917464c72d5dd445a6def540b0d11da93))
- using Penrose programmatically ([#1525](https://github.com/penrose/penrose/issues/1525)) ([7952b2b](https://github.com/penrose/penrose/commit/7952b2baa81fd4e5631135b707703cbc07646380))

### :house: Internal

- switch from jest to vitest ([#1406](https://github.com/penrose/penrose/issues/1406)) ([8ef8c77](https://github.com/penrose/penrose/commit/8ef8c778488b17eb0f02a62d1399e0b0337f5355))

## [v2.3.0](https://github.com/penrose/penrose/compare/v2.2.0...v2.3.0) (2023-03-14)

### :rocket: New Feature

- added "ctrl+enter" binding for recompiling ([#1306](https://github.com/penrose/penrose/issues/1306)) ([d0472d1](https://github.com/penrose/penrose/commit/d0472d1306959f6244ebb64a64894cc0a3fd376b))
- show multiple diagram instances on a grid in `editor` ([#1287](https://github.com/penrose/penrose/issues/1287)) ([fbaf03c](https://github.com/penrose/penrose/commit/fbaf03c7b6c4f87cc628111ee080af76c65ef55e))

### :bug: Bug Fix

- SVG overflow in `Simple` component ([#1321](https://github.com/penrose/penrose/issues/1321)) ([df119ac](https://github.com/penrose/penrose/commit/df119acad87250d0097eeda4f019238bf0d07743))
- nondeterminism in renderer ([#1316](https://github.com/penrose/penrose/issues/1316)) ([9795420](https://github.com/penrose/penrose/commit/97954202c60c2aab6a11af1694f652f8a3bb8e4d))

### :house: Internal

- expand presets in `synthesizer-ui` ([#1149](https://github.com/penrose/penrose/issues/1149)) ([58c288a](https://github.com/penrose/penrose/commit/58c288a2ec5b124f008222e8c3807dfa550dcd6f))

## [v2.2.0](https://github.com/penrose/penrose/compare/v2.1.1...v2.2.0) (2023-02-02)

### :rocket: New Feature

- improve registry schema and loading ([#1212](https://github.com/penrose/penrose/issues/1212)) ([d6bbc30](https://github.com/penrose/penrose/commit/d6bbc302de494e08fa4ca0602ccfa29bdfcd65ae))
- support longer file extensions ([#1280](https://github.com/penrose/penrose/issues/1280)) ([6e83596](https://github.com/penrose/penrose/commit/6e835968280a784a91c4a2ca47a226516a3067d0))

### :memo: Documentation

- fix `import` typo in components README ([#1253](https://github.com/penrose/penrose/issues/1253)) ([05b1f68](https://github.com/penrose/penrose/commit/05b1f68a80a3fc36868e226038f450ae8cd65cf5))

## [v2.1.1](https://github.com/penrose/penrose/compare/v2.1.0...v2.1.1) (2023-01-19)

**Note:** Version bump only for package @penrose/components

# [2.1.0](https://github.com/penrose/penrose/compare/v2.0.0...v2.1.0) (2023-01-19)

**Note:** Version bump only for package @penrose/components

# [2.0.0](https://github.com/penrose/penrose/compare/v1.3.0...v2.0.0) (2023-01-17)

### Bug Fixes

- `ReferenceError` in storybook ([#1228](https://github.com/penrose/penrose/issues/1228)) ([74a0440](https://github.com/penrose/penrose/commit/74a0440be73053a8fb1105b810308254b2957663))
- circleci resource class and storybook trigger ([50e808b](https://github.com/penrose/penrose/commit/50e808b0e7c77dfbb5f6e61c71eb0a1c9636aadd))
- delete `svg-flatten` ([#1208](https://github.com/penrose/penrose/issues/1208)) ([976ca77](https://github.com/penrose/penrose/commit/976ca770c8eae9b95d2d6f7b36937005bbac8bcf))
- storybook deployment base directory ([#1124](https://github.com/penrose/penrose/issues/1124)) ([7fb7a01](https://github.com/penrose/penrose/commit/7fb7a013d01a9fc1d40310b2a8505f99d7ea9468))
- storybook examples and add exterior algebra to registry ([#1122](https://github.com/penrose/penrose/issues/1122)) ([261055e](https://github.com/penrose/penrose/commit/261055edcd31939fa62db97928b86868b9a5e656))
- update examples in storybook ([#790](https://github.com/penrose/penrose/issues/790)) ([3c968b9](https://github.com/penrose/penrose/commit/3c968b90e77017c55fa43db02bd3a3dd874cb047))

### Features

- basic symmetric predicates ([#1061](https://github.com/penrose/penrose/issues/1061)) ([80e0a61](https://github.com/penrose/penrose/commit/80e0a611951cec828dbec5f00b56795a34ddfe26))
- compile on vim write (resolves [#1046](https://github.com/penrose/penrose/issues/1046)) ([#1197](https://github.com/penrose/penrose/issues/1197)) ([9ee17a6](https://github.com/penrose/penrose/commit/9ee17a62bbc78b73caa4c093ed4131359af588d5))
- display errors in the `Simple` component ([#953](https://github.com/penrose/penrose/issues/953)) ([aa6209f](https://github.com/penrose/penrose/commit/aa6209f520b6a2cbdcfe1b80767b233d27d69867)), closes [#535](https://github.com/penrose/penrose/issues/535)
- Editor Rewrite ([#992](https://github.com/penrose/penrose/issues/992)) ([91022fa](https://github.com/penrose/penrose/commit/91022fafdd45e6e5810bcb87448095a1d105bae5))
- hexadecimal color literals in Style ([#1114](https://github.com/penrose/penrose/issues/1114)) ([ce4cb51](https://github.com/penrose/penrose/commit/ce4cb51cdb22b67d01766bd744073f191cc0a262))
- Make Penrose deterministic ([#864](https://github.com/penrose/penrose/issues/864)) ([baabbe6](https://github.com/penrose/penrose/commit/baabbe63cfee662eb1f97a0782ca3a1d609af4cd))
- preset loading in `synthesizer-ui` ([#1133](https://github.com/penrose/penrose/issues/1133)) ([7d0d7d8](https://github.com/penrose/penrose/commit/7d0d7d873df48ce82536f396d193c0cc45a51ff9))
- put the demo in the VitePress site ([#1193](https://github.com/penrose/penrose/issues/1193)) ([22fba15](https://github.com/penrose/penrose/commit/22fba1567426fa425564e9dcd514b69ca69622e6))
- React component library ([#671](https://github.com/penrose/penrose/issues/671)) ([7f5977b](https://github.com/penrose/penrose/commit/7f5977b9c578b0a47c0d7b3643426d62319c93d7))
- resolve paths for included SVGs ([#825](https://github.com/penrose/penrose/issues/825)) ([cedbf1b](https://github.com/penrose/penrose/commit/cedbf1b0f219f013a0c825e08007a2edc3b2c3bc))
- Style inline color widgets ([#1094](https://github.com/penrose/penrose/issues/1094)) ([3ffdbbe](https://github.com/penrose/penrose/commit/3ffdbbe8aef0a2588962bf242de85d39fa4792c7))
- support layout stages in Style ([#1199](https://github.com/penrose/penrose/issues/1199)) ([d22602a](https://github.com/penrose/penrose/commit/d22602a7f31ce48c0c00a984efec5fa3622e63eb))
- unify browser-ui and editor ([#1000](https://github.com/penrose/penrose/issues/1000)) ([3e7f647](https://github.com/penrose/penrose/commit/3e7f64729fb36ba7c735f0360dcc4f33fd04a49c))

### Performance Improvements

- port the optimizer to WebAssembly ([#1092](https://github.com/penrose/penrose/issues/1092)) ([768895a](https://github.com/penrose/penrose/commit/768895a3aac643095f0d139052fa8a139ce28cfb))
