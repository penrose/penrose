# Change Log

All notable changes to this project will be documented in this file.
See [Conventional Commits](https://conventionalcommits.org) for commit guidelines.

## [v4.0.0-alpha.1] (2024-07-11)

### :warning: BREAKING CHANGE

- separate Substance environment and remove unused features (#1677)

### :rocket: New Feature

- codemirror migration ([#1798](https://github.com/penrose/penrose/issues/1798)) ([59d77f2](https://github.com/penrose/penrose/commit/59d77f22822cc28a32daf812f0b8c4b9e791786c))
- initial values for unknown variables ([#1638](https://github.com/penrose/penrose/issues/1638)) ([61f2ad7](https://github.com/penrose/penrose/commit/61f2ad766efc471bf552a6cfc3d9fcf9c2f3779a))
- substance literal values ([#1682](https://github.com/penrose/penrose/issues/1682)) ([7c1978f](https://github.com/penrose/penrose/commit/7c1978f4e33498828d6893d7d8f9257d2f1f839b))

### :bug: Bug Fix

- UI bugs in `edgeworth` ([#1706](https://github.com/penrose/penrose/issues/1706)) ([c255e39](https://github.com/penrose/penrose/commit/c255e39176c118662f91108ac9ab59682966acc6))
- separate Substance environment and remove unused features ([#1677](https://github.com/penrose/penrose/issues/1677)) ([7ad14e7](https://github.com/penrose/penrose/commit/7ad14e7d819d1ddedc03a75eb17f17532430b3aa))

### :nail_care: Polish

- simplified shared `Grid` and custom `Grid` in Edgeworth ([#1729](https://github.com/penrose/penrose/issues/1729)) ([a7c9d4d](https://github.com/penrose/penrose/commit/a7c9d4d5d8cc44b3544afb826da3a7002ef55cff))

### :house: Internal

- bump version to 4.0.0-alpha.0 ([#1735](https://github.com/penrose/penrose/issues/1735)) ([da1343d](https://github.com/penrose/penrose/commit/da1343dca34cb1291894b754b7f7ac3aed672df5))
- simplify names of `IsSubset` and `NotIntersecting` ([#1724](https://github.com/penrose/penrose/issues/1724)) ([b5e8cb8](https://github.com/penrose/penrose/commit/b5e8cb8675c63f279e9e769913b24c1ec8055b23))

## [v4.0.0-alpha.0] (2024-05-07)

### :warning: BREAKING CHANGE

- separate Substance environment and remove unused features (#1677)

### :rocket: New Feature

- initial values for unknown variables ([#1638](https://github.com/penrose/penrose/issues/1638)) ([61f2ad7](https://github.com/penrose/penrose/commit/61f2ad766efc471bf552a6cfc3d9fcf9c2f3779a))
- substance literal values ([#1682](https://github.com/penrose/penrose/issues/1682)) ([7c1978f](https://github.com/penrose/penrose/commit/7c1978f4e33498828d6893d7d8f9257d2f1f839b))

### :bug: Bug Fix

- UI bugs in `edgeworth` ([#1706](https://github.com/penrose/penrose/issues/1706)) ([c255e39](https://github.com/penrose/penrose/commit/c255e39176c118662f91108ac9ab59682966acc6))
- separate Substance environment and remove unused features ([#1677](https://github.com/penrose/penrose/issues/1677)) ([7ad14e7](https://github.com/penrose/penrose/commit/7ad14e7d819d1ddedc03a75eb17f17532430b3aa))

### :nail_care: Polish

- simplified shared `Grid` and custom `Grid` in Edgeworth ([#1729](https://github.com/penrose/penrose/issues/1729)) ([a7c9d4d](https://github.com/penrose/penrose/commit/a7c9d4d5d8cc44b3544afb826da3a7002ef55cff))

### :house: Internal

- simplify names of `IsSubset` and `NotIntersecting` ([#1724](https://github.com/penrose/penrose/issues/1724)) ([b5e8cb8](https://github.com/penrose/penrose/commit/b5e8cb8675c63f279e9e769913b24c1ec8055b23))

## [v3.2.0](https://github.com/penrose/penrose/compare/v3.1.0...v3.2.0) (2023-08-08)

### :rocket: New Feature

- Substance indexed sets ([#1572](https://github.com/penrose/penrose/issues/1572)) ([83f3869](https://github.com/penrose/penrose/commit/83f386950415a0d839ce95ba080c9ed36c4413b7))

## [v3.1.0](https://github.com/penrose/penrose/compare/v3.0.0...v3.1.0) (2023-07-19)

### :rocket: New Feature

- added UI for LLM program generation in `edgeworth` ([#1556](https://github.com/penrose/penrose/issues/1556)) ([865ee9e](https://github.com/penrose/penrose/commit/865ee9e7f6b598bc0f07e416ab73bea978bba6e7))

## [v3.0.0](https://github.com/penrose/penrose/compare/v2.3.0...v3.0.0) (2023-07-14)

### :warning: BREAKING CHANGE

- clean up `core` exports and synthesizer modules (#1367)
- combine `automator` and `roger` (#1387)
- more readable `core` language API (#1527)
- put each trio in its own JSON file (#1393)

### :rocket: New Feature

- deduplication of mutated Substance programs in `edgeworth` ([#1481](https://github.com/penrose/penrose/issues/1481)) ([9eec6d1](https://github.com/penrose/penrose/commit/9eec6d1c51f1b26557c91bbd5b9e87f4f068dfdb))

### :nail_care: Polish

- bump TypeScript version to 5.0 ([#1395](https://github.com/penrose/penrose/issues/1395)) ([b4ae329](https://github.com/penrose/penrose/commit/b4ae3298c9a03926ca690c63f368adcaa031b56d))
- clean up `core` exports and synthesizer modules ([#1367](https://github.com/penrose/penrose/issues/1367)) ([cf24aaa](https://github.com/penrose/penrose/commit/cf24aaad28c3589d5770e75669f3e6e66d19d2aa))
- combine `automator` and `roger` ([#1387](https://github.com/penrose/penrose/issues/1387)) ([678c6e5](https://github.com/penrose/penrose/commit/678c6e528d20d6cbbfd3a04f1fcad656e72bdc6e))
- more readable `core` language API ([#1527](https://github.com/penrose/penrose/issues/1527)) ([22c8fc6](https://github.com/penrose/penrose/commit/22c8fc68f225974a353df244832b3b1c90e5f0e0))
- pull out base `tsconfig.json` ([#1392](https://github.com/penrose/penrose/issues/1392)) ([e6c5f55](https://github.com/penrose/penrose/commit/e6c5f5524837fe4c970713f05bbed821b9cda411))
- put each trio in its own JSON file ([#1393](https://github.com/penrose/penrose/issues/1393)) ([803d7fc](https://github.com/penrose/penrose/commit/803d7fc20199262f833c2f60606ed1b778c92b72))

### :house: Internal

- bump version to 3.0.0-beta.0 ([#1542](https://github.com/penrose/penrose/issues/1542)) ([ef4fffb](https://github.com/penrose/penrose/commit/ef4fffbf22e03fdd3af84c439163ff24bc5ccb41))
- bump version to 3.0.0-beta.1 ([#1543](https://github.com/penrose/penrose/issues/1543)) ([abe43d9](https://github.com/penrose/penrose/commit/abe43d9be98a719204b54cbf3abf4bbec9367d16))
- switch from jest to vitest ([#1406](https://github.com/penrose/penrose/issues/1406)) ([8ef8c77](https://github.com/penrose/penrose/commit/8ef8c778488b17eb0f02a62d1399e0b0337f5355))

## [v2.3.0](https://github.com/penrose/penrose/compare/v2.2.0...v2.3.0) (2023-03-14)

### :rocket: New Feature

- show multiple diagram instances on a grid in `editor` ([#1287](https://github.com/penrose/penrose/issues/1287)) ([fbaf03c](https://github.com/penrose/penrose/commit/fbaf03c7b6c4f87cc628111ee080af76c65ef55e))

### :bug: Bug Fix

- nondeterminism in renderer ([#1316](https://github.com/penrose/penrose/issues/1316)) ([9795420](https://github.com/penrose/penrose/commit/97954202c60c2aab6a11af1694f652f8a3bb8e4d))

### :house: Internal

- add Lewis structures examples to `synthesizer-ui` ([#1334](https://github.com/penrose/penrose/issues/1334)) ([2f1f624](https://github.com/penrose/penrose/commit/2f1f624118d2c48433c6f888075c8b279ff3e387))
- add graph examples to `synthesizer-ui` ([#1336](https://github.com/penrose/penrose/issues/1336)) ([3b5f964](https://github.com/penrose/penrose/commit/3b5f964f2d9ca0d619ce7291844c36dd181e4345))
- expand presets in `synthesizer-ui` ([#1149](https://github.com/penrose/penrose/issues/1149)) ([58c288a](https://github.com/penrose/penrose/commit/58c288a2ec5b124f008222e8c3807dfa550dcd6f))

## [v2.2.0](https://github.com/penrose/penrose/compare/v2.1.1...v2.2.0) (2023-02-02)

### :rocket: New Feature

- improve registry schema and loading ([#1212](https://github.com/penrose/penrose/issues/1212)) ([d6bbc30](https://github.com/penrose/penrose/commit/d6bbc302de494e08fa4ca0602ccfa29bdfcd65ae))
- support longer file extensions ([#1280](https://github.com/penrose/penrose/issues/1280)) ([6e83596](https://github.com/penrose/penrose/commit/6e835968280a784a91c4a2ca47a226516a3067d0))

## [v2.1.1](https://github.com/penrose/penrose/compare/v2.1.0...v2.1.1) (2023-01-19)

**Note:** Version bump only for package @penrose/synthesizer-ui

# [2.1.0](https://github.com/penrose/penrose/compare/v2.0.0...v2.1.0) (2023-01-19)

**Note:** Version bump only for package @penrose/synthesizer-ui

# [2.0.0](https://github.com/penrose/penrose/compare/v1.3.0...v2.0.0) (2023-01-17)

### Bug Fixes

- delete `svg-flatten` ([#1208](https://github.com/penrose/penrose/issues/1208)) ([976ca77](https://github.com/penrose/penrose/commit/976ca770c8eae9b95d2d6f7b36937005bbac8bcf))
- Set paint none=none() in style examples. Closes [#703](https://github.com/penrose/penrose/issues/703) ([#705](https://github.com/penrose/penrose/issues/705)) ([f631d57](https://github.com/penrose/penrose/commit/f631d57f5b3ea7250600593778eaa89158e7a98c)), closes [#651](https://github.com/penrose/penrose/issues/651) [#374](https://github.com/penrose/penrose/issues/374) [#392](https://github.com/penrose/penrose/issues/392) [#392](https://github.com/penrose/penrose/issues/392) [#392](https://github.com/penrose/penrose/issues/392) [#392](https://github.com/penrose/penrose/issues/392) [#392](https://github.com/penrose/penrose/issues/392)
- synthesizer-ui typescript build warnings ([#846](https://github.com/penrose/penrose/issues/846)) ([2b00441](https://github.com/penrose/penrose/commit/2b0044109982ebc1354bfa050e747c8ceeb62931))

### Features

- add browser for synthesizer ([#640](https://github.com/penrose/penrose/issues/640)) ([2d81a55](https://github.com/penrose/penrose/commit/2d81a55d0175c12195dc510f0514c741eb0f7803))
- display errors in the `Simple` component ([#953](https://github.com/penrose/penrose/issues/953)) ([aa6209f](https://github.com/penrose/penrose/commit/aa6209f520b6a2cbdcfe1b80767b233d27d69867)), closes [#535](https://github.com/penrose/penrose/issues/535)
- docusaurus site ([#771](https://github.com/penrose/penrose/issues/771)) ([13396b2](https://github.com/penrose/penrose/commit/13396b298280f63a9161174bf6b585a20613334c))
- enumerative search of Substance mutations ([#638](https://github.com/penrose/penrose/issues/638)) ([97db076](https://github.com/penrose/penrose/commit/97db07673c16970216d56ec8af360639351361da))
- Make Penrose deterministic ([#864](https://github.com/penrose/penrose/issues/864)) ([baabbe6](https://github.com/penrose/penrose/commit/baabbe63cfee662eb1f97a0782ca3a1d609af4cd))
- preset loading in `synthesizer-ui` ([#1133](https://github.com/penrose/penrose/issues/1133)) ([7d0d7d8](https://github.com/penrose/penrose/commit/7d0d7d873df48ce82536f396d193c0cc45a51ff9))
- resolve paths for included SVGs ([#825](https://github.com/penrose/penrose/issues/825)) ([cedbf1b](https://github.com/penrose/penrose/commit/cedbf1b0f219f013a0c825e08007a2edc3b2c3bc))
- unify browser-ui and editor ([#1000](https://github.com/penrose/penrose/issues/1000)) ([3e7f647](https://github.com/penrose/penrose/commit/3e7f64729fb36ba7c735f0360dcc4f33fd04a49c))
