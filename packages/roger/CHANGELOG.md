# Change Log

All notable changes to this project will be documented in this file.
See [Conventional Commits](https://conventionalcommits.org) for commit guidelines.

## [v3.1.0](https://github.com/penrose/penrose/compare/v3.0.0...v3.1.0) (2023-07-19)

### :bug: Bug Fix

- compile `@penrose/roger` for distribution 💂‍♂️ ([#1562](https://github.com/penrose/penrose/issues/1562)) ([7b36125](https://github.com/penrose/penrose/commit/7b3612539bb104ea16edf01ad4884a919c3eac4a))

## [v3.0.0](https://github.com/penrose/penrose/compare/v2.3.0...v3.0.0) (2023-07-14)

### :warning: BREAKING CHANGE

- combine `automator` and `roger` (#1387)
- more readable `core` language API (#1527)
- put each trio in its own JSON file (#1393)
- remove old file extensions from Roger (#1499)
- support non-trio examples in the registry (#1418)

### :rocket: New Feature

- add command to render multiple trios in `roger` ([#1471](https://github.com/penrose/penrose/issues/1471)) ([9c052fe](https://github.com/penrose/penrose/commit/9c052feef479aa644b678fcf432721a5c2538845))
- export SVGs with plain TeX labels ([#1433](https://github.com/penrose/penrose/issues/1433)) ([3e589a0](https://github.com/penrose/penrose/commit/3e589a08fc1c5b46d4e9143655722058970219dc))
- function warnings ([#1498](https://github.com/penrose/penrose/issues/1498)) ([3e68a94](https://github.com/penrose/penrose/commit/3e68a94ab933a841d882ea26c26a58b5728629bf))
- remove old file extensions from Roger ([#1499](https://github.com/penrose/penrose/issues/1499)) ([744aa0a](https://github.com/penrose/penrose/commit/744aa0aca3c9175879711d1ad8d1f8199cf1028c))
- support non-trio examples in the registry ([#1418](https://github.com/penrose/penrose/issues/1418)) ([4fe953f](https://github.com/penrose/penrose/commit/4fe953fb26778e28debae7221de3f05ef30c4e2b))
- support trio switching in `roger watch` and `editor` ([#1486](https://github.com/penrose/penrose/issues/1486)) ([8581efc](https://github.com/penrose/penrose/commit/8581efc9cac3211ca6732e1d7f1d5d542aa4fbc3))

### :bug: Bug Fix

- find the correct working directory for `roger` ([#1434](https://github.com/penrose/penrose/issues/1434)) ([3e1e970](https://github.com/penrose/penrose/commit/3e1e970d6f0dec7020449a93aa369bd49adb4a3c))

### :nail_care: Polish

- combine `automator` and `roger` ([#1387](https://github.com/penrose/penrose/issues/1387)) ([678c6e5](https://github.com/penrose/penrose/commit/678c6e528d20d6cbbfd3a04f1fcad656e72bdc6e))
- more readable `core` language API ([#1527](https://github.com/penrose/penrose/issues/1527)) ([22c8fc6](https://github.com/penrose/penrose/commit/22c8fc68f225974a353df244832b3b1c90e5f0e0))
- put each trio in its own JSON file ([#1393](https://github.com/penrose/penrose/issues/1393)) ([803d7fc](https://github.com/penrose/penrose/commit/803d7fc20199262f833c2f60606ed1b778c92b72))

### :memo: Documentation

- using Penrose programmatically ([#1525](https://github.com/penrose/penrose/issues/1525)) ([7952b2b](https://github.com/penrose/penrose/commit/7952b2baa81fd4e5631135b707703cbc07646380))

### :house: Internal

- bump version to 3.0.0-beta.0 ([#1542](https://github.com/penrose/penrose/issues/1542)) ([ef4fffb](https://github.com/penrose/penrose/commit/ef4fffbf22e03fdd3af84c439163ff24bc5ccb41))
- bump version to 3.0.0-beta.1 ([#1543](https://github.com/penrose/penrose/issues/1543)) ([abe43d9](https://github.com/penrose/penrose/commit/abe43d9be98a719204b54cbf3abf4bbec9367d16))
- switch from jest to vitest ([#1406](https://github.com/penrose/penrose/issues/1406)) ([8ef8c77](https://github.com/penrose/penrose/commit/8ef8c778488b17eb0f02a62d1399e0b0337f5355))

## [v2.3.0](https://github.com/penrose/penrose/compare/v2.2.0...v2.3.0) (2023-03-14)

### :bug: Bug Fix

- nondeterminism in renderer ([#1316](https://github.com/penrose/penrose/issues/1316)) ([9795420](https://github.com/penrose/penrose/commit/97954202c60c2aab6a11af1694f652f8a3bb8e4d))

## [v2.2.0](https://github.com/penrose/penrose/compare/v2.1.1...v2.2.0) (2023-02-02)

### :rocket: New Feature

- support longer file extensions ([#1280](https://github.com/penrose/penrose/issues/1280)) ([6e83596](https://github.com/penrose/penrose/commit/6e835968280a784a91c4a2ca47a226516a3067d0))

## [v2.1.1](https://github.com/penrose/penrose/compare/v2.1.0...v2.1.1) (2023-01-19)

**Note:** Version bump only for package @penrose/roger

# [2.1.0](https://github.com/penrose/penrose/compare/v2.0.0...v2.1.0) (2023-01-19)

**Note:** Version bump only for package @penrose/roger

# [2.0.0](https://github.com/penrose/penrose/compare/v1.3.0...v2.0.0) (2023-01-17)

### Bug Fixes

- `draw` command in `@penrose/roger` ([#937](https://github.com/penrose/penrose/issues/937)) ([261fd4c](https://github.com/penrose/penrose/commit/261fd4cb12f342555572aa40cfc48ffee58dbbfd))
- Add Support for images with absolute URLs ([#1033](https://github.com/penrose/penrose/issues/1033)) ([03a9b03](https://github.com/penrose/penrose/commit/03a9b035b0ead1a28dd6980f58a6c42ceea165c5))
- roger NPE in staged mode. Fixed [#887](https://github.com/penrose/penrose/issues/887) ([#946](https://github.com/penrose/penrose/issues/946)) ([dc50785](https://github.com/penrose/penrose/commit/dc507855ac6d18fc7033c1df7f75efcc181e20c4))
- bump pug version ([#669](https://github.com/penrose/penrose/issues/669)) ([633f101](https://github.com/penrose/penrose/commit/633f101aa505edca938897afa536f0c2a1b61885))
- Docs-site Shape Property page errors and crashes ([#1045](https://github.com/penrose/penrose/issues/1045)) ([880d197](https://github.com/penrose/penrose/commit/880d197aa37290a43cccad9966f313f2d521b32f))
- Fix [#935](https://github.com/penrose/penrose/issues/935) roger fails in draw mode ([#936](https://github.com/penrose/penrose/issues/936)) ([5b3d464](https://github.com/penrose/penrose/commit/5b3d46459c95f9ed68c7f80e3f9219a6171c5008))
- Make autodiff deterministic in graph shape ([#945](https://github.com/penrose/penrose/issues/945)) ([c6fe4e3](https://github.com/penrose/penrose/commit/c6fe4e33cf148c6c85f887deb7466468376bba87))
- path resolution in `roger` ([#836](https://github.com/penrose/penrose/issues/836)) ([52972af](https://github.com/penrose/penrose/commit/52972af9550fdb4ca4e2ffc6f19f99a10e0a231a))
- return types in exported functions ([#637](https://github.com/penrose/penrose/issues/637)) ([944eb01](https://github.com/penrose/penrose/commit/944eb01ecf2dcd8b1b233c712921ec3fd6abe905))

### Features

- compute `Text` bounding box ([#829](https://github.com/penrose/penrose/issues/829)) ([8886074](https://github.com/penrose/penrose/commit/88860747560926c981459b1014f804babd343ed7))
- enumerative search of Substance mutations ([#638](https://github.com/penrose/penrose/issues/638)) ([97db076](https://github.com/penrose/penrose/commit/97db07673c16970216d56ec8af360639351361da))
- increase coverage of geometry domain for use with textbook problems ([#633](https://github.com/penrose/penrose/issues/633)) ([04619a0](https://github.com/penrose/penrose/commit/04619a0eda32a3ad3627603120daa83ed5d4b2dd))
- Make Penrose deterministic ([#864](https://github.com/penrose/penrose/issues/864)) ([baabbe6](https://github.com/penrose/penrose/commit/baabbe63cfee662eb1f97a0782ca3a1d609af4cd))
- resolve paths for included SVGs ([#825](https://github.com/penrose/penrose/issues/825)) ([cedbf1b](https://github.com/penrose/penrose/commit/cedbf1b0f219f013a0c825e08007a2edc3b2c3bc))
- staged diagram generation in roger ([#610](https://github.com/penrose/penrose/issues/610)) ([3de4a31](https://github.com/penrose/penrose/commit/3de4a31543ddac80ed24274fde66d9e84304daa1))
- support layout stages in Style ([#1199](https://github.com/penrose/penrose/issues/1199)) ([d22602a](https://github.com/penrose/penrose/commit/d22602a7f31ce48c0c00a984efec5fa3622e63eb))
- unify browser-ui and editor ([#1000](https://github.com/penrose/penrose/issues/1000)) ([3e7f647](https://github.com/penrose/penrose/commit/3e7f64729fb36ba7c735f0360dcc4f33fd04a49c))

### Performance Improvements

- port the optimizer to WebAssembly ([#1092](https://github.com/penrose/penrose/issues/1092)) ([768895a](https://github.com/penrose/penrose/commit/768895a3aac643095f0d139052fa8a139ce28cfb))
- speed up `roger` using SWC ([#1163](https://github.com/penrose/penrose/issues/1163)) ([516f57e](https://github.com/penrose/penrose/commit/516f57eeddba6110964623c0f88de24cef9d2ccd))

# [1.3.0](https://github.com/penrose/penrose/compare/v1.2.0...v1.3.0) (2021-06-24)

**Note:** Version bump only for package @penrose/roger

# [1.2.0](https://github.com/penrose/penrose/compare/v1.1.0...v1.2.0) (2021-05-24)

### Features

- substance program synthesizer ([#551](https://github.com/penrose/penrose/issues/551)) ([09062ee](https://github.com/penrose/penrose/commit/09062eee7bd027396905958cf009305fcc8aa6f6))

# [1.1.0](https://github.com/penrose/penrose/compare/v1.0.0...v1.1.0) (2021-04-21)

### Features

- `evalEnergy` API function ([#512](https://github.com/penrose/penrose/issues/512)) ([2830bf0](https://github.com/penrose/penrose/commit/2830bf0f30a69f53e4928cce0882747764ecaa6e))

# 1.0.0 (2021-02-15)

**Note:** Version bump only for package @penrose/roger

# [1.0.0-alpha.3](https://github.com/penrose/penrose/compare/v1.0.0-alpha.2...v1.0.0-alpha.3) (2021-02-15)

**Note:** Version bump only for package @penrose/roger

# [1.0.0-alpha.1](https://github.com/penrose/penrose/compare/v1.0.0-alpha.0...v1.0.0-alpha.1) (2021-02-15)

**Note:** Version bump only for package @penrose/roger

## 1.0.1 (2021-01-28)

**Note:** Version bump only for package roger
