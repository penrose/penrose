# Change Log

All notable changes to this project will be documented in this file.
See [Conventional Commits](https://conventionalcommits.org) for commit guidelines.

## [v3.1.0](https://github.com/penrose/penrose/compare/v3.0.0...v3.1.0) (2023-07-19)

**Note:** Version bump only for package @penrose/editor





## [v3.0.0](https://github.com/penrose/penrose/compare/v2.3.0...v3.0.0) (2023-07-14)

### :warning: BREAKING CHANGE

- combine `automator` and `roger` (#1387)
- more readable `core` language API (#1527)
- put each trio in its own JSON file (#1393)
- support non-trio examples in the registry (#1418)

### :rocket: New Feature

- Walk on stars ([#1493](https://github.com/penrose/penrose/issues/1493)) ([1678a91](https://github.com/penrose/penrose/commit/1678a912c5535f2f9896d20a851f421fb75f669e))
- `renderer` adds tightest viewbox metadata to SVG ([#1444](https://github.com/penrose/penrose/issues/1444)) ([0c7b9f0](https://github.com/penrose/penrose/commit/0c7b9f0b16a1754ee0fdae8aabf1f0b1386e81cd))
- box-arrow style and computer architecture diagram ([#1492](https://github.com/penrose/penrose/issues/1492)) ([2c428c3](https://github.com/penrose/penrose/commit/2c428c3227e728fa6cda1d91aaf7ef00843eebe3))
- cropping picks smaller viewbox in gallery and `editor` ([#1518](https://github.com/penrose/penrose/issues/1518)) ([b59854a](https://github.com/penrose/penrose/commit/b59854a69bd40e0d543fa16d6875150fb57207a9))
- error and warning markings in IDE editor ([#1513](https://github.com/penrose/penrose/issues/1513)) ([c91a6c9](https://github.com/penrose/penrose/commit/c91a6c9794cc26a2e9134fe60e02f5d33e572a5b))
- export SVGs with plain TeX labels ([#1433](https://github.com/penrose/penrose/issues/1433)) ([3e589a0](https://github.com/penrose/penrose/commit/3e589a08fc1c5b46d4e9143655722058970219dc))
- function warnings ([#1498](https://github.com/penrose/penrose/issues/1498)) ([3e68a94](https://github.com/penrose/penrose/commit/3e68a94ab933a841d882ea26c26a58b5728629bf))
- homepage gallery component ([#1464](https://github.com/penrose/penrose/issues/1464)) ([130b2b7](https://github.com/penrose/penrose/commit/130b2b77904b926863c7348b7de085a792117ca9))
- line wrap in error pane ([#1357](https://github.com/penrose/penrose/issues/1357)) ([58bd78b](https://github.com/penrose/penrose/commit/58bd78b4f9a8d1e8ae7d2fa1c937dcbf194b0df4))
- show warnings in `editor` ([#1381](https://github.com/penrose/penrose/issues/1381)) ([31a59f9](https://github.com/penrose/penrose/commit/31a59f9fab695281beef2528f172a4764655481c))
- support non-trio examples in the registry ([#1418](https://github.com/penrose/penrose/issues/1418)) ([4fe953f](https://github.com/penrose/penrose/commit/4fe953fb26778e28debae7221de3f05ef30c4e2b))
- support trio switching in `roger watch` and `editor` ([#1486](https://github.com/penrose/penrose/issues/1486)) ([8581efc](https://github.com/penrose/penrose/commit/8581efc9cac3211ca6732e1d7f1d5d542aa4fbc3))

### :bug: Bug Fix

- Diagram panel empty on iOS Chrome and iOS/macOS Safari ([#1465](https://github.com/penrose/penrose/issues/1465)) ([cb89d4c](https://github.com/penrose/penrose/commit/cb89d4cb8bf815f2ff39e9e8f28ec532b956db43))
- query string for examples in `editor` ([#1519](https://github.com/penrose/penrose/issues/1519)) ([5c11a6d](https://github.com/penrose/penrose/commit/5c11a6d152d74735f666945852ea57045356e289))
- remove width and height attributes in `editor` SVG export ([#1410](https://github.com/penrose/penrose/issues/1410)) ([3f68541](https://github.com/penrose/penrose/commit/3f685410b3543e59c77fd8c88893acb18ddfec2d))
- reuse resolver when example becomes local ([#1473](https://github.com/penrose/penrose/issues/1473)) ([5ba3c00](https://github.com/penrose/penrose/commit/5ba3c00eae4709bdf79c48de32a5f98e4eb95a6f))

### :nail_care: Polish

- bump TypeScript version to 5.0 ([#1395](https://github.com/penrose/penrose/issues/1395)) ([b4ae329](https://github.com/penrose/penrose/commit/b4ae3298c9a03926ca690c63f368adcaa031b56d))
- combine `automator` and `roger` ([#1387](https://github.com/penrose/penrose/issues/1387)) ([678c6e5](https://github.com/penrose/penrose/commit/678c6e528d20d6cbbfd3a04f1fcad656e72bdc6e))
- more readable `core` language API ([#1527](https://github.com/penrose/penrose/issues/1527)) ([22c8fc6](https://github.com/penrose/penrose/commit/22c8fc68f225974a353df244832b3b1c90e5f0e0))
- pull out base `tsconfig.json` ([#1392](https://github.com/penrose/penrose/issues/1392)) ([e6c5f55](https://github.com/penrose/penrose/commit/e6c5f5524837fe4c970713f05bbed821b9cda411))
- put each trio in its own JSON file ([#1393](https://github.com/penrose/penrose/issues/1393)) ([803d7fc](https://github.com/penrose/penrose/commit/803d7fc20199262f833c2f60606ed1b778c92b72))

### :memo: Documentation

- add a page about tools and update docs overview ([#1521](https://github.com/penrose/penrose/issues/1521)) ([d179143](https://github.com/penrose/penrose/commit/d179143ea72a8d4cbdfee766ea7cfc98cbf4999d))
- fix the tutorial ([#1501](https://github.com/penrose/penrose/issues/1501)) ([60332b7](https://github.com/penrose/penrose/commit/60332b7b0418fbdf92bf61de0771f1b6c3ded355))
- homepage tweaks ([#1515](https://github.com/penrose/penrose/issues/1515)) ([0fa290e](https://github.com/penrose/penrose/commit/0fa290e3264c8c20c768a9ef509a768ee10d5030))

### :house: Internal

- Revert "fix: Diagram panel empty on iOS Chrome and iOS/macOS Safari" ([#1466](https://github.com/penrose/penrose/issues/1466)) ([fad7125](https://github.com/penrose/penrose/commit/fad7125dc99698185bd8d2d7a9ee5388969f2a03))
- bump version to 3.0.0-beta.0 ([#1542](https://github.com/penrose/penrose/issues/1542)) ([ef4fffb](https://github.com/penrose/penrose/commit/ef4fffbf22e03fdd3af84c439163ff24bc5ccb41))
- bump version to 3.0.0-beta.1 ([#1543](https://github.com/penrose/penrose/issues/1543)) ([abe43d9](https://github.com/penrose/penrose/commit/abe43d9be98a719204b54cbf3abf4bbec9367d16))
- enable example panel on `editor` startup ([#1510](https://github.com/penrose/penrose/issues/1510)) ([8888e7f](https://github.com/penrose/penrose/commit/8888e7fc0b635b7909f08e8bc3adcf15fed01ac6))
- switch from jest to vitest ([#1406](https://github.com/penrose/penrose/issues/1406)) ([8ef8c77](https://github.com/penrose/penrose/commit/8ef8c778488b17eb0f02a62d1399e0b0337f5355))

## [v2.3.0](https://github.com/penrose/penrose/compare/v2.2.0...v2.3.0) (2023-03-14)

### :rocket: New Feature

- compile diagrams in `editor` after detected changes in `roger` ([#1264](https://github.com/penrose/penrose/issues/1264)) ([5ec39dd](https://github.com/penrose/penrose/commit/5ec39ddf92859d653768ddaa088f36f1b522e1af))
- show multiple diagram instances on a grid in `editor` ([#1287](https://github.com/penrose/penrose/issues/1287)) ([fbaf03c](https://github.com/penrose/penrose/commit/fbaf03c7b6c4f87cc628111ee080af76c65ef55e))

### :bug: Bug Fix

- nondeterminism in renderer ([#1316](https://github.com/penrose/penrose/issues/1316)) ([9795420](https://github.com/penrose/penrose/commit/97954202c60c2aab6a11af1694f652f8a3bb8e4d))

## [v2.2.0](https://github.com/penrose/penrose/compare/v2.1.1...v2.2.0) (2023-02-02)

### :rocket: New Feature

- improve registry schema and loading ([#1212](https://github.com/penrose/penrose/issues/1212)) ([d6bbc30](https://github.com/penrose/penrose/commit/d6bbc302de494e08fa4ca0602ccfa29bdfcd65ae))
- support longer file extensions ([#1280](https://github.com/penrose/penrose/issues/1280)) ([6e83596](https://github.com/penrose/penrose/commit/6e835968280a784a91c4a2ca47a226516a3067d0))

## [v2.1.1](https://github.com/penrose/penrose/compare/v2.1.0...v2.1.1) (2023-01-19)

**Note:** Version bump only for package @penrose/editor

# [2.1.0](https://github.com/penrose/penrose/compare/v2.0.0...v2.1.0) (2023-01-19)

**Note:** Version bump only for package @penrose/editor

# [2.0.0](https://github.com/penrose/penrose/compare/v1.3.0...v2.0.0) (2023-01-17)

### Bug Fixes

- Add Support for images with absolute URLs ([#1033](https://github.com/penrose/penrose/issues/1033)) ([03a9b03](https://github.com/penrose/penrose/commit/03a9b035b0ead1a28dd6980f58a6c42ceea165c5))
- base url ([#997](https://github.com/penrose/penrose/issues/997)) ([d329dd0](https://github.com/penrose/penrose/commit/d329dd067b82b182c2eab1a6436fc8b3657b1975))
- canvas disappears on mobile browsers ([#1136](https://github.com/penrose/penrose/issues/1136)) ([a060858](https://github.com/penrose/penrose/commit/a060858a2f79a5da93c12421e258db33b36a4d22))
- delete `svg-flatten` ([#1208](https://github.com/penrose/penrose/issues/1208)) ([976ca77](https://github.com/penrose/penrose/commit/976ca770c8eae9b95d2d6f7b36937005bbac8bcf))
- Give Vite 8 GiB in editor ([#1090](https://github.com/penrose/penrose/issues/1090)) ([3e328c8](https://github.com/penrose/penrose/commit/3e328c81f1843866aac89ebc947b463c08207b80))
- Issue [#1023](https://github.com/penrose/penrose/issues/1023) Allow stroke on Equations ([#1026](https://github.com/penrose/penrose/issues/1026)) ([77e1f87](https://github.com/penrose/penrose/commit/77e1f870ccd02794a0ccdc2f1a9ffcf2e96be829))
- Local image resolution not working in IDE ([#1037](https://github.com/penrose/penrose/issues/1037)) ([c5220b4](https://github.com/penrose/penrose/commit/c5220b43a753a7e8972331f0a5253c3fe475c06b))
- resize behaviors of diagram panel components ([#1105](https://github.com/penrose/penrose/issues/1105)) ([7874667](https://github.com/penrose/penrose/commit/787466793507105b34674cfe1d6e637c160db4ae))
- responsive tab layout on mobile ([#1137](https://github.com/penrose/penrose/issues/1137)) ([95b7f3e](https://github.com/penrose/penrose/commit/95b7f3eac44b1ee29d61d88a071be94eea33780a))
- storybook deployment base directory ([#1124](https://github.com/penrose/penrose/issues/1124)) ([7fb7a01](https://github.com/penrose/penrose/commit/7fb7a013d01a9fc1d40310b2a8505f99d7ea9468))

### Features

- add "Duplicate Workspace" button ([#1106](https://github.com/penrose/penrose/issues/1106)) ([e576078](https://github.com/penrose/penrose/commit/e576078101a21f1103472ec6d6ad74d20140a618))
- add debug mode setting to `@penrose/editor` ([#1030](https://github.com/penrose/penrose/issues/1030)) ([d9c5485](https://github.com/penrose/penrose/commit/d9c54856f6899488694add3f44ceb966cfd4244e))
- compile on vim write (resolves [#1046](https://github.com/penrose/penrose/issues/1046)) ([#1197](https://github.com/penrose/penrose/issues/1197)) ([9ee17a6](https://github.com/penrose/penrose/commit/9ee17a62bbc78b73caa4c093ed4131359af588d5))
- Editor Rewrite ([#992](https://github.com/penrose/penrose/issues/992)) ([91022fa](https://github.com/penrose/penrose/commit/91022fafdd45e6e5810bcb87448095a1d105bae5))
- export diagrams in png ([#1134](https://github.com/penrose/penrose/issues/1134)) ([307c574](https://github.com/penrose/penrose/commit/307c574bfa3c1a11171a8382fa615b3c58c48265))
- host tutorial in online editor ([#1196](https://github.com/penrose/penrose/issues/1196)) ([4f361c9](https://github.com/penrose/penrose/commit/4f361c92de4544247722c931178c786a6546434e))
- improve `euclidean.sty` ([#1117](https://github.com/penrose/penrose/issues/1117)) ([3a94d6d](https://github.com/penrose/penrose/commit/3a94d6d57a8c9c0e4809a05ad2d2711d919349e0))
- Make SVGs "Penrose-editable" ([#1171](https://github.com/penrose/penrose/issues/1171)) ([edb5dc8](https://github.com/penrose/penrose/commit/edb5dc8d80fa86e762dfd4bf17d9a66e1d59a950))
- resolve image paths in `@penrose/editor` ([#1018](https://github.com/penrose/penrose/issues/1018)) ([7bb69e6](https://github.com/penrose/penrose/commit/7bb69e6ecbec42b1f500067e7c77dcc92ac665fa))
- Signed distance functions for Penrose shapes ([#979](https://github.com/penrose/penrose/issues/979)) ([1a00e4c](https://github.com/penrose/penrose/commit/1a00e4c113c8e1e308612e41528af50665d7b194))
- support layout stages in Style ([#1199](https://github.com/penrose/penrose/issues/1199)) ([d22602a](https://github.com/penrose/penrose/commit/d22602a7f31ce48c0c00a984efec5fa3622e63eb))
- unify browser-ui and editor ([#1000](https://github.com/penrose/penrose/issues/1000)) ([3e7f647](https://github.com/penrose/penrose/commit/3e7f64729fb36ba7c735f0360dcc4f33fd04a49c))
- Walk on spheres ([#1019](https://github.com/penrose/penrose/issues/1019)) ([a5d5da1](https://github.com/penrose/penrose/commit/a5d5da1b3e3eabf53360434b9bd6b806780d1eac))

### Performance Improvements

- port the optimizer to WebAssembly ([#1092](https://github.com/penrose/penrose/issues/1092)) ([768895a](https://github.com/penrose/penrose/commit/768895a3aac643095f0d139052fa8a139ce28cfb))

### Reverts

- Revert "feat: Walk on spheres (#1019)" (#1021) ([228746e](https://github.com/penrose/penrose/commit/228746ee7544e4cf69c84f7bf871f0c9d95edcc5)), closes [#1019](https://github.com/penrose/penrose/issues/1019) [#1021](https://github.com/penrose/penrose/issues/1021)
