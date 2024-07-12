# Change Log

All notable changes to this project will be documented in this file.
See [Conventional Commits](https://conventionalcommits.org) for commit guidelines.

## [v4.0.0-alpha.1] (2024-07-11)

### :warning: BREAKING CHANGE

- separate Substance environment and remove unused features (#1677)

### :rocket: New Feature

- Added copy trio to clipboard feature ([#1763](https://github.com/penrose/penrose/issues/1763)) ([e4193ab](https://github.com/penrose/penrose/commit/e4193ab64e7433b7c61e44ae3d250d5c70683090))
- LSP-style optimization worker(s) ([#1801](https://github.com/penrose/penrose/issues/1801)) ([cff021a](https://github.com/penrose/penrose/commit/cff021a613cf1f796efce92e4a58f1dc34d66c65))
- Lsp-style worker + editor vite upgrade ([#1804](https://github.com/penrose/penrose/issues/1804)) ([f711e9d](https://github.com/penrose/penrose/commit/f711e9dff1120ccd799d2544c983be8df26ebafa))
- Making gist sharing clearer (editor ux) ([#1764](https://github.com/penrose/penrose/issues/1764)) ([0ad9f39](https://github.com/penrose/penrose/commit/0ad9f39cd57a8a722241cb89811787a1b6ad236e))
- added cntrl/cmd+enter and :w to compile diagram ([#1822](https://github.com/penrose/penrose/issues/1822)) ([30ff1d6](https://github.com/penrose/penrose/commit/30ff1d621d83db680c3013b49f82aa4df934fc43))
- codemirror migration ([#1798](https://github.com/penrose/penrose/issues/1798)) ([59d77f2](https://github.com/penrose/penrose/commit/59d77f22822cc28a32daf812f0b8c4b9e791786c))
- initial values for unknown variables ([#1638](https://github.com/penrose/penrose/issues/1638)) ([61f2ad7](https://github.com/penrose/penrose/commit/61f2ad766efc471bf552a6cfc3d9fcf9c2f3779a))
- interactive widgets + programatically constrained dragging (experiemental) ([#1796](https://github.com/penrose/penrose/issues/1796)) ([d5f269e](https://github.com/penrose/penrose/commit/d5f269e9fd60740997033b93c2c99cf3a6926bdb))
- run the compiler and optimizer in a web worker ([#1681](https://github.com/penrose/penrose/issues/1681)) ([42a657d](https://github.com/penrose/penrose/commit/42a657dcb32e565ebd8923df671f673b6b6436d5))
- warn `editor` users to save local changes ([#1734](https://github.com/penrose/penrose/issues/1734)) ([c088040](https://github.com/penrose/penrose/commit/c088040a604034e6776b8dcf8dd8a8b39180ca0c))
- workspace button ([#1741](https://github.com/penrose/penrose/issues/1741)) ([09bbd05](https://github.com/penrose/penrose/commit/09bbd0592d28d3aa318f79a10d19a8ad6ca4378a))

### :bug: Bug Fix

- Revert "feat: LSP-style optimization worker(s)" ([#1803](https://github.com/penrose/penrose/issues/1803)) ([9dc8968](https://github.com/penrose/penrose/commit/9dc8968716c4ac4584a152de81744bb0b555785e))
- Solves deletion override and diagram panel not updating ([#1742](https://github.com/penrose/penrose/issues/1742)) ([a7a303f](https://github.com/penrose/penrose/commit/a7a303fb7f38269f889a6994d4dfce117f78ad9d))
- compilation errors not displaying properly ([#1769](https://github.com/penrose/penrose/issues/1769)) ([284a324](https://github.com/penrose/penrose/commit/284a32488d87294e3223b6c8b863d48c57dc6e8d))
- download functionality in the editor ([#1678](https://github.com/penrose/penrose/issues/1678)) ([6401b84](https://github.com/penrose/penrose/commit/6401b841d7413ef1c6a878c84927c737dff80858))
- duplicate `<penrose>` tags in exported SVGs ([#1652](https://github.com/penrose/penrose/issues/1652)) ([1da2cab](https://github.com/penrose/penrose/commit/1da2cabc1ec11d1fdba101dedc0cff5f63ed5880))
- failed handling of optimizer errors ([#1792](https://github.com/penrose/penrose/issues/1792)) ([4076594](https://github.com/penrose/penrose/commit/4076594b3b23510cb13698e8b005304ee636a372))
- fixed SVG upload ([#1753](https://github.com/penrose/penrose/issues/1753)) ([58eccb9](https://github.com/penrose/penrose/commit/58eccb9e756dbccd826a2b62c14773dcebee7be2))
- misc. webworker + IDE fixes ([#1774](https://github.com/penrose/penrose/issues/1774)) ([74c49f8](https://github.com/penrose/penrose/commit/74c49f812869f9ad78e126914edf8ebca0a4902e))
- no error markers ([#1771](https://github.com/penrose/penrose/issues/1771)) ([fbe8a81](https://github.com/penrose/penrose/commit/fbe8a8124f2be25202cd84ec02d7317f40da6d59))
- off-by-one error in time-travel slider ([#1698](https://github.com/penrose/penrose/issues/1698)) ([bcb0c6c](https://github.com/penrose/penrose/commit/bcb0c6caff00d7c95220161d07f00e6b7840e8d4))
- optimizer entering error state on compile error from init ([#1800](https://github.com/penrose/penrose/issues/1800)) ([6a7ce9a](https://github.com/penrose/penrose/commit/6a7ce9ac254a5b04c2981e20aa6051596f32fc9f))
- queue and resolve pending requests in optimizer worker ([#1722](https://github.com/penrose/penrose/issues/1722)) ([357b1da](https://github.com/penrose/penrose/commit/357b1da0d53abb39408863ce78a28f45150d6716))
- regression of TeX SVG download ([#1759](https://github.com/penrose/penrose/issues/1759)) ([8f6f0b8](https://github.com/penrose/penrose/commit/8f6f0b80e01969348206ab143cfdb91ee2d37d32))
- resolve state-related bugs in the editor ([#1696](https://github.com/penrose/penrose/issues/1696)) ([b9e08d4](https://github.com/penrose/penrose/commit/b9e08d4f8897efd194a0ee843ebcdad26eaa91da))
- separate Substance environment and remove unused features ([#1677](https://github.com/penrose/penrose/issues/1677)) ([7ad14e7](https://github.com/penrose/penrose/commit/7ad14e7d819d1ddedc03a75eb17f17532430b3aa))
- style canvas default indent error ([#1747](https://github.com/penrose/penrose/issues/1747)) ([5c92e62](https://github.com/penrose/penrose/commit/5c92e62f455de6514593d3985e94985c0573f8be))

### :nail_care: Polish

- OptimizerWorker/worker ([#1765](https://github.com/penrose/penrose/issues/1765)) ([4110f80](https://github.com/penrose/penrose/commit/4110f80092aa6b4c1f199a22a7144ab1220dceeb))
- simplified shared `Grid` and custom `Grid` in Edgeworth ([#1729](https://github.com/penrose/penrose/issues/1729)) ([a7c9d4d](https://github.com/penrose/penrose/commit/a7c9d4d5d8cc44b3544afb826da3a7002ef55cff))

### :house: Internal

- add default canvas block to `editor` ([#1674](https://github.com/penrose/penrose/issues/1674)) ([52f8fd2](https://github.com/penrose/penrose/commit/52f8fd28e1cfc22e0c9750f3c9cdc61e5f9a6591))
- bump version to 4.0.0-alpha.0 ([#1735](https://github.com/penrose/penrose/issues/1735)) ([da1343d](https://github.com/penrose/penrose/commit/da1343dca34cb1291894b754b7f7ac3aed672df5))

### :running_woman: Performance

- optimizer worker speed up ([#1772](https://github.com/penrose/penrose/issues/1772)) ([3383139](https://github.com/penrose/penrose/commit/3383139ca526b2f991b78c984b8a2ef5717aca74))

## [v4.0.0-alpha.0] (2024-05-07)

### :warning: BREAKING CHANGE

- separate Substance environment and remove unused features (#1677)

### :rocket: New Feature

- initial values for unknown variables ([#1638](https://github.com/penrose/penrose/issues/1638)) ([61f2ad7](https://github.com/penrose/penrose/commit/61f2ad766efc471bf552a6cfc3d9fcf9c2f3779a))
- run the compiler and optimizer in a web worker ([#1681](https://github.com/penrose/penrose/issues/1681)) ([42a657d](https://github.com/penrose/penrose/commit/42a657dcb32e565ebd8923df671f673b6b6436d5))
- warn `editor` users to save local changes ([#1734](https://github.com/penrose/penrose/issues/1734)) ([c088040](https://github.com/penrose/penrose/commit/c088040a604034e6776b8dcf8dd8a8b39180ca0c))

### :bug: Bug Fix

- download functionality in the editor ([#1678](https://github.com/penrose/penrose/issues/1678)) ([6401b84](https://github.com/penrose/penrose/commit/6401b841d7413ef1c6a878c84927c737dff80858))
- duplicate `<penrose>` tags in exported SVGs ([#1652](https://github.com/penrose/penrose/issues/1652)) ([1da2cab](https://github.com/penrose/penrose/commit/1da2cabc1ec11d1fdba101dedc0cff5f63ed5880))
- off-by-one error in time-travel slider ([#1698](https://github.com/penrose/penrose/issues/1698)) ([bcb0c6c](https://github.com/penrose/penrose/commit/bcb0c6caff00d7c95220161d07f00e6b7840e8d4))
- queue and resolve pending requests in optimizer worker ([#1722](https://github.com/penrose/penrose/issues/1722)) ([357b1da](https://github.com/penrose/penrose/commit/357b1da0d53abb39408863ce78a28f45150d6716))
- resolve state-related bugs in the editor ([#1696](https://github.com/penrose/penrose/issues/1696)) ([b9e08d4](https://github.com/penrose/penrose/commit/b9e08d4f8897efd194a0ee843ebcdad26eaa91da))
- separate Substance environment and remove unused features ([#1677](https://github.com/penrose/penrose/issues/1677)) ([7ad14e7](https://github.com/penrose/penrose/commit/7ad14e7d819d1ddedc03a75eb17f17532430b3aa))

### :nail_care: Polish

- simplified shared `Grid` and custom `Grid` in Edgeworth ([#1729](https://github.com/penrose/penrose/issues/1729)) ([a7c9d4d](https://github.com/penrose/penrose/commit/a7c9d4d5d8cc44b3544afb826da3a7002ef55cff))

### :house: Internal

- add default canvas block to `editor` ([#1674](https://github.com/penrose/penrose/issues/1674)) ([52f8fd2](https://github.com/penrose/penrose/commit/52f8fd28e1cfc22e0c9750f3c9cdc61e5f9a6591))

## [v3.2.0](https://github.com/penrose/penrose/compare/v3.1.0...v3.2.0) (2023-08-08)

**Note:** Version bump only for package @penrose/editor

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
