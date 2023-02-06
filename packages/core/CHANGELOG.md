# Change Log

All notable changes to this project will be documented in this file.
See [Conventional Commits](https://conventionalcommits.org) for commit guidelines.

## [v2.2.0](https://github.com/penrose/penrose/compare/v2.1.1...v2.2.0) (2023-02-02)

### :rocket: New Feature

- improve registry schema and loading ([#1212](https://github.com/penrose/penrose/issues/1212)) ([d6bbc30](https://github.com/penrose/penrose/commit/d6bbc302de494e08fa4ca0602ccfa29bdfcd65ae))
- inline comparison operators ([#1257](https://github.com/penrose/penrose/issues/1257)) ([b3c7c2f](https://github.com/penrose/penrose/commit/b3c7c2f0547a245ece5865d94184d04f7edf334e))
- support longer file extensions ([#1280](https://github.com/penrose/penrose/issues/1280)) ([6e83596](https://github.com/penrose/penrose/commit/6e835968280a784a91c4a2ca47a226516a3067d0))

### :bug: Bug Fix

- enforcing ordering in `collinearOrdered` constraint ([#1265](https://github.com/penrose/penrose/issues/1265)) ([2336b4b](https://github.com/penrose/penrose/commit/2336b4b2a567fa520219fef4768c6e0406c310d9))

### :house: Internal

- clarify a couple `Graph` method docstrings ([#1285](https://github.com/penrose/penrose/issues/1285)) ([6da46aa](https://github.com/penrose/penrose/commit/6da46aa79599f29a69301ea777be16ff8bfb616e))

## [v2.1.1](https://github.com/penrose/penrose/compare/v2.1.0...v2.1.1) (2023-01-19)

### :bug: Bug Fix

- make pandemonium a dependency of core ([#1249](https://github.com/penrose/penrose/issues/1249)) ([aac81e8](https://github.com/penrose/penrose/commit/aac81e856182a246c7c1dff96aed91bf7e260b1e))
- symmetric predicate check runs on empty type graph ([#1248](https://github.com/penrose/penrose/issues/1248)) ([2493c51](https://github.com/penrose/penrose/commit/2493c51fd5ddbbd690387a13cb777e4ca723f393))

### :memo: Documentation

- update `core` usage in README ([#1250](https://github.com/penrose/penrose/issues/1250)) ([cb1781f](https://github.com/penrose/penrose/commit/cb1781f7ec26ea7692e874af170eb0fdc8b85e1d))

# [2.1.0](https://github.com/penrose/penrose/compare/v2.0.0...v2.1.0) (2023-01-19)

### Bug Fixes

- Stage reset after dragging ([#1237](https://github.com/penrose/penrose/issues/1237)) ([9bf0a36](https://github.com/penrose/penrose/commit/9bf0a363e2554451c0685157c099e91b8134445a))
- style relation checker using substance variables ([#1239](https://github.com/penrose/penrose/issues/1239)) ([2e7de5e](https://github.com/penrose/penrose/commit/2e7de5e7729ae6f54ed378fb98c843f821b72ccf))
- Wrong flipped start arrowhead X offset ([#1236](https://github.com/penrose/penrose/issues/1236)) ([ce7a348](https://github.com/penrose/penrose/commit/ce7a34823bb9cf71da6749366222ef72e4c8e0cb))

### Features

- Functions/Constraints for curves ([#1206](https://github.com/penrose/penrose/issues/1206)) ([6edc412](https://github.com/penrose/penrose/commit/6edc412fadeb8c1cb813719ea1b189bd35fa7ecb))

# [2.0.0](https://github.com/penrose/penrose/compare/v1.3.0...v2.0.0) (2023-01-17)

### Bug Fixes

- errors for undefined terms in Style selectors ([#660](https://github.com/penrose/penrose/issues/660)) ([e7f1f5f](https://github.com/penrose/penrose/commit/e7f1f5f0d4ca1873875986d1400a6884e52f9c44))
- account for descent in `Text` center computation ([#842](https://github.com/penrose/penrose/issues/842)) ([d9999eb](https://github.com/penrose/penrose/commit/d9999eb7ffa538b20d264f255782c53d52ffe004))
- add duplicate name check in Substance checker ([#657](https://github.com/penrose/penrose/issues/657)) ([1a3df91](https://github.com/penrose/penrose/commit/1a3df9134385f4b7fef31e9174b34a64f7852cbe))
- add labels to unmatched Substance objects in the translation ([#666](https://github.com/penrose/penrose/issues/666)) ([15462a6](https://github.com/penrose/penrose/commit/15462a6d3c56ad4cf2d37369c49eef664f5b4e88))
- Catch errors thrown in `core` in `browser-ui` ([#625](https://github.com/penrose/penrose/issues/625)) ([2a2ca45](https://github.com/penrose/penrose/commit/2a2ca454be27a72b36846f24dd89fd94d8941603))
- Change the derivative of abs to be sign ([#1104](https://github.com/penrose/penrose/issues/1104)) ([2cb933f](https://github.com/penrose/penrose/commit/2cb933f567501ff58268ce9def74781c7c4c803b))
- check constructor name match in Style selector ([#757](https://github.com/penrose/penrose/issues/757)) ([3ab0042](https://github.com/penrose/penrose/commit/3ab0042f7603d000d170643fe93bb31e4c80081f))
- circle-rectangle interactions ([#848](https://github.com/penrose/penrose/issues/848)) ([428cad6](https://github.com/penrose/penrose/commit/428cad66840054378a736bd948acbdbcd15d4bbc))
- clone rendered SVG node for `Equation` at render-time ([#1144](https://github.com/penrose/penrose/issues/1144)) ([47b6dd2](https://github.com/penrose/penrose/commit/47b6dd217ab06426b8022289f4933b9cf0ff9d00))
- Cos/sin shouldn't use degrees [#374](https://github.com/penrose/penrose/issues/374) ([#677](https://github.com/penrose/penrose/issues/677)) ([d65ac8d](https://github.com/penrose/penrose/commit/d65ac8d61ed6403bcd2965e7af1fbdd903ad5beb)), closes [#651](https://github.com/penrose/penrose/issues/651)
- Default fill xor stroke. FreeformPolygon stack dumps renderer. Closes [#704](https://github.com/penrose/penrose/issues/704), Closes [#706](https://github.com/penrose/penrose/issues/706), Closes [#708](https://github.com/penrose/penrose/issues/708) ([#707](https://github.com/penrose/penrose/issues/707)) ([7662137](https://github.com/penrose/penrose/commit/766213755b7a4762da510b20f45a409943006c58)), closes [#651](https://github.com/penrose/penrose/issues/651) [#374](https://github.com/penrose/penrose/issues/374) [#392](https://github.com/penrose/penrose/issues/392) [#392](https://github.com/penrose/penrose/issues/392) [#392](https://github.com/penrose/penrose/issues/392) [#392](https://github.com/penrose/penrose/issues/392) [#392](https://github.com/penrose/penrose/issues/392)
- delete `svg-flatten` ([#1208](https://github.com/penrose/penrose/issues/1208)) ([976ca77](https://github.com/penrose/penrose/commit/976ca770c8eae9b95d2d6f7b36937005bbac8bcf))
- determine `pointer-event` for dragging based on shape kind ([#686](https://github.com/penrose/penrose/issues/686)) ([c3c0cc3](https://github.com/penrose/penrose/commit/c3c0cc3fa59ad0dfed41b2ec462644cd4531b220))
- Docs-site Shape Property page errors and crashes ([#1045](https://github.com/penrose/penrose/issues/1045)) ([880d197](https://github.com/penrose/penrose/commit/880d197aa37290a43cccad9966f313f2d521b32f))
- empty label check in Style selector ([#789](https://github.com/penrose/penrose/issues/789)) ([f28c1ba](https://github.com/penrose/penrose/commit/f28c1ba80665e36fc0f10a23383e21b2ad28520d))
- Issue [#1023](https://github.com/penrose/penrose/issues/1023) Allow stroke on Equations ([#1026](https://github.com/penrose/penrose/issues/1026)) ([77e1f87](https://github.com/penrose/penrose/commit/77e1f870ccd02794a0ccdc2f1a9ffcf2e96be829))
- issue [#1024](https://github.com/penrose/penrose/issues/1024) exclude name, ensureOnCavas in SVG ([#1025](https://github.com/penrose/penrose/issues/1025)) ([ed7bb4d](https://github.com/penrose/penrose/commit/ed7bb4d99eb16468193fac1eef58df5be193e294))
- Make autodiff deterministic in graph shape ([#945](https://github.com/penrose/penrose/issues/945)) ([c6fe4e3](https://github.com/penrose/penrose/commit/c6fe4e33cf148c6c85f887deb7466468376bba87))
- Make TypeDecl subType range more precise ([#963](https://github.com/penrose/penrose/issues/963)) ([a7811dd](https://github.com/penrose/penrose/commit/a7811dd500ff95a69facc57edee09d9a7806d578))
- multiple matching ([#1063](https://github.com/penrose/penrose/issues/1063)) ([eb0991b](https://github.com/penrose/penrose/commit/eb0991b87145a3547a1a3697e29b5a54619c4a96)), closes [#1064](https://github.com/penrose/penrose/issues/1064) [/github.com/penrose/penrose/issues/1064#issuecomment-1189228125](https://github.com//github.com/penrose/penrose/issues/1064/issues/issuecomment-1189228125)
- Remove duplicate 'pow' conditional ([#880](https://github.com/penrose/penrose/issues/880)) ([033a259](https://github.com/penrose/penrose/commit/033a2592c38132c2b479c70f76a9f909a9b31a9e))
- Remove Rectangle Rotation BBox logic ([#803](https://github.com/penrose/penrose/issues/803)) ([2ea92aa](https://github.com/penrose/penrose/commit/2ea92aaa86fcb4c30dfaad671937f02092463ff1))
- remove reference to disambiguateFunctions ([#940](https://github.com/penrose/penrose/issues/940)) ([a6be39f](https://github.com/penrose/penrose/commit/a6be39f6a39271bdd7aab1dec9d52f352463416d))
- Rename the padding argument of the overlapping constraint to overlap and negate its semantic ([#1130](https://github.com/penrose/penrose/issues/1130)) ([28684f4](https://github.com/penrose/penrose/commit/28684f4b0040a567d40bda927c46b8c74b1b6af7))
- repel objective on segments causes slow optimization time ([#613](https://github.com/penrose/penrose/issues/613)) ([cfc8f46](https://github.com/penrose/penrose/commit/cfc8f46d07450d189aa94b08635de78587d7b759))
- Replace Substance undefined symbol usage ([#745](https://github.com/penrose/penrose/issues/745)) ([7587ce6](https://github.com/penrose/penrose/commit/7587ce62bd71510352dcb4d91e23b0c253f591be))
- resolved NaN in the nested.sub by increasing the max size of sets shapes [#498](https://github.com/penrose/penrose/issues/498) ([#628](https://github.com/penrose/penrose/issues/628)) ([dcce355](https://github.com/penrose/penrose/commit/dcce35571f585602fb5bb97dfe73bdeb0dc1f457))
- return types in exported functions ([#637](https://github.com/penrose/penrose/issues/637)) ([944eb01](https://github.com/penrose/penrose/commit/944eb01ecf2dcd8b1b233c712921ec3fd6abe905))
- selector matching `n^m` complexity ([#1016](https://github.com/penrose/penrose/issues/1016)) ([930fa0d](https://github.com/penrose/penrose/commit/930fa0d2be74e2ad87e237131ac4f7fba7c27975))
- some symmetric predicates don't match ([#1127](https://github.com/penrose/penrose/issues/1127)) ([b1f35e8](https://github.com/penrose/penrose/commit/b1f35e88644bec21ad09e023afabc59425098c5b))
- Strengthen length check in zip3 ([#915](https://github.com/penrose/penrose/issues/915)) ([36c0867](https://github.com/penrose/penrose/commit/36c08679d355eddbaecf02b41d69efb8bdb8caa7))
- strokeDasharray strokeLinecap consistency ([#850](https://github.com/penrose/penrose/issues/850)) ([c7bf34b](https://github.com/penrose/penrose/commit/c7bf34b2da79ce22fd1490a617c00040bc5a33ab))
- Support empty fill and stroke style [#392](https://github.com/penrose/penrose/issues/392) ([#699](https://github.com/penrose/penrose/issues/699)) ([ee28a6f](https://github.com/penrose/penrose/commit/ee28a6f5e3f81894ea093377b1edea2a765f9731)), closes [#651](https://github.com/penrose/penrose/issues/651) [#374](https://github.com/penrose/penrose/issues/374)
- symmetry in nested predicates ([#1069](https://github.com/penrose/penrose/issues/1069)) ([7d0d4bf](https://github.com/penrose/penrose/commit/7d0d4bfc78e1e5592a4c36e5a231af9517dacaca)), closes [#1068](https://github.com/penrose/penrose/issues/1068)
- Throw error on invalid labels ([#663](https://github.com/penrose/penrose/issues/663)) ([96772ca](https://github.com/penrose/penrose/commit/96772ca0f7716bb915f244d60871e247363ef690))
- toHex usage ([#746](https://github.com/penrose/penrose/issues/746)) ([847c033](https://github.com/penrose/penrose/commit/847c033472f913ad7446d4df588411bfd6b05c9e))
- top-level diagram function ([#862](https://github.com/penrose/penrose/issues/862)) ([7f0f727](https://github.com/penrose/penrose/commit/7f0f7275c7727aec74fd3fbc1563d98220811e66))
- transform polygon/polyline points to screen space ([#849](https://github.com/penrose/penrose/issues/849)) ([861447f](https://github.com/penrose/penrose/commit/861447f66e299e1b7697f4accff245fe52be6a6e))
- unwrap the result of `stepUntilConvergence` in `browser-ui` ([#634](https://github.com/penrose/penrose/issues/634)) ([e5796fb](https://github.com/penrose/penrose/commit/e5796fb2fba1217fcab85ec08b55a260efea0d03))
- use empty string as the default label and check autolabel statements ([#754](https://github.com/penrose/penrose/issues/754)) ([6ce1b97](https://github.com/penrose/penrose/commit/6ce1b97a68c3ef002035f716ba2cc1c4065ffaaa))
- Use ifCond in atDist ([#667](https://github.com/penrose/penrose/issues/667)) ([07157b6](https://github.com/penrose/penrose/commit/07157b6fcaa00298449054ae699209aadd41c5e8))

### Features

- "follow the cursor" dragging mode ([#1143](https://github.com/penrose/penrose/issues/1143)) ([e15276c](https://github.com/penrose/penrose/commit/e15276c81da7881fd66d9f5c4d862558f63b5ed2))
- 2d triangle mesh domain ([#770](https://github.com/penrose/penrose/issues/770)) ([bfc7b2e](https://github.com/penrose/penrose/commit/bfc7b2e67d2324f024366fb6e23d4baae2b7b291))
- 2d triangle mesh examples ([#808](https://github.com/penrose/penrose/issues/808)) ([e407858](https://github.com/penrose/penrose/commit/e407858ec17091ac460bc262e5ed3dc09fedf694))
- add `@penrose/panels` to the monorepo ([#681](https://github.com/penrose/penrose/issues/681)) ([24d5bd0](https://github.com/penrose/penrose/commit/24d5bd09256b75c21b69e34e53758d732925e24e))
- Add a polynomial roots node ([#906](https://github.com/penrose/penrose/issues/906)) ([580ada4](https://github.com/penrose/penrose/commit/580ada4e2270a620f8c5662c7189c478e742eb7c))
- add browser for synthesizer ([#640](https://github.com/penrose/penrose/issues/640)) ([2d81a55](https://github.com/penrose/penrose/commit/2d81a55d0175c12195dc510f0514c741eb0f7803))
- add default `onCanvas` constraints for all shapes ([#694](https://github.com/penrose/penrose/issues/694)) ([20409e4](https://github.com/penrose/penrose/commit/20409e4756f1938e743d117af442b77abb237f6c))
- Add examples with nonconvex shapes ([#893](https://github.com/penrose/penrose/issues/893)) ([91edc5c](https://github.com/penrose/penrose/commit/91edc5c86342285d96f2e85f593129c3f2e861c1))
- add right angle marker to euclidean style ([#606](https://github.com/penrose/penrose/issues/606)) ([ec12a72](https://github.com/penrose/penrose/commit/ec12a722eada6a987c5b02c8109265b97c1708c1))
- Add SVG text ([#740](https://github.com/penrose/penrose/issues/740)) ([e066e53](https://github.com/penrose/penrose/commit/e066e539963c09c2e93fa12a09010205a3ad5a20))
- add visualization of computational graph of energy in inspector [#238](https://github.com/penrose/penrose/issues/238) ([#540](https://github.com/penrose/penrose/issues/540)) ([133576b](https://github.com/penrose/penrose/commit/133576bdd837ec640f78d8c63981a1eed741721b))
- added multiple tick mark functionality to euclidean style ([#643](https://github.com/penrose/penrose/issues/643)) ([1ec90f5](https://github.com/penrose/penrose/commit/1ec90f5287c5c6c5486be7e1cc9ee4b91f87e9ea))
- added swap-in mutator ([#646](https://github.com/penrose/penrose/issues/646)) ([71b872b](https://github.com/penrose/penrose/commit/71b872be762cfe354afd47b6b87c9247b485c889))
- allow autodiff addends to be masked ([#1192](https://github.com/penrose/penrose/issues/1192)) ([2870b5f](https://github.com/penrose/penrose/commit/2870b5fd1706da5cf614ee211a3e7729dd0f872f))
- Allow convex partition on clockwise polygons ([#901](https://github.com/penrose/penrose/issues/901)) ([27a99ee](https://github.com/penrose/penrose/commit/27a99ee2e4816b77e7baaf83cdbaed9045a20e3a))
- allow cycles in partial layer orderings ([#760](https://github.com/penrose/penrose/issues/760)) ([004ba9e](https://github.com/penrose/penrose/commit/004ba9e1d31d6bc40f5362ed24241f2cfcea40fe))
- Allow Greek letters in style identifiers ([#804](https://github.com/penrose/penrose/issues/804)) ([fa49392](https://github.com/penrose/penrose/commit/fa49392522c4beadc7b6a4e46ad743bfdfc70559))
- Allow subtypes inline ([#735](https://github.com/penrose/penrose/issues/735)) ([e4a0e41](https://github.com/penrose/penrose/commit/e4a0e418a129fc2182b2f5c2b02939d2d0d3ce85))
- Allow SVG kebab passthroughs via map ([#806](https://github.com/penrose/penrose/issues/806)) ([08a0fc7](https://github.com/penrose/penrose/commit/08a0fc7a70b7d7115c0090aee90437f81aae0c65))
- basic symmetric predicates ([#1061](https://github.com/penrose/penrose/issues/1061)) ([80e0a61](https://github.com/penrose/penrose/commit/80e0a611951cec828dbec5f00b56795a34ddfe26))
- call `evalShapes` only twice to generate a computation graph ([#976](https://github.com/penrose/penrose/issues/976)) ([0ff28e5](https://github.com/penrose/penrose/commit/0ff28e56635c3a6a7d94526d4e728201901e3fd7))
- check existence and type of labels in Style selectors ([#777](https://github.com/penrose/penrose/issues/777)) ([9357e4e](https://github.com/penrose/penrose/commit/9357e4e20a7b4cab66e0059fb4d2549bde1d90c5))
- compute `Text` bounding box ([#829](https://github.com/penrose/penrose/issues/829)) ([8886074](https://github.com/penrose/penrose/commit/88860747560926c981459b1014f804babd343ed7))
- customizable arrowheads on both ends + new arrowhead styles ([#1140](https://github.com/penrose/penrose/issues/1140)) ([0f60f05](https://github.com/penrose/penrose/commit/0f60f050f9fbff1effa50d04d2d3b097b87d2b18))
- Define bounding box function for every shape ([#698](https://github.com/penrose/penrose/issues/698)) ([28226dd](https://github.com/penrose/penrose/commit/28226ddc9880a48dc2c8982bbb4b158bbd5d463e))
- detect and report cyclic assignments in Style ([#1147](https://github.com/penrose/penrose/issues/1147)) ([0f122fb](https://github.com/penrose/penrose/commit/0f122fb7ff2e5ab1df3d06d70c7bd06fe184834c))
- display errors in the `Simple` component ([#953](https://github.com/penrose/penrose/issues/953)) ([aa6209f](https://github.com/penrose/penrose/commit/aa6209f520b6a2cbdcfe1b80767b233d27d69867)), closes [#535](https://github.com/penrose/penrose/issues/535)
- docusaurus site ([#771](https://github.com/penrose/penrose/issues/771)) ([13396b2](https://github.com/penrose/penrose/commit/13396b298280f63a9161174bf6b585a20613334c))
- Editor Rewrite ([#992](https://github.com/penrose/penrose/issues/992)) ([91022fa](https://github.com/penrose/penrose/commit/91022fafdd45e6e5810bcb87448095a1d105bae5))
- enumerative search of Substance mutations ([#638](https://github.com/penrose/penrose/issues/638)) ([97db076](https://github.com/penrose/penrose/commit/97db07673c16970216d56ec8af360639351361da))
- faster matching ([#1072](https://github.com/penrose/penrose/issues/1072)) ([99c6383](https://github.com/penrose/penrose/commit/99c63837b534aab687b98f8864b27a176273b4e8))
- Graphics tweaks ([#843](https://github.com/penrose/penrose/issues/843)) ([c492e4a](https://github.com/penrose/penrose/commit/c492e4a1814e8f45bdcce7998ebd9d0bdfe43c72))
- hexadecimal color literals in Style ([#1114](https://github.com/penrose/penrose/issues/1114)) ([ce4cb51](https://github.com/penrose/penrose/commit/ce4cb51cdb22b67d01766bd744073f191cc0a262))
- Implement convex partitioning ([#877](https://github.com/penrose/penrose/issues/877)) ([d7968fb](https://github.com/penrose/penrose/commit/d7968fb5ceb90843a2935d515882a9f899fd6c73))
- improve `euclidean.sty` ([#1117](https://github.com/penrose/penrose/issues/1117)) ([3a94d6d](https://github.com/penrose/penrose/commit/3a94d6d57a8c9c0e4809a05ad2d2711d919349e0))
- increase coverage of geometry domain for use with textbook problems ([#633](https://github.com/penrose/penrose/issues/633)) ([04619a0](https://github.com/penrose/penrose/commit/04619a0eda32a3ad3627603120daa83ed5d4b2dd))
- increase test coverage for constraints ([#791](https://github.com/penrose/penrose/issues/791)) ([28a8cfd](https://github.com/penrose/penrose/commit/28a8cfd14b759462cf1688533c39cf30df1e350b))
- Lagrange bases ([#999](https://github.com/penrose/penrose/issues/999)) ([e4d0259](https://github.com/penrose/penrose/commit/e4d0259c69e4635911e14415eec5eb7d737ca93f))
- Line polygon constraints ([#810](https://github.com/penrose/penrose/issues/810)) ([7d5538b](https://github.com/penrose/penrose/commit/7d5538b05dbeb06d70ae2c0a360988d268302353))
- load function names from `core` for style autocomplete ([#692](https://github.com/penrose/penrose/issues/692)) ([e7eab14](https://github.com/penrose/penrose/commit/e7eab140a38ad9a4a0f379367d25539a14d57fdf))
- Make NotTypeConsIn... into ParseError ([#961](https://github.com/penrose/penrose/issues/961)) ([047ce4b](https://github.com/penrose/penrose/commit/047ce4bd65539e22bf4ae80c97822608f083225d))
- Make parse errors more readable ([#1080](https://github.com/penrose/penrose/issues/1080)) ([1a8b440](https://github.com/penrose/penrose/commit/1a8b4401fbe65702bc851ebe31f990b5857f3ffe))
- Make Penrose deterministic ([#864](https://github.com/penrose/penrose/issues/864)) ([baabbe6](https://github.com/penrose/penrose/commit/baabbe63cfee662eb1f97a0782ca3a1d609af4cd))
- Make SVGs "Penrose-editable" ([#1171](https://github.com/penrose/penrose/issues/1171)) ([edb5dc8](https://github.com/penrose/penrose/commit/edb5dc8d80fa86e762dfd4bf17d9a66e1d59a950))
- match metadata ([#1074](https://github.com/penrose/penrose/issues/1074)) ([3f09477](https://github.com/penrose/penrose/commit/3f0947795e975c33f7d8cfad0be467746221f005))
- Math functions ([#736](https://github.com/penrose/penrose/issues/736)) ([9ded71d](https://github.com/penrose/penrose/commit/9ded71d2294bf94cdfb68721b1a5c2496a4cf2c2))
- maximal and minimal objectives ([#852](https://github.com/penrose/penrose/issues/852)) ([c402b7f](https://github.com/penrose/penrose/commit/c402b7f44cddc3f1e078be2ea8df59ddabf41824))
- Minkowski penalties ([#648](https://github.com/penrose/penrose/issues/648)) ([f2b799f](https://github.com/penrose/penrose/commit/f2b799f6f003cd6ca65ffe07de29fe1479fb004f))
- Minkowski penalties for Ellipse-Ellipse ([#977](https://github.com/penrose/penrose/issues/977)) ([2be787c](https://github.com/penrose/penrose/commit/2be787c2ad879e1351492b0b9566dec110fb2694))
- Minkowski penalties for Ellipse-Polygon ([#911](https://github.com/penrose/penrose/issues/911)) ([2842be8](https://github.com/penrose/penrose/commit/2842be8196f78053699d9d4ec4b1b70d008d1152))
- Polygon contains ([#868](https://github.com/penrose/penrose/issues/868)) ([9b7ad99](https://github.com/penrose/penrose/commit/9b7ad9932ff7c9f0f61cade4a36514a1e5d16aad))
- port the SIGGRAPH Euclidean geometry example ([#693](https://github.com/penrose/penrose/issues/693)) ([25878c4](https://github.com/penrose/penrose/commit/25878c49a326be7cc59aa91fed7539f56102a6d7))
- Predicate aliasing ([#1066](https://github.com/penrose/penrose/issues/1066)) ([de83edf](https://github.com/penrose/penrose/commit/de83edf8de661c5529e92c05524d6a28d914702a)), closes [#623](https://github.com/penrose/penrose/issues/623)
- React component library ([#671](https://github.com/penrose/penrose/issues/671)) ([7f5977b](https://github.com/penrose/penrose/commit/7f5977b9c578b0a47c0d7b3643426d62319c93d7))
- renderer: Passthrough unknown properties to Svg output ([d3175c6](https://github.com/penrose/penrose/commit/d3175c6145c7234971188a69cdd3de0053e2db94))
- renderer/style: Passthrough unknown properties to Svg output ([#759](https://github.com/penrose/penrose/issues/759)) ([d6b4283](https://github.com/penrose/penrose/commit/d6b428306ecb13aff73b2833c57b00525beebda8))
- requires `forall` declarations ([#1073](https://github.com/penrose/penrose/issues/1073)) ([942b06f](https://github.com/penrose/penrose/commit/942b06f3d161077aa9602405fff2d2d02aee9fa2))
- resolve image paths in `@penrose/editor` ([#1018](https://github.com/penrose/penrose/issues/1018)) ([7bb69e6](https://github.com/penrose/penrose/commit/7bb69e6ecbec42b1f500067e7c77dcc92ac665fa))
- resolve paths for included SVGs ([#825](https://github.com/penrose/penrose/issues/825)) ([cedbf1b](https://github.com/penrose/penrose/commit/cedbf1b0f219f013a0c825e08007a2edc3b2c3bc))
- Signed distance functions for Penrose shapes ([#979](https://github.com/penrose/penrose/issues/979)) ([1a00e4c](https://github.com/penrose/penrose/commit/1a00e4c113c8e1e308612e41528af50665d7b194))
- Split Optimization Status Tab Into Constraints and Objectives ([#611](https://github.com/penrose/penrose/issues/611)) ([ab9eee9](https://github.com/penrose/penrose/commit/ab9eee988df39ff2afee1c8dfc11ab45b75bc7e3))
- staged diagram generation in automator ([#610](https://github.com/penrose/penrose/issues/610)) ([3de4a31](https://github.com/penrose/penrose/commit/3de4a31543ddac80ed24274fde66d9e84304daa1))
- Structural formula example ([#734](https://github.com/penrose/penrose/issues/734)) ([bb18a6f](https://github.com/penrose/penrose/commit/bb18a6f56c6881ef6aa7cc9395ecee43670e2655))
- Substance mutations as data + refactored program generator ([#601](https://github.com/penrose/penrose/issues/601)) ([da8f9e5](https://github.com/penrose/penrose/commit/da8f9e5cd53043095826f67d095c5cade1ea71fe))
- support layout stages in Style ([#1199](https://github.com/penrose/penrose/issues/1199)) ([d22602a](https://github.com/penrose/penrose/commit/d22602a7f31ce48c0c00a984efec5fa3622e63eb))
- support path lists in `layer` expressions ([#1111](https://github.com/penrose/penrose/issues/1111)) ([e1340e8](https://github.com/penrose/penrose/commit/e1340e837197964fff84811025a0e44005c952c5))
- Text baseline fix ([#875](https://github.com/penrose/penrose/issues/875)) ([eadf6a1](https://github.com/penrose/penrose/commit/eadf6a1a5b5186129dada31b5b3e3375266aa2e1))
- unify browser-ui and editor ([#1000](https://github.com/penrose/penrose/issues/1000)) ([3e7f647](https://github.com/penrose/penrose/commit/3e7f64729fb36ba7c735f0360dcc4f33fd04a49c))
- Use C-style syntax for Domain arglists ([#737](https://github.com/penrose/penrose/issues/737)) ([2af2447](https://github.com/penrose/penrose/commit/2af2447d936095f9770a51693cced5e6661946b2))
- Walk on spheres ([#1019](https://github.com/penrose/penrose/issues/1019)) ([a5d5da1](https://github.com/penrose/penrose/commit/a5d5da1b3e3eabf53360434b9bd6b806780d1eac))

### Performance Improvements

- Add a benchmark suite ([#921](https://github.com/penrose/penrose/issues/921)) ([9513462](https://github.com/penrose/penrose/commit/95134626d08b6e3f57c85eab9de44a2a6f07f726))
- improve performance of Autodiff ([#796](https://github.com/penrose/penrose/issues/796)) ([8bca6db](https://github.com/penrose/penrose/commit/8bca6dbee81e7bebd6fffa071b683658f04367da))
- improve performance of symbolic differentiation ([#840](https://github.com/penrose/penrose/issues/840)) ([7b5dd6a](https://github.com/penrose/penrose/commit/7b5dd6a3103268d11b70d6908f0e855484903225))
- port the optimizer to WebAssembly ([#1092](https://github.com/penrose/penrose/issues/1092)) ([768895a](https://github.com/penrose/penrose/commit/768895a3aac643095f0d139052fa8a139ce28cfb))

### Reverts

- Revert "feat: Walk on spheres (#1019)" (#1021) ([228746e](https://github.com/penrose/penrose/commit/228746ee7544e4cf69c84f7bf871f0c9d95edcc5)), closes [#1019](https://github.com/penrose/penrose/issues/1019) [#1021](https://github.com/penrose/penrose/issues/1021)

# [1.3.0](https://github.com/penrose/penrose/compare/v1.2.0...v1.3.0) (2021-06-24)

### Bug Fixes

- arrowheads incorporated in the path length ([#587](https://github.com/penrose/penrose/issues/587)) ([ba8ddf2](https://github.com/penrose/penrose/commit/ba8ddf2dab3e43d5bbfccdaf69042300b3a97562))
- type-change in synthesizer was adding improperly formed bind statements to AST ([#590](https://github.com/penrose/penrose/issues/590)) ([b3c50a6](https://github.com/penrose/penrose/commit/b3c50a69856c7646b76dbf6a730bee30787eb3d1))
- Unify BBox Computation in `Constraints.ts` and Make `Square` Rect-Like ([#582](https://github.com/penrose/penrose/issues/582)) ([cce0160](https://github.com/penrose/penrose/commit/cce01603f97dd1937467bca1e79a4d6c660cc2d2)), closes [#584](https://github.com/penrose/penrose/issues/584)

### Features

- add arc command to path shape object, implement tick marks for triangle congruency ([#603](https://github.com/penrose/penrose/issues/603)) ([b3190dd](https://github.com/penrose/penrose/commit/b3190ddb422d4c19aeae1d8a78af59186378cf78))
- Add callout shapes ([#556](https://github.com/penrose/penrose/issues/556)) ([6e3d5a2](https://github.com/penrose/penrose/commit/6e3d5a24eeba981ed857c936c81425da7a2bbc76))
- existential graph domain ([#600](https://github.com/penrose/penrose/issues/600)) ([115111d](https://github.com/penrose/penrose/commit/115111d2f6e8f3b9004c1669c3236f18c69c16ab))
- rename statement mutation added to synthesizer ([#578](https://github.com/penrose/penrose/issues/578)) ([dbf7106](https://github.com/penrose/penrose/commit/dbf710646bcaad9f0e3142388ae4759ca6b3a740))
- Style-Controlled Canvas Dimensions ([#589](https://github.com/penrose/penrose/issues/589)) ([9bf6901](https://github.com/penrose/penrose/commit/9bf6901c3e246bd00f2cab470aa17088595fbf77))
- type-change implemented for synthesizer ([#585](https://github.com/penrose/penrose/issues/585)) ([81ea4cd](https://github.com/penrose/penrose/commit/81ea4cd0237ee2b811e46cc14352abd8c29b353a))

# [1.2.0](https://github.com/penrose/penrose/compare/v1.1.0...v1.2.0) (2021-05-24)

### Bug Fixes

- handle Text GPIs with empty strings ([#553](https://github.com/penrose/penrose/issues/553)) ([0a5cfe1](https://github.com/penrose/penrose/commit/0a5cfe19c74c07607b8f5a47bea6e4f34f4cdf99))

### Features

- PathString GPI ([#549](https://github.com/penrose/penrose/issues/549)) ([2e9069d](https://github.com/penrose/penrose/commit/2e9069d7c14436226029b5e0ae2050b2dc6c205c))
- substance program synthesizer ([#551](https://github.com/penrose/penrose/issues/551)) ([09062ee](https://github.com/penrose/penrose/commit/09062eee7bd027396905958cf009305fcc8aa6f6))

# [1.1.0](https://github.com/penrose/penrose/compare/v1.0.0...v1.1.0) (2021-04-21)

### Bug Fixes

- [#481](https://github.com/penrose/penrose/issues/481) - wrong NS attribute ([#485](https://github.com/penrose/penrose/issues/485)) ([e6b4c7a](https://github.com/penrose/penrose/commit/e6b4c7ae252f6789c8145dbe0699a608f4f00490))
- [#520](https://github.com/penrose/penrose/issues/520) (moving types out, fixing soundness) ([#526](https://github.com/penrose/penrose/issues/526)) ([ba0abf3](https://github.com/penrose/penrose/commit/ba0abf3fb666beea4d6e85f60d9cf6840d668dba))
- [style-errors-2] fix style path validation code or remove unsupported style program features so LA example works (`findExpr` accesses lists) [#489](https://github.com/penrose/penrose/issues/489) ([#491](https://github.com/penrose/penrose/issues/491)) ([389f25c](https://github.com/penrose/penrose/commit/389f25c37d74a59155b8df23941a1ff9fc001012))
- add source loc to parser errors; report unexpected EOF ([#510](https://github.com/penrose/penrose/issues/510)) ([8555c84](https://github.com/penrose/penrose/commit/8555c84a3a94ddca5930e8d012168de75c68599d))
- handle CRLF in all parsers ([#500](https://github.com/penrose/penrose/issues/500)) ([da2238a](https://github.com/penrose/penrose/commit/da2238a0e44c380daa59ecf3e6ee6737a8a74d76))
- K&R style braces in GPI exprs ([#544](https://github.com/penrose/penrose/issues/544)) ([ef0ae12](https://github.com/penrose/penrose/commit/ef0ae12d6244ec7689a5e3ae9ad03829ebe61115))
- newlines in errors (wrap now) and newlines in mathjax ([#493](https://github.com/penrose/penrose/issues/493)) ([9fbe49f](https://github.com/penrose/penrose/commit/9fbe49f27cd90500f27e20bdb1010692795d171a))
- skip static check for access paths ([#495](https://github.com/penrose/penrose/issues/495)) ([ffeb55b](https://github.com/penrose/penrose/commit/ffeb55be7df0323e52e312edf17c0fb29b68517d))
- stroke-dasharray rendering ([5815cac](https://github.com/penrose/penrose/commit/5815cac5dc4079226bcc671b0214262138f669b9))
- vector initialization, as well as overriding elements of vector-typed shape properties ([#522](https://github.com/penrose/penrose/issues/522)) ([238145c](https://github.com/penrose/penrose/commit/238145ce1073804dc5ca6ab470b94e555fca3c3e)), closes [#501](https://github.com/penrose/penrose/issues/501) [#515](https://github.com/penrose/penrose/issues/515) [#501](https://github.com/penrose/penrose/issues/501) [#515](https://github.com/penrose/penrose/issues/515) [#515](https://github.com/penrose/penrose/issues/515) [#516](https://github.com/penrose/penrose/issues/516) [#521](https://github.com/penrose/penrose/issues/521) [#2](https://github.com/penrose/penrose/issues/2) [#504](https://github.com/penrose/penrose/issues/504)

### Features

- [layout] add `evalFns` for individual opt fns, and compile each opt function ([#527](https://github.com/penrose/penrose/issues/527)) ([3eec8eb](https://github.com/penrose/penrose/commit/3eec8ebe93870e9ab4ffd6fef26e1f3ac2995c05))
- `evalEnergy` API function ([#512](https://github.com/penrose/penrose/issues/512)) ([2830bf0](https://github.com/penrose/penrose/commit/2830bf0f30a69f53e4928cce0882747764ecaa6e))
- add `strokeDashArray` to all GPIs ([#531](https://github.com/penrose/penrose/issues/531)) ([629d5cc](https://github.com/penrose/penrose/commit/629d5cc469ae3c7ccc2f63c7935c37cc51ff8359))
- Add basic graph domain and more standard library functions for Style ([#502](https://github.com/penrose/penrose/issues/502)) ([859f3ae](https://github.com/penrose/penrose/commit/859f3aefed035136278cf92dc93aa493973125a5)), closes [#501](https://github.com/penrose/penrose/issues/501)
- add Polygon and Polyline to the shape library ([#539](https://github.com/penrose/penrose/issues/539)) ([50bd559](https://github.com/penrose/penrose/commit/50bd55988420569b0aef732b3fd5f336a2ec6121))
- Enhance opt inspector ([#537](https://github.com/penrose/penrose/issues/537)) ([fe05e5c](https://github.com/penrose/penrose/commit/fe05e5c4fbe1e26e10448eaadc1e7025cd86203a))
- Get disjoint AABB rect/line constraints to work; make autodiff more correct; add varying initialization to Style ([#538](https://github.com/penrose/penrose/issues/538)) ([f848970](https://github.com/penrose/penrose/commit/f848970907e0bd484095d6b1188f5fc851dfe4f2)), closes [#504](https://github.com/penrose/penrose/issues/504) [#504](https://github.com/penrose/penrose/issues/504) [#504](https://github.com/penrose/penrose/issues/504) [#496](https://github.com/penrose/penrose/issues/496) [#542](https://github.com/penrose/penrose/issues/542)
- mathtransform with jscodeshift ([#389](https://github.com/penrose/penrose/issues/389)) ([db17b27](https://github.com/penrose/penrose/commit/db17b278bebe04f70d8e8c8135189d403f67aa2e))
- opt tab for inspector; localStorage settings ([#534](https://github.com/penrose/penrose/issues/534)) ([b1e69cd](https://github.com/penrose/penrose/commit/b1e69cd19c0817a32cf74b1b62c6b02b09139a06))
- process prelude values in substance checker ([#533](https://github.com/penrose/penrose/issues/533)) ([2c143ac](https://github.com/penrose/penrose/commit/2c143ac39a5d3ef84ee8277457bfa3460d45d255))

# 1.0.0 (2021-02-15)

**Note:** Version bump only for package @penrose/core

# [1.0.0-alpha.3](https://github.com/penrose/penrose/compare/v1.0.0-alpha.2...v1.0.0-alpha.3) (2021-02-15)

**Note:** Version bump only for package @penrose/core

# [1.0.0-alpha.1](https://github.com/penrose/penrose/compare/v1.0.0-alpha.0...v1.0.0-alpha.1) (2021-02-15)

**Note:** Version bump only for package @penrose/core

## 1.0.1 (2021-01-28)

### Bug Fixes

- install script for roger ([2724feb](https://github.com/penrose/penrose/commit/2724feb19d5ff2c4697a8da563b91e330857091d))
