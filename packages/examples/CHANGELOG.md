# Change Log

All notable changes to this project will be documented in this file.
See [Conventional Commits](https://conventionalcommits.org) for commit guidelines.

## [v3.0.0](https://github.com/penrose/penrose/compare/v2.3.0...v3.0.0) (2023-07-14)

### :warning: BREAKING CHANGE

- combine `automator` and `roger` (#1387)
- consolidate shape types (#1337)
- more readable `core` language API (#1527)
- put each trio in its own JSON file (#1393)
- support non-trio examples in the registry (#1418)

### :rocket: New Feature

- Additional curve support ([#1503](https://github.com/penrose/penrose/issues/1503)) ([daa593d](https://github.com/penrose/penrose/commit/daa593d0342a6189038324e3e300855ed74fe478))
- Another curved graph style ([#1386](https://github.com/penrose/penrose/issues/1386)) ([44a9005](https://github.com/penrose/penrose/commit/44a90053db5757ccc52da86fd4a1db5fc81e1a96))
- Blobs ([#1388](https://github.com/penrose/penrose/issues/1388)) ([5c192ae](https://github.com/penrose/penrose/commit/5c192ae31d10ea9280e99a6cede0ffe6cc69668e))
- Expose random sampling in Style ([#1404](https://github.com/penrose/penrose/issues/1404)) ([2afb63c](https://github.com/penrose/penrose/commit/2afb63cbfae50a20654a6a7f55486b31aa51c0f4))
- Geometric Queries ([#1428](https://github.com/penrose/penrose/issues/1428)) ([092063a](https://github.com/penrose/penrose/commit/092063ae8e653ce1195bf6186c715d6b7c908775))
- Improve elastic energy ([#1384](https://github.com/penrose/penrose/issues/1384)) ([291e244](https://github.com/penrose/penrose/commit/291e24435c489dd8ead4827c30c9481998d2a403))
- Spectral graph ([#1487](https://github.com/penrose/penrose/issues/1487)) ([9a347ad](https://github.com/penrose/penrose/commit/9a347adf2c5a7689d8254bed92b123110ef0b057))
- Update curves ([#1533](https://github.com/penrose/penrose/issues/1533)) ([41655a4](https://github.com/penrose/penrose/commit/41655a4c5d74df3c12f1bd6278ffbaf8d84edf07))
- Walk on stars ([#1493](https://github.com/penrose/penrose/issues/1493)) ([1678a91](https://github.com/penrose/penrose/commit/1678a912c5535f2f9896d20a851f421fb75f669e))
- abstract shape parameters ([#1361](https://github.com/penrose/penrose/issues/1361)) ([627ee5c](https://github.com/penrose/penrose/commit/627ee5c3b350a205cd0e06b1ede9ce3fcfedb756))
- accurate `Equation` measurements ([#1445](https://github.com/penrose/penrose/issues/1445)) ([93d30f5](https://github.com/penrose/penrose/commit/93d30f5315aba02b0846d70185638fa65ca6960e))
- add multiple choice component to `synthesizer-ui` ([#1344](https://github.com/penrose/penrose/issues/1344)) ([8767e36](https://github.com/penrose/penrose/commit/8767e360ba8cf92210b1177ba2f07586d945d613))
- additional constructor declaration syntax in Domain and Substance ([#1472](https://github.com/penrose/penrose/issues/1472)) ([ff1a7d7](https://github.com/penrose/penrose/commit/ff1a7d70a643d0cc19d4f14c7b962207f28c46b7))
- allow namespace updates in Style ([#1379](https://github.com/penrose/penrose/issues/1379)) ([5991b84](https://github.com/penrose/penrose/commit/5991b848c8d7f9bc1e582892aeda5f692944b2a5))
- box-arrow style and computer architecture diagram ([#1492](https://github.com/penrose/penrose/issues/1492)) ([2c428c3](https://github.com/penrose/penrose/commit/2c428c3227e728fa6cda1d91aaf7ef00843eebe3))
- export `core` API functions for optimization specification and solving ([#1391](https://github.com/penrose/penrose/issues/1391)) ([71aa047](https://github.com/penrose/penrose/commit/71aa047cb276b5ec366a7893620d2250f3fd07f8))
- function as data ([#1352](https://github.com/penrose/penrose/issues/1352)) ([8df3229](https://github.com/penrose/penrose/commit/8df3229de606c912dd03ed4e766f30655fb13eca))
- function warnings ([#1498](https://github.com/penrose/penrose/issues/1498)) ([3e68a94](https://github.com/penrose/penrose/commit/3e68a94ab933a841d882ea26c26a58b5728629bf))
- homepage gallery component ([#1464](https://github.com/penrose/penrose/issues/1464)) ([130b2b7](https://github.com/penrose/penrose/commit/130b2b77904b926863c7348b7de085a792117ca9))
- show warnings in `editor` ([#1381](https://github.com/penrose/penrose/issues/1381)) ([31a59f9](https://github.com/penrose/penrose/commit/31a59f9fab695281beef2528f172a4764655481c))
- space curves ([#1399](https://github.com/penrose/penrose/issues/1399)) ([20e187d](https://github.com/penrose/penrose/commit/20e187dd4c0379b6975cec81ad55cf9f3e5a00f6))
- support non-trio examples in the registry ([#1418](https://github.com/penrose/penrose/issues/1418)) ([4fe953f](https://github.com/penrose/penrose/commit/4fe953fb26778e28debae7221de3f05ef30c4e2b))
- t-SNE Experiment ([#1453](https://github.com/penrose/penrose/issues/1453)) ([42bac33](https://github.com/penrose/penrose/commit/42bac33a28650d3f081c2bec3a32bb1be7419a69))
- timeline ([#1369](https://github.com/penrose/penrose/issues/1369)) ([71aaf38](https://github.com/penrose/penrose/commit/71aaf38314b7d2ce8bdd5c5e53e1d956a97fa347))
- updated examples visible in gallery ([#1505](https://github.com/penrose/penrose/issues/1505)) ([230d534](https://github.com/penrose/penrose/commit/230d5344502a1b6506f03614f8b74c2035f50f71))

### :bug: Bug Fix

- `style` passthrough in `core` renderer ([#1517](https://github.com/penrose/penrose/issues/1517)) ([18b9218](https://github.com/penrose/penrose/commit/18b9218b7cf1e64042176a04c8aa8a297ae817fb))
- async loading of solid examples in the registry ([#1484](https://github.com/penrose/penrose/issues/1484)) ([3234243](https://github.com/penrose/penrose/commit/3234243d2154bb567f45cffe1054395ccd0e034b))
- bad clipping on transformed shape ([#1495](https://github.com/penrose/penrose/issues/1495)) ([6ed899f](https://github.com/penrose/penrose/commit/6ed899f41230732572662b89cc601d25a889ca7b))
- broken Edgeworth geometry examples ([#1346](https://github.com/penrose/penrose/issues/1346)) ([0f31177](https://github.com/penrose/penrose/commit/0f31177f8711f78fd204c20ab232833ca1c53c93))
- separate type exports in core API and remove solid examples from registry ([#1482](https://github.com/penrose/penrose/issues/1482)) ([d3dd1f9](https://github.com/penrose/penrose/commit/d3dd1f9adb12ce7fef6038af8cf2b6c3741523b9))
- warnings in example diagrams ([#1522](https://github.com/penrose/penrose/issues/1522)) ([9abc3a2](https://github.com/penrose/penrose/commit/9abc3a2a8896668fcdc8e9af29b3719733a76fe2))

### :nail_care: Polish

- combine `automator` and `roger` ([#1387](https://github.com/penrose/penrose/issues/1387)) ([678c6e5](https://github.com/penrose/penrose/commit/678c6e528d20d6cbbfd3a04f1fcad656e72bdc6e))
- consolidate shape types ([#1337](https://github.com/penrose/penrose/issues/1337)) ([0d69c97](https://github.com/penrose/penrose/commit/0d69c9709d68f4dd4f8cc6a7773740fa6f872ccf))
- more readable `core` language API ([#1527](https://github.com/penrose/penrose/issues/1527)) ([22c8fc6](https://github.com/penrose/penrose/commit/22c8fc68f225974a353df244832b3b1c90e5f0e0))
- put each trio in its own JSON file ([#1393](https://github.com/penrose/penrose/issues/1393)) ([803d7fc](https://github.com/penrose/penrose/commit/803d7fc20199262f833c2f60606ed1b778c92b72))
- restore the 2D triangle mesh domain ([#1354](https://github.com/penrose/penrose/issues/1354)) ([a09d193](https://github.com/penrose/penrose/commit/a09d1939e5788e5f6587546fb6aee1f7dcc803a7))
- unify default strokeColor for outline shapes ([#1169](https://github.com/penrose/penrose/issues/1169)) ([a105c7c](https://github.com/penrose/penrose/commit/a105c7c6be43752bf23437fd3d347432597bf92e))

### :memo: Documentation

- fix the tutorial ([#1501](https://github.com/penrose/penrose/issues/1501)) ([60332b7](https://github.com/penrose/penrose/commit/60332b7b0418fbdf92bf61de0771f1b6c3ded355))
- homepage tweaks ([#1515](https://github.com/penrose/penrose/issues/1515)) ([0fa290e](https://github.com/penrose/penrose/commit/0fa290e3264c8c20c768a9ef509a768ee10d5030))
- start a blog ([#1325](https://github.com/penrose/penrose/issues/1325)) ([6669567](https://github.com/penrose/penrose/commit/6669567917464c72d5dd445a6def540b0d11da93))

### :house: Internal

- choose easier Hamilton circuit example ([#1349](https://github.com/penrose/penrose/issues/1349)) ([bc77c2a](https://github.com/penrose/penrose/commit/bc77c2adc3e8afd402d644ddbe7e6bf3b2f36704))
- split valence electrons into predicates ([#1347](https://github.com/penrose/penrose/issues/1347)) ([3aa67e9](https://github.com/penrose/penrose/commit/3aa67e963c1dbfc79a7c8047ab640c72fecc6cac))
- switch from jest to vitest ([#1406](https://github.com/penrose/penrose/issues/1406)) ([8ef8c77](https://github.com/penrose/penrose/commit/8ef8c778488b17eb0f02a62d1399e0b0337f5355))

## [v2.3.0](https://github.com/penrose/penrose/compare/v2.2.0...v2.3.0) (2023-03-14)

### :rocket: New Feature

- Add a function to compute closest points ([#1039](https://github.com/penrose/penrose/issues/1039)) ([c2c8fc6](https://github.com/penrose/penrose/commit/c2c8fc68804943abe3c62ac0d3738bac0e77a870))
- Add group-theory example to registry ([#1301](https://github.com/penrose/penrose/issues/1301)) ([115a609](https://github.com/penrose/penrose/commit/115a6094025347e525ebe33a2207a944df8aa837))
- Lewis structures Style ([#1320](https://github.com/penrose/penrose/issues/1320)) ([5411f35](https://github.com/penrose/penrose/commit/5411f35d06039b82a0e77786c24edec7117d7cdf))
- basic group shape ([#1294](https://github.com/penrose/penrose/issues/1294)) ([cf77bff](https://github.com/penrose/penrose/commit/cf77bffa38273368c489d26ef981975c5d07bf80))
- compute rect-line distance exactly ([#1332](https://github.com/penrose/penrose/issues/1332)) ([b86be31](https://github.com/penrose/penrose/commit/b86be314de09725447707de56039069a92a99f20))
- experimental example emulating 3D diagramming ([#1299](https://github.com/penrose/penrose/issues/1299)) ([171a1d7](https://github.com/penrose/penrose/commit/171a1d7f3823a268a5c974ebf695a98569d45684))
- matrix and vector operations in Style ([#1310](https://github.com/penrose/penrose/issues/1310)) ([70d190e](https://github.com/penrose/penrose/commit/70d190eeab2cac3f1c0afb4e760d067bceeb04bf))
- provide a `shapeDistance` function ([#1328](https://github.com/penrose/penrose/issues/1328)) ([76a91e5](https://github.com/penrose/penrose/commit/76a91e5186c7ae4628417e4e9d6f9df289412ede))
- show multiple diagram instances on a grid in `editor` ([#1287](https://github.com/penrose/penrose/issues/1287)) ([fbaf03c](https://github.com/penrose/penrose/commit/fbaf03c7b6c4f87cc628111ee080af76c65ef55e))
- triangle-mesh-3d example improvements ([#1300](https://github.com/penrose/penrose/issues/1300)) ([b308c36](https://github.com/penrose/penrose/commit/b308c368a3c18700e91b9e5af60d5eb488617bab))

### :bug: Bug Fix

- `inRange` implemented incorrectly ([#1297](https://github.com/penrose/penrose/issues/1297)) ([f692a44](https://github.com/penrose/penrose/commit/f692a44b9f55df419db8e0e19998e79cf19ac88c))
- avoid `EPS_DENOM` in core autodiff ([#1333](https://github.com/penrose/penrose/issues/1333)) ([db9f38b](https://github.com/penrose/penrose/commit/db9f38becbcb628eb9864b3ba7d0a7018e304c64))

### :house: Internal

- add Lewis structures examples to `synthesizer-ui` ([#1334](https://github.com/penrose/penrose/issues/1334)) ([2f1f624](https://github.com/penrose/penrose/commit/2f1f624118d2c48433c6f888075c8b279ff3e387))
- add graph examples to `synthesizer-ui` ([#1336](https://github.com/penrose/penrose/issues/1336)) ([3b5f964](https://github.com/penrose/penrose/commit/3b5f964f2d9ca0d619ce7291844c36dd181e4345))
- diagram some graphs ([#1317](https://github.com/penrose/penrose/issues/1317)) ([37acb1b](https://github.com/penrose/penrose/commit/37acb1bb78ad5bbdf4b8b8188e09b975a7838113))
- expand presets in `synthesizer-ui` ([#1149](https://github.com/penrose/penrose/issues/1149)) ([58c288a](https://github.com/penrose/penrose/commit/58c288a2ec5b124f008222e8c3807dfa550dcd6f))
- layout tweaks in `euclidean.style` ([#1335](https://github.com/penrose/penrose/issues/1335)) ([12363c9](https://github.com/penrose/penrose/commit/12363c94c1325ef6e47da157f1e82d77d05d73fd))

## [v2.2.0](https://github.com/penrose/penrose/compare/v2.1.1...v2.2.0) (2023-02-02)

### :rocket: New Feature

- Group theory ([#1276](https://github.com/penrose/penrose/issues/1276)) ([16c64fa](https://github.com/penrose/penrose/commit/16c64fa54748f6ed9d8fcdb4c03c766f553dd720))
- Group theory - multiplication table style ([#1277](https://github.com/penrose/penrose/issues/1277)) ([fea6d0b](https://github.com/penrose/penrose/commit/fea6d0b2e25f88fad54331610b4ac541677fc658))
- improve registry schema and loading ([#1212](https://github.com/penrose/penrose/issues/1212)) ([d6bbc30](https://github.com/penrose/penrose/commit/d6bbc302de494e08fa4ca0602ccfa29bdfcd65ae))
- inline comparison operators ([#1257](https://github.com/penrose/penrose/issues/1257)) ([b3c7c2f](https://github.com/penrose/penrose/commit/b3c7c2f0547a245ece5865d94184d04f7edf334e))
- support longer file extensions ([#1280](https://github.com/penrose/penrose/issues/1280)) ([6e83596](https://github.com/penrose/penrose/commit/6e835968280a784a91c4a2ca47a226516a3067d0))

### :bug: Bug Fix

- enforcing ordering in `collinearOrdered` constraint ([#1265](https://github.com/penrose/penrose/issues/1265)) ([2336b4b](https://github.com/penrose/penrose/commit/2336b4b2a567fa520219fef4768c6e0406c310d9))

### :house: Internal

- remove full moon trio ([#1259](https://github.com/penrose/penrose/issues/1259)) ([664595b](https://github.com/penrose/penrose/commit/664595bac0e849f326f9a3a2bc3fc0c41e085b39))

## [v2.1.1](https://github.com/penrose/penrose/compare/v2.1.0...v2.1.1) (2023-01-19)

**Note:** Version bump only for package @penrose/examples

# [2.1.0](https://github.com/penrose/penrose/compare/v2.0.0...v2.1.0) (2023-01-19)

### Features

- Functions/Constraints for curves ([#1206](https://github.com/penrose/penrose/issues/1206)) ([6edc412](https://github.com/penrose/penrose/commit/6edc412fadeb8c1cb813719ea1b189bd35fa7ecb))

# [2.0.0](https://github.com/penrose/penrose/compare/v1.3.0...v2.0.0) (2023-01-17)

### Bug Fixes

- Add Support for images with absolute URLs ([#1033](https://github.com/penrose/penrose/issues/1033)) ([03a9b03](https://github.com/penrose/penrose/commit/03a9b035b0ead1a28dd6980f58a6c42ceea165c5))
- delete `svg-flatten` ([#1208](https://github.com/penrose/penrose/issues/1208)) ([976ca77](https://github.com/penrose/penrose/commit/976ca770c8eae9b95d2d6f7b36937005bbac8bcf))
- Issue [#1023](https://github.com/penrose/penrose/issues/1023) Allow stroke on Equations ([#1026](https://github.com/penrose/penrose/issues/1026)) ([77e1f87](https://github.com/penrose/penrose/commit/77e1f870ccd02794a0ccdc2f1a9ffcf2e96be829))
- Local image resolution not working in IDE ([#1037](https://github.com/penrose/penrose/issues/1037)) ([c5220b4](https://github.com/penrose/penrose/commit/c5220b43a753a7e8972331f0a5253c3fe475c06b))
- multiple matching ([#1063](https://github.com/penrose/penrose/issues/1063)) ([eb0991b](https://github.com/penrose/penrose/commit/eb0991b87145a3547a1a3697e29b5a54619c4a96)), closes [#1064](https://github.com/penrose/penrose/issues/1064) [/github.com/penrose/penrose/issues/1064#issuecomment-1189228125](https://github.com//github.com/penrose/penrose/issues/1064/issues/issuecomment-1189228125)
- Reaction example Style ([#1125](https://github.com/penrose/penrose/issues/1125)) ([6b76d1e](https://github.com/penrose/penrose/commit/6b76d1e5eb96813ede1189f0dfdae82bc801871c))
- Rename the padding argument of the overlapping constraint to overlap and negate its semantic ([#1130](https://github.com/penrose/penrose/issues/1130)) ([28684f4](https://github.com/penrose/penrose/commit/28684f4b0040a567d40bda927c46b8c74b1b6af7))
- storybook examples and add exterior algebra to registry ([#1122](https://github.com/penrose/penrose/issues/1122)) ([261055e](https://github.com/penrose/penrose/commit/261055edcd31939fa62db97928b86868b9a5e656))

### Features

- Add Mobius transform example ([#1216](https://github.com/penrose/penrose/issues/1216)) ([43e7740](https://github.com/penrose/penrose/commit/43e774010301b8e7c594bad17433c21b66b747a6))
- basic symmetric predicates ([#1061](https://github.com/penrose/penrose/issues/1061)) ([80e0a61](https://github.com/penrose/penrose/commit/80e0a611951cec828dbec5f00b56795a34ddfe26))
- call `evalShapes` only twice to generate a computation graph ([#976](https://github.com/penrose/penrose/issues/976)) ([0ff28e5](https://github.com/penrose/penrose/commit/0ff28e56635c3a6a7d94526d4e728201901e3fd7))
- customizable arrowheads on both ends + new arrowhead styles ([#1140](https://github.com/penrose/penrose/issues/1140)) ([0f60f05](https://github.com/penrose/penrose/commit/0f60f050f9fbff1effa50d04d2d3b097b87d2b18))
- Fake 3d linear algebra examples ([#1058](https://github.com/penrose/penrose/issues/1058)) ([41d0c83](https://github.com/penrose/penrose/commit/41d0c830b82ab221af75b4adc2176bd71575d8f8))
- faster matching ([#1072](https://github.com/penrose/penrose/issues/1072)) ([99c6383](https://github.com/penrose/penrose/commit/99c63837b534aab687b98f8864b27a176273b4e8))
- hexadecimal color literals in Style ([#1114](https://github.com/penrose/penrose/issues/1114)) ([ce4cb51](https://github.com/penrose/penrose/commit/ce4cb51cdb22b67d01766bd744073f191cc0a262))
- host tutorial in online editor ([#1196](https://github.com/penrose/penrose/issues/1196)) ([4f361c9](https://github.com/penrose/penrose/commit/4f361c92de4544247722c931178c786a6546434e))
- Hypergraph example ([#998](https://github.com/penrose/penrose/issues/998)) ([15053e3](https://github.com/penrose/penrose/commit/15053e3c428d27ed16713895ddc551c10caefe0c))
- improve `euclidean.sty` ([#1117](https://github.com/penrose/penrose/issues/1117)) ([3a94d6d](https://github.com/penrose/penrose/commit/3a94d6d57a8c9c0e4809a05ad2d2711d919349e0))
- Lagrange bases ([#999](https://github.com/penrose/penrose/issues/999)) ([e4d0259](https://github.com/penrose/penrose/commit/e4d0259c69e4635911e14415eec5eb7d737ca93f))
- match metadata ([#1074](https://github.com/penrose/penrose/issues/1074)) ([3f09477](https://github.com/penrose/penrose/commit/3f0947795e975c33f7d8cfad0be467746221f005))
- Minkowski penalties for Ellipse-Ellipse ([#977](https://github.com/penrose/penrose/issues/977)) ([2be787c](https://github.com/penrose/penrose/commit/2be787c2ad879e1351492b0b9566dec110fb2694))
- Minkowski penalties for Ellipse-Polygon ([#911](https://github.com/penrose/penrose/issues/911)) ([2842be8](https://github.com/penrose/penrose/commit/2842be8196f78053699d9d4ec4b1b70d008d1152))
- Persistent homology example ([#1007](https://github.com/penrose/penrose/issues/1007)) ([299145b](https://github.com/penrose/penrose/commit/299145bfd7561ef20dfd7b422ea36775aea80883))
- port the SIGGRAPH Euclidean geometry example ([#693](https://github.com/penrose/penrose/issues/693)) ([25878c4](https://github.com/penrose/penrose/commit/25878c49a326be7cc59aa91fed7539f56102a6d7))
- Predicate aliasing ([#1066](https://github.com/penrose/penrose/issues/1066)) ([de83edf](https://github.com/penrose/penrose/commit/de83edf8de661c5529e92c05524d6a28d914702a)), closes [#623](https://github.com/penrose/penrose/issues/623)
- requires `forall` declarations ([#1073](https://github.com/penrose/penrose/issues/1073)) ([942b06f](https://github.com/penrose/penrose/commit/942b06f3d161077aa9602405fff2d2d02aee9fa2))
- Signed distance functions for Penrose shapes ([#979](https://github.com/penrose/penrose/issues/979)) ([1a00e4c](https://github.com/penrose/penrose/commit/1a00e4c113c8e1e308612e41528af50665d7b194))
- support layout stages in Style ([#1199](https://github.com/penrose/penrose/issues/1199)) ([d22602a](https://github.com/penrose/penrose/commit/d22602a7f31ce48c0c00a984efec5fa3622e63eb))
- support path lists in `layer` expressions ([#1111](https://github.com/penrose/penrose/issues/1111)) ([e1340e8](https://github.com/penrose/penrose/commit/e1340e837197964fff84811025a0e44005c952c5))
- unify browser-ui and editor ([#1000](https://github.com/penrose/penrose/issues/1000)) ([3e7f647](https://github.com/penrose/penrose/commit/3e7f64729fb36ba7c735f0360dcc4f33fd04a49c))
- Walk on spheres ([#1019](https://github.com/penrose/penrose/issues/1019)) ([a5d5da1](https://github.com/penrose/penrose/commit/a5d5da1b3e3eabf53360434b9bd6b806780d1eac))
- Walk on spheres ([#1022](https://github.com/penrose/penrose/issues/1022)) ([5863147](https://github.com/penrose/penrose/commit/58631478cadef46aa357aec4f7bf68fd5ff5d1ec))

### Performance Improvements

- port the optimizer to WebAssembly ([#1092](https://github.com/penrose/penrose/issues/1092)) ([768895a](https://github.com/penrose/penrose/commit/768895a3aac643095f0d139052fa8a139ce28cfb))

### Reverts

- Revert "feat: Walk on spheres (#1019)" (#1021) ([228746e](https://github.com/penrose/penrose/commit/228746ee7544e4cf69c84f7bf871f0c9d95edcc5)), closes [#1019](https://github.com/penrose/penrose/issues/1019) [#1021](https://github.com/penrose/penrose/issues/1021)
