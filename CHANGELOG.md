# Change Log

All notable changes to this project will be documented in this file.
See [Conventional Commits](https://conventionalcommits.org) for commit guidelines.

## [v2.3.0](https://github.com/penrose/penrose/compare/v2.2.0...v2.3.0) (2023-03-14)

### :rocket: New Feature

- Add a function to compute closest points ([#1039](https://github.com/penrose/penrose/issues/1039)) ([c2c8fc6](https://github.com/penrose/penrose/commit/c2c8fc68804943abe3c62ac0d3738bac0e77a870))
- Add group-theory example to registry ([#1301](https://github.com/penrose/penrose/issues/1301)) ([115a609](https://github.com/penrose/penrose/commit/115a6094025347e525ebe33a2207a944df8aa837))
- Lewis structures Style ([#1320](https://github.com/penrose/penrose/issues/1320)) ([5411f35](https://github.com/penrose/penrose/commit/5411f35d06039b82a0e77786c24edec7117d7cdf))
- added "ctrl+enter" binding for recompiling ([#1306](https://github.com/penrose/penrose/issues/1306)) ([d0472d1](https://github.com/penrose/penrose/commit/d0472d1306959f6244ebb64a64894cc0a3fd376b))
- basic group shape ([#1294](https://github.com/penrose/penrose/issues/1294)) ([cf77bff](https://github.com/penrose/penrose/commit/cf77bffa38273368c489d26ef981975c5d07bf80))
- compile diagrams in `editor` after detected changes in `roger` ([#1264](https://github.com/penrose/penrose/issues/1264)) ([5ec39dd](https://github.com/penrose/penrose/commit/5ec39ddf92859d653768ddaa088f36f1b522e1af))
- compute rect-line distance exactly ([#1332](https://github.com/penrose/penrose/issues/1332)) ([b86be31](https://github.com/penrose/penrose/commit/b86be314de09725447707de56039069a92a99f20))
- experimental example emulating 3D diagramming ([#1299](https://github.com/penrose/penrose/issues/1299)) ([171a1d7](https://github.com/penrose/penrose/commit/171a1d7f3823a268a5c974ebf695a98569d45684))
- matrix and vector operations in Style ([#1310](https://github.com/penrose/penrose/issues/1310)) ([70d190e](https://github.com/penrose/penrose/commit/70d190eeab2cac3f1c0afb4e760d067bceeb04bf))
- provide a `shapeDistance` function ([#1328](https://github.com/penrose/penrose/issues/1328)) ([76a91e5](https://github.com/penrose/penrose/commit/76a91e5186c7ae4628417e4e9d6f9df289412ede))
- show multiple diagram instances on a grid in `editor` ([#1287](https://github.com/penrose/penrose/issues/1287)) ([fbaf03c](https://github.com/penrose/penrose/commit/fbaf03c7b6c4f87cc628111ee080af76c65ef55e))
- triangle-mesh-3d example improvements ([#1300](https://github.com/penrose/penrose/issues/1300)) ([b308c36](https://github.com/penrose/penrose/commit/b308c368a3c18700e91b9e5af60d5eb488617bab))

### :bug: Bug Fix

- SVG overflow in `Simple` component ([#1321](https://github.com/penrose/penrose/issues/1321)) ([df119ac](https://github.com/penrose/penrose/commit/df119acad87250d0097eeda4f019238bf0d07743))
- `inRange` implemented incorrectly ([#1297](https://github.com/penrose/penrose/issues/1297)) ([f692a44](https://github.com/penrose/penrose/commit/f692a44b9f55df419db8e0e19998e79cf19ac88c))
- avoid `EPS_DENOM` in core autodiff ([#1333](https://github.com/penrose/penrose/issues/1333)) ([db9f38b](https://github.com/penrose/penrose/commit/db9f38becbcb628eb9864b3ba7d0a7018e304c64))
- github action node version ([#1318](https://github.com/penrose/penrose/issues/1318)) ([0d52677](https://github.com/penrose/penrose/commit/0d5267723665816019b84f5bbbe8088f613c2c35))
- improve performance of pseudoTopsort ([#1302](https://github.com/penrose/penrose/issues/1302)) ([60bc4b5](https://github.com/penrose/penrose/commit/60bc4b5505dec71bb52ff787e0696797748af057))
- nondeterminism in renderer ([#1316](https://github.com/penrose/penrose/issues/1316)) ([9795420](https://github.com/penrose/penrose/commit/97954202c60c2aab6a11af1694f652f8a3bb8e4d))
- render shapes in order for determinism ([#1323](https://github.com/penrose/penrose/issues/1323)) ([c479eec](https://github.com/penrose/penrose/commit/c479eecdf4c4cbaa97c075bfd3608073dd576279))

### :memo: Documentation

- guide for creating new releases ([#1304](https://github.com/penrose/penrose/issues/1304)) ([1895d8f](https://github.com/penrose/penrose/commit/1895d8febdf4ced698bd1a478c3f3eaf1452b511))
- move wiki ([#1331](https://github.com/penrose/penrose/issues/1331)) ([062e8ed](https://github.com/penrose/penrose/commit/062e8edad60249db71238d5a72ff99d9e1cc8239))
- update Team page ([#1324](https://github.com/penrose/penrose/issues/1324)) ([e61e0ca](https://github.com/penrose/penrose/commit/e61e0ca86546185717cf4b5aff6215c6dc782da8))

### :house: Internal

- add Lewis structures examples to `synthesizer-ui` ([#1334](https://github.com/penrose/penrose/issues/1334)) ([2f1f624](https://github.com/penrose/penrose/commit/2f1f624118d2c48433c6f888075c8b279ff3e387))
- add graph examples to `synthesizer-ui` ([#1336](https://github.com/penrose/penrose/issues/1336)) ([3b5f964](https://github.com/penrose/penrose/commit/3b5f964f2d9ca0d619ce7291844c36dd181e4345))
- add more stuff to `.prettierignore` ([#1339](https://github.com/penrose/penrose/issues/1339)) ([e3a3f34](https://github.com/penrose/penrose/commit/e3a3f34f7e9e3a387393e068767232fa36a24a1a))
- add optimizer `build/` to `.prettierignore` ([#1315](https://github.com/penrose/penrose/issues/1315)) ([3c8b424](https://github.com/penrose/penrose/commit/3c8b424f9042800be68f8ad21404a66c846af2d5))
- diagram some graphs ([#1317](https://github.com/penrose/penrose/issues/1317)) ([37acb1b](https://github.com/penrose/penrose/commit/37acb1bb78ad5bbdf4b8b8188e09b975a7838113))
- expand presets in `synthesizer-ui` ([#1149](https://github.com/penrose/penrose/issues/1149)) ([58c288a](https://github.com/penrose/penrose/commit/58c288a2ec5b124f008222e8c3807dfa550dcd6f))
- layout tweaks in `euclidean.style` ([#1335](https://github.com/penrose/penrose/issues/1335)) ([12363c9](https://github.com/penrose/penrose/commit/12363c94c1325ef6e47da157f1e82d77d05d73fd))
- remove ESLint from PR template checklist ([#1330](https://github.com/penrose/penrose/issues/1330)) ([ef74877](https://github.com/penrose/penrose/commit/ef748779359f8da04a2baf95036d67274c44929a))

## [v2.2.0](https://github.com/penrose/penrose/compare/v2.1.1...v2.2.0) (2023-02-02)

### :rocket: New Feature

- Add link to Wiki ([#1275](https://github.com/penrose/penrose/issues/1275)) ([5c2f368](https://github.com/penrose/penrose/commit/5c2f368ead39c8b28af087350153e1228f5a8531))
- Group theory ([#1276](https://github.com/penrose/penrose/issues/1276)) ([16c64fa](https://github.com/penrose/penrose/commit/16c64fa54748f6ed9d8fcdb4c03c766f553dd720))
- Group theory - multiplication table style ([#1277](https://github.com/penrose/penrose/issues/1277)) ([fea6d0b](https://github.com/penrose/penrose/commit/fea6d0b2e25f88fad54331610b4ac541677fc658))
- improve registry schema and loading ([#1212](https://github.com/penrose/penrose/issues/1212)) ([d6bbc30](https://github.com/penrose/penrose/commit/d6bbc302de494e08fa4ca0602ccfa29bdfcd65ae))
- inline comparison operators ([#1257](https://github.com/penrose/penrose/issues/1257)) ([b3c7c2f](https://github.com/penrose/penrose/commit/b3c7c2f0547a245ece5865d94184d04f7edf334e))
- support longer file extensions ([#1280](https://github.com/penrose/penrose/issues/1280)) ([6e83596](https://github.com/penrose/penrose/commit/6e835968280a784a91c4a2ca47a226516a3067d0))

### :bug: Bug Fix

- enforcing ordering in `collinearOrdered` constraint ([#1265](https://github.com/penrose/penrose/issues/1265)) ([2336b4b](https://github.com/penrose/penrose/commit/2336b4b2a567fa520219fef4768c6e0406c310d9))

### :nail_care: Polish

- bring VS Code extension into this repo ([#1271](https://github.com/penrose/penrose/issues/1271)) ([a74a7e2](https://github.com/penrose/penrose/commit/a74a7e2746069c5b6c5c657b84a24f3d2b9bf897))

### :memo: Documentation

- fix `import` typo in components README ([#1253](https://github.com/penrose/penrose/issues/1253)) ([05b1f68](https://github.com/penrose/penrose/commit/05b1f68a80a3fc36868e226038f450ae8cd65cf5))

### :house: Internal

- clarify a couple `Graph` method docstrings ([#1285](https://github.com/penrose/penrose/issues/1285)) ([6da46aa](https://github.com/penrose/penrose/commit/6da46aa79599f29a69301ea777be16ff8bfb616e))
- fix Twitter badge in README ([#1260](https://github.com/penrose/penrose/issues/1260)) ([e60b3ed](https://github.com/penrose/penrose/commit/e60b3ed6e6635f76d00e8e2920edcb6abb3fb462))
- remove full moon trio ([#1259](https://github.com/penrose/penrose/issues/1259)) ([664595b](https://github.com/penrose/penrose/commit/664595bac0e849f326f9a3a2bc3fc0c41e085b39))

## [v2.1.1](https://github.com/penrose/penrose/compare/v2.1.0...v2.1.1) (2023-01-19)

### :bug: Bug Fix

- make pandemonium a dependency of core ([#1249](https://github.com/penrose/penrose/issues/1249)) ([aac81e8](https://github.com/penrose/penrose/commit/aac81e856182a246c7c1dff96aed91bf7e260b1e))
- symmetric predicate check runs on empty type graph ([#1248](https://github.com/penrose/penrose/issues/1248)) ([2493c51](https://github.com/penrose/penrose/commit/2493c51fd5ddbbd690387a13cb777e4ca723f393))

### :memo: Documentation

- update `core` usage in README ([#1250](https://github.com/penrose/penrose/issues/1250)) ([cb1781f](https://github.com/penrose/penrose/commit/cb1781f7ec26ea7692e874af170eb0fdc8b85e1d))

### :house: Internal

- include perf in changelogs ([#1247](https://github.com/penrose/penrose/issues/1247)) ([4cd62e1](https://github.com/penrose/penrose/commit/4cd62e140809ce023c2b6709947a8e42b6c51e44))

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
- `draw` command in `@penrose/roger` ([#937](https://github.com/penrose/penrose/issues/937)) ([261fd4c](https://github.com/penrose/penrose/commit/261fd4cb12f342555572aa40cfc48ffee58dbbfd))
- `ReferenceError` in storybook ([#1228](https://github.com/penrose/penrose/issues/1228)) ([74a0440](https://github.com/penrose/penrose/commit/74a0440be73053a8fb1105b810308254b2957663))
- Absolute imports for panels ([#685](https://github.com/penrose/penrose/issues/685)) ([bb0737e](https://github.com/penrose/penrose/commit/bb0737ef3942c1cace308b75bc8aef2a813d0ca1))
- account for descent in `Text` center computation ([#842](https://github.com/penrose/penrose/issues/842)) ([d9999eb](https://github.com/penrose/penrose/commit/d9999eb7ffa538b20d264f255782c53d52ffe004))
- add duplicate name check in Substance checker ([#657](https://github.com/penrose/penrose/issues/657)) ([1a3df91](https://github.com/penrose/penrose/commit/1a3df9134385f4b7fef31e9174b34a64f7852cbe))
- add editor as explicit dep ([#996](https://github.com/penrose/penrose/issues/996)) ([ce1ee31](https://github.com/penrose/penrose/commit/ce1ee318ee91e5d5f5d8ccd364b1cbbc7dcf42d1))
- add examples to build ([#994](https://github.com/penrose/penrose/issues/994)) ([e9d2ac9](https://github.com/penrose/penrose/commit/e9d2ac95064debe7979933252f8b2c04c82147f6))
- add labels to unmatched Substance objects in the translation ([#666](https://github.com/penrose/penrose/issues/666)) ([15462a6](https://github.com/penrose/penrose/commit/15462a6d3c56ad4cf2d37369c49eef664f5b4e88))
- Add Support for images with absolute URLs ([#1033](https://github.com/penrose/penrose/issues/1033)) ([03a9b03](https://github.com/penrose/penrose/commit/03a9b035b0ead1a28dd6980f58a6c42ceea165c5))
- Allow Docusaurus and IDE co-existence ([#916](https://github.com/penrose/penrose/issues/916)) ([d8aa65f](https://github.com/penrose/penrose/commit/d8aa65f6e681f9e1fdca33417c06cb6fc1b0e978))
- roger NPE in staged mode. Fixed [#887](https://github.com/penrose/penrose/issues/887) ([#946](https://github.com/penrose/penrose/issues/946)) ([dc50785](https://github.com/penrose/penrose/commit/dc507855ac6d18fc7033c1df7f75efcc181e20c4))
- avoid 404 in homepage try link ([#1187](https://github.com/penrose/penrose/issues/1187)) ([9da7193](https://github.com/penrose/penrose/commit/9da7193e795313980dd1505805909d553c9a4bac))
- base url ([#997](https://github.com/penrose/penrose/issues/997)) ([d329dd0](https://github.com/penrose/penrose/commit/d329dd067b82b182c2eab1a6436fc8b3657b1975))
- build `@penrose/core` before deploying storybook ([a87cc23](https://github.com/penrose/penrose/commit/a87cc23b1eea5f30f53a7489223ecd5447a9075a))
- bump pug version ([#669](https://github.com/penrose/penrose/issues/669)) ([633f101](https://github.com/penrose/penrose/commit/633f101aa505edca938897afa536f0c2a1b61885))
- calls correct node navigation methods ([#807](https://github.com/penrose/penrose/issues/807)) ([4a94cac](https://github.com/penrose/penrose/commit/4a94cac24d4d33d37e242e444e7d3ef4df6f8998))
- canvas disappears on mobile browsers ([#1136](https://github.com/penrose/penrose/issues/1136)) ([a060858](https://github.com/penrose/penrose/commit/a060858a2f79a5da93c12421e258db33b36a4d22))
- Catch errors thrown in `core` in `browser-ui` ([#625](https://github.com/penrose/penrose/issues/625)) ([2a2ca45](https://github.com/penrose/penrose/commit/2a2ca454be27a72b36846f24dd89fd94d8941603))
- Change the derivative of abs to be sign ([#1104](https://github.com/penrose/penrose/issues/1104)) ([2cb933f](https://github.com/penrose/penrose/commit/2cb933f567501ff58268ce9def74781c7c4c803b))
- check constructor name match in Style selector ([#757](https://github.com/penrose/penrose/issues/757)) ([3ab0042](https://github.com/penrose/penrose/commit/3ab0042f7603d000d170643fe93bb31e4c80081f))
- Circle CI OOM error when building [#651](https://github.com/penrose/penrose/issues/651) ([#655](https://github.com/penrose/penrose/issues/655)) ([a50ccae](https://github.com/penrose/penrose/commit/a50ccaeb1201872c2985e45aedd93fdb798cfadd))
- circle-rectangle interactions ([#848](https://github.com/penrose/penrose/issues/848)) ([428cad6](https://github.com/penrose/penrose/commit/428cad66840054378a736bd948acbdbcd15d4bbc))
- circleci resource class and storybook trigger ([50e808b](https://github.com/penrose/penrose/commit/50e808b0e7c77dfbb5f6e61c71eb0a1c9636aadd))
- clone rendered SVG node for `Equation` at render-time ([#1144](https://github.com/penrose/penrose/issues/1144)) ([47b6dd2](https://github.com/penrose/penrose/commit/47b6dd217ab06426b8022289f4933b9cf0ff9d00))
- Cos/sin shouldn't use degrees [#374](https://github.com/penrose/penrose/issues/374) ([#677](https://github.com/penrose/penrose/issues/677)) ([d65ac8d](https://github.com/penrose/penrose/commit/d65ac8d61ed6403bcd2965e7af1fbdd903ad5beb)), closes [#651](https://github.com/penrose/penrose/issues/651)
- Default fill xor stroke. FreeformPolygon stack dumps renderer. Closes [#704](https://github.com/penrose/penrose/issues/704), Closes [#706](https://github.com/penrose/penrose/issues/706), Closes [#708](https://github.com/penrose/penrose/issues/708) ([#707](https://github.com/penrose/penrose/issues/707)) ([7662137](https://github.com/penrose/penrose/commit/766213755b7a4762da510b20f45a409943006c58)), closes [#651](https://github.com/penrose/penrose/issues/651) [#374](https://github.com/penrose/penrose/issues/374) [#392](https://github.com/penrose/penrose/issues/392) [#392](https://github.com/penrose/penrose/issues/392) [#392](https://github.com/penrose/penrose/issues/392) [#392](https://github.com/penrose/penrose/issues/392) [#392](https://github.com/penrose/penrose/issues/392)
- delete `svg-flatten` ([#1208](https://github.com/penrose/penrose/issues/1208)) ([976ca77](https://github.com/penrose/penrose/commit/976ca770c8eae9b95d2d6f7b36937005bbac8bcf))
- Delete packages/browser-ui/src/contexts.tsx ([#912](https://github.com/penrose/penrose/issues/912)) ([ffd63d0](https://github.com/penrose/penrose/commit/ffd63d022a825a96795b5ef5e982200801b40159))
- determine `pointer-event` for dragging based on shape kind ([#686](https://github.com/penrose/penrose/issues/686)) ([c3c0cc3](https://github.com/penrose/penrose/commit/c3c0cc3fa59ad0dfed41b2ec462644cd4531b220))
- docs site mkdir ([#995](https://github.com/penrose/penrose/issues/995)) ([c149e59](https://github.com/penrose/penrose/commit/c149e596298230e252418f15d0d20edebf27f29b))
- Docs-site Shape Property page errors and crashes ([#1045](https://github.com/penrose/penrose/issues/1045)) ([880d197](https://github.com/penrose/penrose/commit/880d197aa37290a43cccad9966f313f2d521b32f))
- empty label check in Style selector ([#789](https://github.com/penrose/penrose/issues/789)) ([f28c1ba](https://github.com/penrose/penrose/commit/f28c1ba80665e36fc0f10a23383e21b2ad28520d))
- Fix [#935](https://github.com/penrose/penrose/issues/935) roger fails in draw mode ([#936](https://github.com/penrose/penrose/issues/936)) ([5b3d464](https://github.com/penrose/penrose/commit/5b3d46459c95f9ed68c7f80e3f9219a6171c5008))
- GitHub Actions canary release ([#778](https://github.com/penrose/penrose/issues/778)) ([7f8f093](https://github.com/penrose/penrose/commit/7f8f093078694b720acaf3de8f5f78bb4656a097))
- GitHub Actions canary release ([#780](https://github.com/penrose/penrose/issues/780)) ([00681d5](https://github.com/penrose/penrose/commit/00681d50ee44421e49943f6d0b93ca1975336f13))
- Give Vite 8 GiB in editor ([#1090](https://github.com/penrose/penrose/issues/1090)) ([3e328c8](https://github.com/penrose/penrose/commit/3e328c81f1843866aac89ebc947b463c08207b80))
- Issue [#1023](https://github.com/penrose/penrose/issues/1023) Allow stroke on Equations ([#1026](https://github.com/penrose/penrose/issues/1026)) ([77e1f87](https://github.com/penrose/penrose/commit/77e1f870ccd02794a0ccdc2f1a9ffcf2e96be829))
- issue [#1024](https://github.com/penrose/penrose/issues/1024) exclude name, ensureOnCavas in SVG ([#1025](https://github.com/penrose/penrose/issues/1025)) ([ed7bb4d](https://github.com/penrose/penrose/commit/ed7bb4d99eb16468193fac1eef58df5be193e294))
- Local image resolution not working in IDE ([#1037](https://github.com/penrose/penrose/issues/1037)) ([c5220b4](https://github.com/penrose/penrose/commit/c5220b43a753a7e8972331f0a5253c3fe475c06b))
- Make autodiff deterministic in graph shape ([#945](https://github.com/penrose/penrose/issues/945)) ([c6fe4e3](https://github.com/penrose/penrose/commit/c6fe4e33cf148c6c85f887deb7466468376bba87))
- Make Roger allow unordered triples ([#658](https://github.com/penrose/penrose/issues/658)) ([2d8f90e](https://github.com/penrose/penrose/commit/2d8f90e6c6b7db1984c84c57ea94dc0fd396f974))
- Make TypeDecl subType range more precise ([#963](https://github.com/penrose/penrose/issues/963)) ([a7811dd](https://github.com/penrose/penrose/commit/a7811dd500ff95a69facc57edee09d9a7806d578))
- make website routing work ([#1195](https://github.com/penrose/penrose/issues/1195)) ([08f4f61](https://github.com/penrose/penrose/commit/08f4f615bab43c70516548cc275d9ef53b2b5ac6))
- map `Option::None` to `null`, not `undefined` ([#1191](https://github.com/penrose/penrose/issues/1191)) ([cd195e9](https://github.com/penrose/penrose/commit/cd195e9284c9624573aa88afa98b98ad0fd344f0))
- multiple matching ([#1063](https://github.com/penrose/penrose/issues/1063)) ([eb0991b](https://github.com/penrose/penrose/commit/eb0991b87145a3547a1a3697e29b5a54619c4a96)), closes [#1064](https://github.com/penrose/penrose/issues/1064) [/github.com/penrose/penrose/issues/1064#issuecomment-1189228125](https://github.com//github.com/penrose/penrose/issues/1064/issues/issuecomment-1189228125)
- path resolution in `roger` ([#836](https://github.com/penrose/penrose/issues/836)) ([52972af](https://github.com/penrose/penrose/commit/52972af9550fdb4ca4e2ffc6f19f99a10e0a231a))
- PointcareDisk.sty missing canvas def ([#851](https://github.com/penrose/penrose/issues/851)) ([b20400a](https://github.com/penrose/penrose/commit/b20400ad9a6d6b46d9e00ef1f84281f6755b01c8))
- Reaction example Style ([#1125](https://github.com/penrose/penrose/issues/1125)) ([6b76d1e](https://github.com/penrose/penrose/commit/6b76d1e5eb96813ede1189f0dfdae82bc801871c))
- Remove duplicate 'pow' conditional ([#880](https://github.com/penrose/penrose/issues/880)) ([033a259](https://github.com/penrose/penrose/commit/033a2592c38132c2b479c70f76a9f909a9b31a9e))
- remove error wrapper in `browser-ui` ([#695](https://github.com/penrose/penrose/issues/695)) ([5a194aa](https://github.com/penrose/penrose/commit/5a194aac21349c5a8aafeb5e0d87144274d53b04))
- Remove Rectangle Rotation BBox logic ([#803](https://github.com/penrose/penrose/issues/803)) ([2ea92aa](https://github.com/penrose/penrose/commit/2ea92aaa86fcb4c30dfaad671937f02092463ff1))
- remove reference to disambiguateFunctions ([#940](https://github.com/penrose/penrose/issues/940)) ([a6be39f](https://github.com/penrose/penrose/commit/a6be39f6a39271bdd7aab1dec9d52f352463416d))
- Rename the padding argument of the overlapping constraint to overlap and negate its semantic ([#1130](https://github.com/penrose/penrose/issues/1130)) ([28684f4](https://github.com/penrose/penrose/commit/28684f4b0040a567d40bda927c46b8c74b1b6af7))
- repel objective on segments causes slow optimization time ([#613](https://github.com/penrose/penrose/issues/613)) ([cfc8f46](https://github.com/penrose/penrose/commit/cfc8f46d07450d189aa94b08635de78587d7b759))
- Replace Substance undefined symbol usage ([#745](https://github.com/penrose/penrose/issues/745)) ([7587ce6](https://github.com/penrose/penrose/commit/7587ce62bd71510352dcb4d91e23b0c253f591be))
- Resample button in panes ([#881](https://github.com/penrose/penrose/issues/881)) ([c1626cf](https://github.com/penrose/penrose/commit/c1626cfdc7936fe7d4c0e6d7fcba9688f346313a))
- resize behaviors of diagram panel components ([#1105](https://github.com/penrose/penrose/issues/1105)) ([7874667](https://github.com/penrose/penrose/commit/787466793507105b34674cfe1d6e637c160db4ae))
- Resolve browser-ui build warnings ([#839](https://github.com/penrose/penrose/issues/839)) ([21425aa](https://github.com/penrose/penrose/commit/21425aab6ed338428f3ee2c02eb1c942182a7008))
- resolved NaN in the nested.sub by increasing the max size of sets shapes [#498](https://github.com/penrose/penrose/issues/498) ([#628](https://github.com/penrose/penrose/issues/628)) ([dcce355](https://github.com/penrose/penrose/commit/dcce35571f585602fb5bb97dfe73bdeb0dc1f457))
- responsive tab layout on mobile ([#1137](https://github.com/penrose/penrose/issues/1137)) ([95b7f3e](https://github.com/penrose/penrose/commit/95b7f3eac44b1ee29d61d88a071be94eea33780a))
- return types in exported functions ([#637](https://github.com/penrose/penrose/issues/637)) ([944eb01](https://github.com/penrose/penrose/commit/944eb01ecf2dcd8b1b233c712921ec3fd6abe905))
- selector matching `n^m` complexity ([#1016](https://github.com/penrose/penrose/issues/1016)) ([930fa0d](https://github.com/penrose/penrose/commit/930fa0d2be74e2ad87e237131ac4f7fba7c27975))
- Set paint none=none() in style examples. Closes [#703](https://github.com/penrose/penrose/issues/703) ([#705](https://github.com/penrose/penrose/issues/705)) ([f631d57](https://github.com/penrose/penrose/commit/f631d57f5b3ea7250600593778eaa89158e7a98c)), closes [#651](https://github.com/penrose/penrose/issues/651) [#374](https://github.com/penrose/penrose/issues/374) [#392](https://github.com/penrose/penrose/issues/392) [#392](https://github.com/penrose/penrose/issues/392) [#392](https://github.com/penrose/penrose/issues/392) [#392](https://github.com/penrose/penrose/issues/392) [#392](https://github.com/penrose/penrose/issues/392)
- some symmetric predicates don't match ([#1127](https://github.com/penrose/penrose/issues/1127)) ([b1f35e8](https://github.com/penrose/penrose/commit/b1f35e88644bec21ad09e023afabc59425098c5b))
- storybook deployment base directory ([#1124](https://github.com/penrose/penrose/issues/1124)) ([7fb7a01](https://github.com/penrose/penrose/commit/7fb7a013d01a9fc1d40310b2a8505f99d7ea9468))
- storybook examples and add exterior algebra to registry ([#1122](https://github.com/penrose/penrose/issues/1122)) ([261055e](https://github.com/penrose/penrose/commit/261055edcd31939fa62db97928b86868b9a5e656))
- Strengthen length check in zip3 ([#915](https://github.com/penrose/penrose/issues/915)) ([36c0867](https://github.com/penrose/penrose/commit/36c08679d355eddbaecf02b41d69efb8bdb8caa7))
- strokeDasharray strokeLinecap consistency ([#850](https://github.com/penrose/penrose/issues/850)) ([c7bf34b](https://github.com/penrose/penrose/commit/c7bf34b2da79ce22fd1490a617c00040bc5a33ab))
- Support empty fill and stroke style [#392](https://github.com/penrose/penrose/issues/392) ([#699](https://github.com/penrose/penrose/issues/699)) ([ee28a6f](https://github.com/penrose/penrose/commit/ee28a6f5e3f81894ea093377b1edea2a765f9731)), closes [#651](https://github.com/penrose/penrose/issues/651) [#374](https://github.com/penrose/penrose/issues/374)
- symmetry in nested predicates ([#1069](https://github.com/penrose/penrose/issues/1069)) ([7d0d4bf](https://github.com/penrose/penrose/commit/7d0d4bfc78e1e5592a4c36e5a231af9517dacaca)), closes [#1068](https://github.com/penrose/penrose/issues/1068)
- synthesizer-ui typescript build warnings ([#846](https://github.com/penrose/penrose/issues/846)) ([2b00441](https://github.com/penrose/penrose/commit/2b0044109982ebc1354bfa050e747c8ceeb62931))
- Throw error on invalid labels ([#663](https://github.com/penrose/penrose/issues/663)) ([96772ca](https://github.com/penrose/penrose/commit/96772ca0f7716bb915f244d60871e247363ef690))
- toHex usage ([#746](https://github.com/penrose/penrose/issues/746)) ([847c033](https://github.com/penrose/penrose/commit/847c033472f913ad7446d4df588411bfd6b05c9e))
- top-level diagram function ([#862](https://github.com/penrose/penrose/issues/862)) ([7f0f727](https://github.com/penrose/penrose/commit/7f0f7275c7727aec74fd3fbc1563d98220811e66))
- transform polygon/polyline points to screen space ([#849](https://github.com/penrose/penrose/issues/849)) ([861447f](https://github.com/penrose/penrose/commit/861447f66e299e1b7697f4accff245fe52be6a6e))
- twosets-simple example [#616](https://github.com/penrose/penrose/issues/616) ([#617](https://github.com/penrose/penrose/issues/617)) ([184ac34](https://github.com/penrose/penrose/commit/184ac34cb61e5f79a8bc51225a77a7b23de740a9))
- unwrap the result of `stepUntilConvergence` in `browser-ui` ([#634](https://github.com/penrose/penrose/issues/634)) ([e5796fb](https://github.com/penrose/penrose/commit/e5796fb2fba1217fcab85ec08b55a260efea0d03))
- update examples in storybook ([#790](https://github.com/penrose/penrose/issues/790)) ([3c968b9](https://github.com/penrose/penrose/commit/3c968b90e77017c55fa43db02bd3a3dd874cb047))
- Upgrade TypeScript for roger prepack ([#781](https://github.com/penrose/penrose/issues/781)) ([b89b4af](https://github.com/penrose/penrose/commit/b89b4afc60c781876bbb821124651507275211f2))
- use empty string as the default label and check autolabel statements ([#754](https://github.com/penrose/penrose/issues/754)) ([6ce1b97](https://github.com/penrose/penrose/commit/6ce1b97a68c3ef002035f716ba2cc1c4065ffaaa))
- Use ifCond in atDist ([#667](https://github.com/penrose/penrose/issues/667)) ([07157b6](https://github.com/penrose/penrose/commit/07157b6fcaa00298449054ae699209aadd41c5e8))

### Features

- "follow the cursor" dragging mode ([#1143](https://github.com/penrose/penrose/issues/1143)) ([e15276c](https://github.com/penrose/penrose/commit/e15276c81da7881fd66d9f5c4d862558f63b5ed2))
- 2d triangle mesh domain ([#770](https://github.com/penrose/penrose/issues/770)) ([bfc7b2e](https://github.com/penrose/penrose/commit/bfc7b2e67d2324f024366fb6e23d4baae2b7b291))
- 2d triangle mesh examples ([#808](https://github.com/penrose/penrose/issues/808)) ([e407858](https://github.com/penrose/penrose/commit/e407858ec17091ac460bc262e5ed3dc09fedf694))
- add "Duplicate Workspace" button ([#1106](https://github.com/penrose/penrose/issues/1106)) ([e576078](https://github.com/penrose/penrose/commit/e576078101a21f1103472ec6d6ad74d20140a618))
- add `@penrose/panels` to the monorepo ([#681](https://github.com/penrose/penrose/issues/681)) ([24d5bd0](https://github.com/penrose/penrose/commit/24d5bd09256b75c21b69e34e53758d732925e24e))
- Add a polynomial roots node ([#906](https://github.com/penrose/penrose/issues/906)) ([580ada4](https://github.com/penrose/penrose/commit/580ada4e2270a620f8c5662c7189c478e742eb7c))
- add browser for synthesizer ([#640](https://github.com/penrose/penrose/issues/640)) ([2d81a55](https://github.com/penrose/penrose/commit/2d81a55d0175c12195dc510f0514c741eb0f7803))
- add debug mode setting to `@penrose/editor` ([#1030](https://github.com/penrose/penrose/issues/1030)) ([d9c5485](https://github.com/penrose/penrose/commit/d9c54856f6899488694add3f44ceb966cfd4244e))
- add default `onCanvas` constraints for all shapes ([#694](https://github.com/penrose/penrose/issues/694)) ([20409e4](https://github.com/penrose/penrose/commit/20409e4756f1938e743d117af442b77abb237f6c))
- add docs-site build ([#856](https://github.com/penrose/penrose/issues/856)) ([526a635](https://github.com/penrose/penrose/commit/526a635d2f741f82a067365774e04e201f930f7e))
- Add examples with nonconvex shapes ([#893](https://github.com/penrose/penrose/issues/893)) ([91edc5c](https://github.com/penrose/penrose/commit/91edc5c86342285d96f2e85f593129c3f2e861c1))
- add full moon example ([#872](https://github.com/penrose/penrose/issues/872)) ([7c84f09](https://github.com/penrose/penrose/commit/7c84f09867363d9670bf1ced3b2e9970e88b38ef))
- Add Mobius transform example ([#1216](https://github.com/penrose/penrose/issues/1216)) ([43e7740](https://github.com/penrose/penrose/commit/43e774010301b8e7c594bad17433c21b66b747a6))
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
- compile on vim write (resolves [#1046](https://github.com/penrose/penrose/issues/1046)) ([#1197](https://github.com/penrose/penrose/issues/1197)) ([9ee17a6](https://github.com/penrose/penrose/commit/9ee17a62bbc78b73caa4c093ed4131359af588d5))
- compute `Text` bounding box ([#829](https://github.com/penrose/penrose/issues/829)) ([8886074](https://github.com/penrose/penrose/commit/88860747560926c981459b1014f804babd343ed7))
- customizable arrowheads on both ends + new arrowhead styles ([#1140](https://github.com/penrose/penrose/issues/1140)) ([0f60f05](https://github.com/penrose/penrose/commit/0f60f050f9fbff1effa50d04d2d3b097b87d2b18))
- Define bounding box function for every shape ([#698](https://github.com/penrose/penrose/issues/698)) ([28226dd](https://github.com/penrose/penrose/commit/28226ddc9880a48dc2c8982bbb4b158bbd5d463e))
- detect and report cyclic assignments in Style ([#1147](https://github.com/penrose/penrose/issues/1147)) ([0f122fb](https://github.com/penrose/penrose/commit/0f122fb7ff2e5ab1df3d06d70c7bd06fe184834c))
- display errors in the `Simple` component ([#953](https://github.com/penrose/penrose/issues/953)) ([aa6209f](https://github.com/penrose/penrose/commit/aa6209f520b6a2cbdcfe1b80767b233d27d69867)), closes [#535](https://github.com/penrose/penrose/issues/535)
- docusaurus site ([#771](https://github.com/penrose/penrose/issues/771)) ([13396b2](https://github.com/penrose/penrose/commit/13396b298280f63a9161174bf6b585a20613334c))
- Domain syntax highlighting ([#691](https://github.com/penrose/penrose/issues/691)) ([d9fdcb5](https://github.com/penrose/penrose/commit/d9fdcb53e98baa714d9049afcaa78cc1995eaf5e))
- Editor Rewrite ([#992](https://github.com/penrose/penrose/issues/992)) ([91022fa](https://github.com/penrose/penrose/commit/91022fafdd45e6e5810bcb87448095a1d105bae5))
- enumerative search of Substance mutations ([#638](https://github.com/penrose/penrose/issues/638)) ([97db076](https://github.com/penrose/penrose/commit/97db07673c16970216d56ec8af360639351361da))
- export diagrams in png ([#1134](https://github.com/penrose/penrose/issues/1134)) ([307c574](https://github.com/penrose/penrose/commit/307c574bfa3c1a11171a8382fa615b3c58c48265))
- Exterior algebra ([#812](https://github.com/penrose/penrose/issues/812)) ([9e49a45](https://github.com/penrose/penrose/commit/9e49a45175533ef3de056e0220addd7afea9e61f))
- Fake 3d linear algebra examples ([#1058](https://github.com/penrose/penrose/issues/1058)) ([41d0c83](https://github.com/penrose/penrose/commit/41d0c830b82ab221af75b4adc2176bd71575d8f8))
- faster matching ([#1072](https://github.com/penrose/penrose/issues/1072)) ([99c6383](https://github.com/penrose/penrose/commit/99c63837b534aab687b98f8864b27a176273b4e8))
- Graphics tweaks ([#843](https://github.com/penrose/penrose/issues/843)) ([c492e4a](https://github.com/penrose/penrose/commit/c492e4a1814e8f45bdcce7998ebd9d0bdfe43c72))
- hexadecimal color literals in Style ([#1114](https://github.com/penrose/penrose/issues/1114)) ([ce4cb51](https://github.com/penrose/penrose/commit/ce4cb51cdb22b67d01766bd744073f191cc0a262))
- host tutorial in online editor ([#1196](https://github.com/penrose/penrose/issues/1196)) ([4f361c9](https://github.com/penrose/penrose/commit/4f361c92de4544247722c931178c786a6546434e))
- Hypergraph example ([#998](https://github.com/penrose/penrose/issues/998)) ([15053e3](https://github.com/penrose/penrose/commit/15053e3c428d27ed16713895ddc551c10caefe0c))
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
- More domain examples ([#635](https://github.com/penrose/penrose/issues/635)) ([c0a0c3f](https://github.com/penrose/penrose/commit/c0a0c3f74789b1dae6809d2ff77b2cbce7faeb65))
- Persistent homology example ([#1007](https://github.com/penrose/penrose/issues/1007)) ([299145b](https://github.com/penrose/penrose/commit/299145bfd7561ef20dfd7b422ea36775aea80883))
- Polygon contains ([#868](https://github.com/penrose/penrose/issues/868)) ([9b7ad99](https://github.com/penrose/penrose/commit/9b7ad9932ff7c9f0f61cade4a36514a1e5d16aad))
- port the SIGGRAPH Euclidean geometry example ([#693](https://github.com/penrose/penrose/issues/693)) ([25878c4](https://github.com/penrose/penrose/commit/25878c49a326be7cc59aa91fed7539f56102a6d7))
- Predicate aliasing ([#1066](https://github.com/penrose/penrose/issues/1066)) ([de83edf](https://github.com/penrose/penrose/commit/de83edf8de661c5529e92c05524d6a28d914702a)), closes [#623](https://github.com/penrose/penrose/issues/623)
- preset loading in `synthesizer-ui` ([#1133](https://github.com/penrose/penrose/issues/1133)) ([7d0d7d8](https://github.com/penrose/penrose/commit/7d0d7d873df48ce82536f396d193c0cc45a51ff9))
- put the demo in the VitePress site ([#1193](https://github.com/penrose/penrose/issues/1193)) ([22fba15](https://github.com/penrose/penrose/commit/22fba1567426fa425564e9dcd514b69ca69622e6))
- React component library ([#671](https://github.com/penrose/penrose/issues/671)) ([7f5977b](https://github.com/penrose/penrose/commit/7f5977b9c578b0a47c0d7b3643426d62319c93d7))
- renderer: Passthrough unknown properties to Svg output ([d3175c6](https://github.com/penrose/penrose/commit/d3175c6145c7234971188a69cdd3de0053e2db94))
- renderer/style: Passthrough unknown properties to Svg output ([#759](https://github.com/penrose/penrose/issues/759)) ([d6b4283](https://github.com/penrose/penrose/commit/d6b428306ecb13aff73b2833c57b00525beebda8))
- requires `forall` declarations ([#1073](https://github.com/penrose/penrose/issues/1073)) ([942b06f](https://github.com/penrose/penrose/commit/942b06f3d161077aa9602405fff2d2d02aee9fa2))
- resolve image paths in `@penrose/editor` ([#1018](https://github.com/penrose/penrose/issues/1018)) ([7bb69e6](https://github.com/penrose/penrose/commit/7bb69e6ecbec42b1f500067e7c77dcc92ac665fa))
- resolve paths for included SVGs ([#825](https://github.com/penrose/penrose/issues/825)) ([cedbf1b](https://github.com/penrose/penrose/commit/cedbf1b0f219f013a0c825e08007a2edc3b2c3bc))
- Signed distance functions for Penrose shapes ([#979](https://github.com/penrose/penrose/issues/979)) ([1a00e4c](https://github.com/penrose/penrose/commit/1a00e4c113c8e1e308612e41528af50665d7b194))
- Split Optimization Status Tab Into Constraints and Objectives ([#611](https://github.com/penrose/penrose/issues/611)) ([ab9eee9](https://github.com/penrose/penrose/commit/ab9eee988df39ff2afee1c8dfc11ab45b75bc7e3))
- staged diagram generation in roger ([#610](https://github.com/penrose/penrose/issues/610)) ([3de4a31](https://github.com/penrose/penrose/commit/3de4a31543ddac80ed24274fde66d9e84304daa1))
- Structural formula example ([#734](https://github.com/penrose/penrose/issues/734)) ([bb18a6f](https://github.com/penrose/penrose/commit/bb18a6f56c6881ef6aa7cc9395ecee43670e2655))
- Style inline color widgets ([#1094](https://github.com/penrose/penrose/issues/1094)) ([3ffdbbe](https://github.com/penrose/penrose/commit/3ffdbbe8aef0a2588962bf242de85d39fa4792c7))
- Substance mutations as data + refactored program generator ([#601](https://github.com/penrose/penrose/issues/601)) ([da8f9e5](https://github.com/penrose/penrose/commit/da8f9e5cd53043095826f67d095c5cade1ea71fe))
- support layout stages in Style ([#1199](https://github.com/penrose/penrose/issues/1199)) ([d22602a](https://github.com/penrose/penrose/commit/d22602a7f31ce48c0c00a984efec5fa3622e63eb))
- support path lists in `layer` expressions ([#1111](https://github.com/penrose/penrose/issues/1111)) ([e1340e8](https://github.com/penrose/penrose/commit/e1340e837197964fff84811025a0e44005c952c5))
- Text baseline fix ([#875](https://github.com/penrose/penrose/issues/875)) ([eadf6a1](https://github.com/penrose/penrose/commit/eadf6a1a5b5186129dada31b5b3e3375266aa2e1))
- unify browser-ui and editor ([#1000](https://github.com/penrose/penrose/issues/1000)) ([3e7f647](https://github.com/penrose/penrose/commit/3e7f64729fb36ba7c735f0360dcc4f33fd04a49c))
- Use C-style syntax for Domain arglists ([#737](https://github.com/penrose/penrose/issues/737)) ([2af2447](https://github.com/penrose/penrose/commit/2af2447d936095f9770a51693cced5e6661946b2))
- Walk on spheres ([#1019](https://github.com/penrose/penrose/issues/1019)) ([a5d5da1](https://github.com/penrose/penrose/commit/a5d5da1b3e3eabf53360434b9bd6b806780d1eac))
- Walk on spheres ([#1022](https://github.com/penrose/penrose/issues/1022)) ([5863147](https://github.com/penrose/penrose/commit/58631478cadef46aa357aec4f7bf68fd5ff5d1ec))

### Performance Improvements

- Add a benchmark suite ([#921](https://github.com/penrose/penrose/issues/921)) ([9513462](https://github.com/penrose/penrose/commit/95134626d08b6e3f57c85eab9de44a2a6f07f726))
- improve performance of Autodiff ([#796](https://github.com/penrose/penrose/issues/796)) ([8bca6db](https://github.com/penrose/penrose/commit/8bca6dbee81e7bebd6fffa071b683658f04367da))
- improve performance of symbolic differentiation ([#840](https://github.com/penrose/penrose/issues/840)) ([7b5dd6a](https://github.com/penrose/penrose/commit/7b5dd6a3103268d11b70d6908f0e855484903225))
- port the optimizer to WebAssembly ([#1092](https://github.com/penrose/penrose/issues/1092)) ([768895a](https://github.com/penrose/penrose/commit/768895a3aac643095f0d139052fa8a139ce28cfb))
- speed up `roger` using SWC ([#1163](https://github.com/penrose/penrose/issues/1163)) ([516f57e](https://github.com/penrose/penrose/commit/516f57eeddba6110964623c0f88de24cef9d2ccd))

### Reverts

- Revert "feat: Walk on spheres (#1019)" (#1021) ([228746e](https://github.com/penrose/penrose/commit/228746ee7544e4cf69c84f7bf871f0c9d95edcc5)), closes [#1019](https://github.com/penrose/penrose/issues/1019) [#1021](https://github.com/penrose/penrose/issues/1021)

# [1.3.0](https://github.com/penrose/penrose/compare/v1.2.0...v1.3.0) (2021-06-24)

### Bug Fixes

- add repo link to docs deploy ([#598](https://github.com/penrose/penrose/issues/598)) ([1c2c2c8](https://github.com/penrose/penrose/commit/1c2c2c8bea493ced5ab52f38466284f989be5743))
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
- remove canvas from browser-ui's dependencies ([#559](https://github.com/penrose/penrose/issues/559)) ([b62f9e1](https://github.com/penrose/penrose/commit/b62f9e1f0ae88a9f7c395324d545bea232ee06c5))

### Features

- PathString GPI ([#549](https://github.com/penrose/penrose/issues/549)) ([2e9069d](https://github.com/penrose/penrose/commit/2e9069d7c14436226029b5e0ae2050b2dc6c205c))
- substance program synthesizer ([#551](https://github.com/penrose/penrose/issues/551)) ([09062ee](https://github.com/penrose/penrose/commit/09062eee7bd027396905958cf009305fcc8aa6f6))

# [1.1.0](https://github.com/penrose/penrose/compare/v1.0.0...v1.1.0) (2021-04-21)

### Bug Fixes

- [#481](https://github.com/penrose/penrose/issues/481) - wrong NS attribute ([#485](https://github.com/penrose/penrose/issues/485)) ([e6b4c7a](https://github.com/penrose/penrose/commit/e6b4c7ae252f6789c8145dbe0699a608f4f00490))
- [#520](https://github.com/penrose/penrose/issues/520) (moving types out, fixing soundness) ([#526](https://github.com/penrose/penrose/issues/526)) ([ba0abf3](https://github.com/penrose/penrose/commit/ba0abf3fb666beea4d6e85f60d9cf6840d668dba))
- [style-errors-2] fix style path validation code or remove unsupported style program features so LA example works (`findExpr` accesses lists) [#489](https://github.com/penrose/penrose/issues/489) ([#491](https://github.com/penrose/penrose/issues/491)) ([389f25c](https://github.com/penrose/penrose/commit/389f25c37d74a59155b8df23941a1ff9fc001012))
- add cross-env in browser-ui build scripts ([#497](https://github.com/penrose/penrose/issues/497)) ([201390e](https://github.com/penrose/penrose/commit/201390e99a4f2ca919b924b86685cbffd006295e))
- add source loc to parser errors; report unexpected EOF ([#510](https://github.com/penrose/penrose/issues/510)) ([8555c84](https://github.com/penrose/penrose/commit/8555c84a3a94ddca5930e8d012168de75c68599d))
- handle CRLF in all parsers ([#500](https://github.com/penrose/penrose/issues/500)) ([da2238a](https://github.com/penrose/penrose/commit/da2238a0e44c380daa59ecf3e6ee6737a8a74d76))
- K&R style braces in GPI exprs ([#544](https://github.com/penrose/penrose/issues/544)) ([ef0ae12](https://github.com/penrose/penrose/commit/ef0ae12d6244ec7689a5e3ae9ad03829ebe61115))
- newlines in errors (wrap now) and newlines in mathjax ([#493](https://github.com/penrose/penrose/issues/493)) ([9fbe49f](https://github.com/penrose/penrose/commit/9fbe49f27cd90500f27e20bdb1010692795d171a))
- render sizing due to container ([#488](https://github.com/penrose/penrose/issues/488)) ([3aa475b](https://github.com/penrose/penrose/commit/3aa475b6661b705f04ebaa7b5a9a92a73d65105d))
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

### Bug Fixes

- [#442](https://github.com/penrose/penrose/issues/442) ([7b39f52](https://github.com/penrose/penrose/commit/7b39f5234448220db0a9ecd37b806ce5ff914032))
- [#443](https://github.com/penrose/penrose/issues/443) [#444](https://github.com/penrose/penrose/issues/444); add type annotations to all postprocessors ([c952679](https://github.com/penrose/penrose/commit/c952679db4755048d243d7c86d35433c0a9a5d30))
- [#448](https://github.com/penrose/penrose/issues/448) ([940f118](https://github.com/penrose/penrose/commit/940f118f3c705c09d1c891dbd031d8ca4b7b155c))
- add decl_bind back to Substance parser ([#457](https://github.com/penrose/penrose/issues/457)) ([c6760e0](https://github.com/penrose/penrose/commit/c6760e0d034f7a3c7509cae806a3fa39a5ee1a96))
- add global pseudorandomness ([#472](https://github.com/penrose/penrose/issues/472)) ([0e1f793](https://github.com/penrose/penrose/commit/0e1f7937b6b611d63ca2cec558f7053218698167)), closes [#466](https://github.com/penrose/penrose/issues/466)
- add missing contrib functions ([2dda7b6](https://github.com/penrose/penrose/commit/2dda7b6e1545ec92c37fbe41f507b0f9cffd57f4))
- add string transform to string_literal token ([#458](https://github.com/penrose/penrose/issues/458)) ([9fb2169](https://github.com/penrose/penrose/commit/9fb2169910ec5194fdc2a8e95417ccd6006129aa))
- allow trailing comments without newline at the end ([#471](https://github.com/penrose/penrose/issues/471)) ([3098b62](https://github.com/penrose/penrose/commit/3098b628ffb77576251e3ffdc7d1dff99d63b112))
- disable new iter warning [#431](https://github.com/penrose/penrose/issues/431) ([d2b69ae](https://github.com/penrose/penrose/commit/d2b69ae49dd489aa3557cee2fba030ec5f417537))
- enable autostep on startup [#420](https://github.com/penrose/penrose/issues/420) ([712ff5c](https://github.com/penrose/penrose/commit/712ff5c82544eb0fef5060aee9d57aa9ef76152c))
- layer expr error [#433](https://github.com/penrose/penrose/issues/433) ([4d60331](https://github.com/penrose/penrose/commit/4d60331566bd44eca7f32a908b89695d72410948))
- return parser errors monadically ([#468](https://github.com/penrose/penrose/issues/468)) ([6305301](https://github.com/penrose/penrose/commit/6305301dbcd66a6eee41fd771a29eb2c8604cc9c))
- still process state packets in protocol ([af7ccdd](https://github.com/penrose/penrose/commit/af7ccdd41beac5a331f76ff8d052a1a127458905))

### Features

- vanilla renderer ([#465](https://github.com/penrose/penrose/issues/465)) ([72f3bb0](https://github.com/penrose/penrose/commit/72f3bb0b1569e1debe97f515105e8fd32a3118b5))

# [1.0.0-alpha.3](https://github.com/penrose/penrose/compare/v1.0.0-alpha.2...v1.0.0-alpha.3) (2021-02-15)

**Note:** Version bump only for package penrose

# [1.0.0-alpha.2](https://github.com/penrose/penrose/compare/v1.0.0-alpha.1...v1.0.0-alpha.2) (2021-02-15)

**Note:** Version bump only for package penrose

## 1.0.1 (2021-01-28)

### Bug Fixes

- [#442](https://github.com/penrose/penrose/issues/442) ([7b39f52](https://github.com/penrose/penrose/commit/7b39f5234448220db0a9ecd37b806ce5ff914032))
- [#443](https://github.com/penrose/penrose/issues/443) [#444](https://github.com/penrose/penrose/issues/444); add type annotations to all postprocessors ([c952679](https://github.com/penrose/penrose/commit/c952679db4755048d243d7c86d35433c0a9a5d30))
- [#448](https://github.com/penrose/penrose/issues/448) ([940f118](https://github.com/penrose/penrose/commit/940f118f3c705c09d1c891dbd031d8ca4b7b155c))
- install script for roger ([2724feb](https://github.com/penrose/penrose/commit/2724feb19d5ff2c4697a8da563b91e330857091d))
- still process state packets in protocol ([af7ccdd](https://github.com/penrose/penrose/commit/af7ccdd41beac5a331f76ff8d052a1a127458905))

# Revision history for penrose

## 0.1.0.0 -- YYYY-mm-dd

- First version. Released on an unsuspecting world.
