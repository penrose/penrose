# Change Log

All notable changes to this project will be documented in this file.
See [Conventional Commits](https://conventionalcommits.org) for commit guidelines.

# [2.0.0](https://github.com/penrose/penrose/compare/v1.3.0...v2.0.0) (2023-01-17)


### Bug Fixes

* Add Support for images with absolute URLs ([#1033](https://github.com/penrose/penrose/issues/1033)) ([03a9b03](https://github.com/penrose/penrose/commit/03a9b035b0ead1a28dd6980f58a6c42ceea165c5))
* base url ([#997](https://github.com/penrose/penrose/issues/997)) ([d329dd0](https://github.com/penrose/penrose/commit/d329dd067b82b182c2eab1a6436fc8b3657b1975))
* canvas disappears on mobile browsers ([#1136](https://github.com/penrose/penrose/issues/1136)) ([a060858](https://github.com/penrose/penrose/commit/a060858a2f79a5da93c12421e258db33b36a4d22))
* delete `svg-flatten` ([#1208](https://github.com/penrose/penrose/issues/1208)) ([976ca77](https://github.com/penrose/penrose/commit/976ca770c8eae9b95d2d6f7b36937005bbac8bcf))
* Give Vite 8 GiB in editor ([#1090](https://github.com/penrose/penrose/issues/1090)) ([3e328c8](https://github.com/penrose/penrose/commit/3e328c81f1843866aac89ebc947b463c08207b80))
* Issue [#1023](https://github.com/penrose/penrose/issues/1023) Allow stroke on Equations ([#1026](https://github.com/penrose/penrose/issues/1026)) ([77e1f87](https://github.com/penrose/penrose/commit/77e1f870ccd02794a0ccdc2f1a9ffcf2e96be829))
* Local image resolution not working in IDE ([#1037](https://github.com/penrose/penrose/issues/1037)) ([c5220b4](https://github.com/penrose/penrose/commit/c5220b43a753a7e8972331f0a5253c3fe475c06b))
* resize behaviors of diagram panel components ([#1105](https://github.com/penrose/penrose/issues/1105)) ([7874667](https://github.com/penrose/penrose/commit/787466793507105b34674cfe1d6e637c160db4ae))
* responsive tab layout on mobile ([#1137](https://github.com/penrose/penrose/issues/1137)) ([95b7f3e](https://github.com/penrose/penrose/commit/95b7f3eac44b1ee29d61d88a071be94eea33780a))
* storybook deployment base directory ([#1124](https://github.com/penrose/penrose/issues/1124)) ([7fb7a01](https://github.com/penrose/penrose/commit/7fb7a013d01a9fc1d40310b2a8505f99d7ea9468))


### Features

* add "Duplicate Workspace" button ([#1106](https://github.com/penrose/penrose/issues/1106)) ([e576078](https://github.com/penrose/penrose/commit/e576078101a21f1103472ec6d6ad74d20140a618))
* add debug mode setting to `@penrose/editor` ([#1030](https://github.com/penrose/penrose/issues/1030)) ([d9c5485](https://github.com/penrose/penrose/commit/d9c54856f6899488694add3f44ceb966cfd4244e))
* compile on vim write (resolves [#1046](https://github.com/penrose/penrose/issues/1046)) ([#1197](https://github.com/penrose/penrose/issues/1197)) ([9ee17a6](https://github.com/penrose/penrose/commit/9ee17a62bbc78b73caa4c093ed4131359af588d5))
* Editor Rewrite ([#992](https://github.com/penrose/penrose/issues/992)) ([91022fa](https://github.com/penrose/penrose/commit/91022fafdd45e6e5810bcb87448095a1d105bae5))
* export diagrams in png ([#1134](https://github.com/penrose/penrose/issues/1134)) ([307c574](https://github.com/penrose/penrose/commit/307c574bfa3c1a11171a8382fa615b3c58c48265))
* host tutorial in online editor ([#1196](https://github.com/penrose/penrose/issues/1196)) ([4f361c9](https://github.com/penrose/penrose/commit/4f361c92de4544247722c931178c786a6546434e))
* improve `euclidean.sty` ([#1117](https://github.com/penrose/penrose/issues/1117)) ([3a94d6d](https://github.com/penrose/penrose/commit/3a94d6d57a8c9c0e4809a05ad2d2711d919349e0))
* Make SVGs "Penrose-editable" ([#1171](https://github.com/penrose/penrose/issues/1171)) ([edb5dc8](https://github.com/penrose/penrose/commit/edb5dc8d80fa86e762dfd4bf17d9a66e1d59a950))
* resolve image paths in `@penrose/editor` ([#1018](https://github.com/penrose/penrose/issues/1018)) ([7bb69e6](https://github.com/penrose/penrose/commit/7bb69e6ecbec42b1f500067e7c77dcc92ac665fa))
* Signed distance functions for Penrose shapes ([#979](https://github.com/penrose/penrose/issues/979)) ([1a00e4c](https://github.com/penrose/penrose/commit/1a00e4c113c8e1e308612e41528af50665d7b194))
* support layout stages in Style ([#1199](https://github.com/penrose/penrose/issues/1199)) ([d22602a](https://github.com/penrose/penrose/commit/d22602a7f31ce48c0c00a984efec5fa3622e63eb))
* unify browser-ui and editor ([#1000](https://github.com/penrose/penrose/issues/1000)) ([3e7f647](https://github.com/penrose/penrose/commit/3e7f64729fb36ba7c735f0360dcc4f33fd04a49c))
* Walk on spheres ([#1019](https://github.com/penrose/penrose/issues/1019)) ([a5d5da1](https://github.com/penrose/penrose/commit/a5d5da1b3e3eabf53360434b9bd6b806780d1eac))


### Performance Improvements

* port the optimizer to WebAssembly ([#1092](https://github.com/penrose/penrose/issues/1092)) ([768895a](https://github.com/penrose/penrose/commit/768895a3aac643095f0d139052fa8a139ce28cfb))


### Reverts

* Revert "feat: Walk on spheres (#1019)" (#1021) ([228746e](https://github.com/penrose/penrose/commit/228746ee7544e4cf69c84f7bf871f0c9d95edcc5)), closes [#1019](https://github.com/penrose/penrose/issues/1019) [#1021](https://github.com/penrose/penrose/issues/1021)
