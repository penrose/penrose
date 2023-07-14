# Change Log

All notable changes to this project will be documented in this file.
See [Conventional Commits](https://conventionalcommits.org) for commit guidelines.

## [v3.0.0-beta.0](https://github.com/penrose/penrose/compare/v2.3.0...v3.0.0-beta.0) (2023-07-14)

### :warning: BREAKING CHANGE

- cleanup optimizer interface (#1368)
- combine `automator` and `roger` (#1387)
- decouple optimizer from codegen (#1338)

### :rocket: New Feature

- cleanup optimizer interface ([#1368](https://github.com/penrose/penrose/issues/1368)) ([fec7838](https://github.com/penrose/penrose/commit/fec78388610690b7743596bd3f797f34cf4b29ba))
- decouple optimizer from codegen ([#1338](https://github.com/penrose/penrose/issues/1338)) ([99f2633](https://github.com/penrose/penrose/commit/99f263315478dae3055fd299a0d588632c501d3f))
- export `core` API functions for optimization specification and solving ([#1391](https://github.com/penrose/penrose/issues/1391)) ([71aa047](https://github.com/penrose/penrose/commit/71aa047cb276b5ec366a7893620d2250f3fd07f8))

### :bug: Bug Fix

- use `Serializer::json_compatible()` ([#1457](https://github.com/penrose/penrose/issues/1457)) ([d120b25](https://github.com/penrose/penrose/commit/d120b2591beaa93ae395d7c3a288910beb96ecaa))

### :nail_care: Polish

- bump TypeScript version to 5.0 ([#1395](https://github.com/penrose/penrose/issues/1395)) ([b4ae329](https://github.com/penrose/penrose/commit/b4ae3298c9a03926ca690c63f368adcaa031b56d))
- combine `automator` and `roger` ([#1387](https://github.com/penrose/penrose/issues/1387)) ([678c6e5](https://github.com/penrose/penrose/commit/678c6e528d20d6cbbfd3a04f1fcad656e72bdc6e))
- pull out base `tsconfig.json` ([#1392](https://github.com/penrose/penrose/issues/1392)) ([e6c5f55](https://github.com/penrose/penrose/commit/e6c5f5524837fe4c970713f05bbed821b9cda411))
- put L-BFGS into its own module ([#1355](https://github.com/penrose/penrose/issues/1355)) ([cc21457](https://github.com/penrose/penrose/commit/cc214579990a6330b6843d23c03c876686817550))
- use Clippy ([#1426](https://github.com/penrose/penrose/issues/1426)) ([59da3d2](https://github.com/penrose/penrose/commit/59da3d2d36ac4a08e2fa0290c196c4b92302c489))

### :memo: Documentation

- using Penrose programmatically ([#1525](https://github.com/penrose/penrose/issues/1525)) ([7952b2b](https://github.com/penrose/penrose/commit/7952b2baa81fd4e5631135b707703cbc07646380))

### :house: Internal

- switch from jest to vitest ([#1406](https://github.com/penrose/penrose/issues/1406)) ([8ef8c77](https://github.com/penrose/penrose/commit/8ef8c778488b17eb0f02a62d1399e0b0337f5355))

### :running_woman: Performance

- don't materialize any Hessian estimate ([#1353](https://github.com/penrose/penrose/issues/1353)) ([2678139](https://github.com/penrose/penrose/commit/2678139c5399ef3c0d64dcceb91be41d6141df2f))

## [v2.3.0](https://github.com/penrose/penrose/compare/v2.2.0...v2.3.0) (2023-03-14)

### :bug: Bug Fix

- avoid `EPS_DENOM` in core autodiff ([#1333](https://github.com/penrose/penrose/issues/1333)) ([db9f38b](https://github.com/penrose/penrose/commit/db9f38becbcb628eb9864b3ba7d0a7018e304c64))

## [v2.2.0](https://github.com/penrose/penrose/compare/v2.1.1...v2.2.0) (2023-02-02)

### :rocket: New Feature

- support longer file extensions ([#1280](https://github.com/penrose/penrose/issues/1280)) ([6e83596](https://github.com/penrose/penrose/commit/6e835968280a784a91c4a2ca47a226516a3067d0))

## [v2.1.1](https://github.com/penrose/penrose/compare/v2.1.0...v2.1.1) (2023-01-19)

**Note:** Version bump only for package @penrose/optimizer

# [2.1.0](https://github.com/penrose/penrose/compare/v2.0.0...v2.1.0) (2023-01-19)

**Note:** Version bump only for package @penrose/optimizer

# [2.0.0](https://github.com/penrose/penrose/compare/v1.3.0...v2.0.0) (2023-01-17)

### Bug Fixes

- map `Option::None` to `null`, not `undefined` ([#1191](https://github.com/penrose/penrose/issues/1191)) ([cd195e9](https://github.com/penrose/penrose/commit/cd195e9284c9624573aa88afa98b98ad0fd344f0))

### Features

- allow autodiff addends to be masked ([#1192](https://github.com/penrose/penrose/issues/1192)) ([2870b5f](https://github.com/penrose/penrose/commit/2870b5fd1706da5cf614ee211a3e7729dd0f872f))
- support layout stages in Style ([#1199](https://github.com/penrose/penrose/issues/1199)) ([d22602a](https://github.com/penrose/penrose/commit/d22602a7f31ce48c0c00a984efec5fa3622e63eb))

### Performance Improvements

- port the optimizer to WebAssembly ([#1092](https://github.com/penrose/penrose/issues/1092)) ([768895a](https://github.com/penrose/penrose/commit/768895a3aac643095f0d139052fa8a139ce28cfb))
