# Guide to the compiler

- `Style.ts`: compiler implementation
  - It is called in `Canvas.tsx` as such: `compileStyle(stateJSON, styJSON)`
    - currently hardcoded to run on `linear-algebra-paper-simple.sty` JSON data only
  - To run: In the context of the system (`npm start`), check Chrome console; currently throws error at the end
- `Style.test.ts`: tests on `StyleTestData.ts`
  - To run: in `penrose-web`, run `npm test` for all tests (including parser); `npm test -- -t Compiler` to just test the Style compiler.
    - This starts a test server, which will automatically rerun the tests when `Style.test.ts` is saved
  - `StyleTestData.ts`: JSON structure for `linear-algebra-paper-simple.sty` and `linear-algebra-paper-simple.ast.json`

Incomplete parts are marked with TODO or COMBAK.

# Generating new test data

This is needed if the parser changes (e.g. the grammar changed).

1. Rerun the normal system build-and-run chain: `npm run build-lib; npm install` This will regenerate the parser.
2. Run the parser's tests to generate the new ASTs from its tests, which are used for the Style compiler tests. To do this, from `penrose-web` run `npm test -- -t "Real Program"`. (See below for more info on how to rerun the parser.)
3. Move the new ASTs into the `compiler` folder as follows: from `penrose-web`, do `cp -rf /tmp/asts compiler` (this will override the old `compiler/asts` folder).
4. Make sure the compiler runs and passes tests with the new grammar.

# Running the parser

The test suite is located at `penrose-web/src/parser/StyleParser.test.ts`. It has suites for each kind of grammatical construct (e.g. block expr, expr, arith expr).

Run `npm test -- -t StyleParser` to execute only this suite. The `Real Program` suite will parse all working examples and write the asts to `/tmp/asts`.
