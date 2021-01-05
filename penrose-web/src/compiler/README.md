Guide:

- `Style.ts`: compiler implementation
    - It is called in `Canvas.tsx` as such: `compileStyle(stateJSON, styJSON)`
        - currently hardcoded to run on `linear-algebra-paper-simple.sty` JSON data only
    - To run: In the context of the system (`npm start`), check Chrome console; currently throws error at the end
- `Style.test.ts`: tests on `StyleTestData.ts`
    - To run: `npm test` for all tests (including parser); `npm test -- -t Compiler` to just test the Style compiler. 
        - This starts a test server, which will automatically rerun the tests when `Style.test.ts` is saved
    - `StyleTestData.ts`: JSON structure for `linear-algebra-paper-simple.sty` and `linear-algebra-paper-simple.ast.json`

Incomplete parts are marked with TODO or COMBAK.
