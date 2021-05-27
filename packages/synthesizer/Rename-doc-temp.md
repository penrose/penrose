## Rename Operator Doc

(Josh wanted this operator to be called something else and I can't quite remember what he had in mind?)

#### Things to be accounted for

- Within `synthesizer` package: Need to make some updates to JSON configuration files so that Rename is one of the possible operations that it tries. Initially would be good to make a JSON config that only allows Renames to make sure that things are working correctly
  - Would Rename only be replacing predicates?
  - All operations that someone is interested in trying for a given problem set would be included in a JSON config file, is that correct?
  - Seems like Synthesizer will make a random choice between add/edit/delete regardless if they are defined in the JSON file, and just return no change if there are no valid options?
- Within `core/src/synthesis`, `Synthesizer.ts`:
  - Make a random choice between edit operations now that there are 2 (line 351)
  - Create a new interface called `Rename`, add it to `Modify` type
  - There’s already an interface called `Replace` that is included in the `Modify` type which seems to have some logic implemented already, how is it different than the intended Rename functionality?
  - Add logic in `editStmt()` method to handle switch case for `Rename`
    - Similar to what is being done in `swap` case but call `Rename` instead. Also don't need to find indices since those orrespond to the parameters passed into the constructor/function and are specific to the `swap` functionality.
  - What is the difference between `Bind`, `EqualPredicate` and `ApplyPredicate`?
- Within `core/src/types/substance.ts`, what is the difference between the difference kinds of `SubStmt`? Can I use one of the `Predicate` types or should I make a new one for this case?
- Within `core/src/analysis/SubstanceAnalysis.ts`: Write a function called `rename()` that can be called in `core/src/synthesis`. It should take a parameter called `stmt` that is a type of `SubStmt`, maybe one or more of the types mentioned above (`Bind`, `EqualPredicate`, `ApplyPredicate`)?
  - Write a helper function that can determine what the **type** and **number** of parameters in the `stmt` are, because we can only Rename predicates that will not cause type errors or expect more/less parameters than are provided. i.e. `IsSubset(A,B) -> Equal(A,B)` is valid but `IsSubset(A,B) -> Colinear(B,D,E)` is not. This helper function will choose a random predicate from the set of all valid Renames, or the original predicate if there were no valid options.
- At a high level, is the synthesizer doing this:

```
Read .sub and json input, init synthesizer
new_progs = []
for i in range(numProgramsToMake):
    reset to original substance
    for j in range(random(mutationCount)):
        mutate(program) #randomly choose between add/edit/delete, all of rename logic should be within the editStmt method
        new_progs.append(mutatedProgram)
return new_progs converted from objects to a bunch of files
```
