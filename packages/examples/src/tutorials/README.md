# Penrose Tutorials

The tutorial can be found [here](https://penrose.cs.cmu.edu/docs/tutorial/welcome).

Starter code for Penrose tutorials. Each folder within `./code` contains a "program triple" (a `.sub`, `.sty`, and .`dsl` file), the minimum set of files that are needed to run a Penrose program. Solutions for each example can be found in `./solutions`.

## Prerequistes

Follow [these instructions](https://github.com/penrose/penrose/wiki/Building-and-running) to get the Penrose repo set up.

## Instructions

Clone this repository and refer to the linked tutorial above to get started!

## Building Examples

To compile examples and view them in your <http://localhost:3000> browser window, you will need two terminal windows opened at the **root of your cloned Penrose repository**.

- Like the build instructions, we will run `yarn start` from one of the terminal windows.
- In the second terminal window, run
  ```bash
  npx roger watch <path_to_triple>/<sub_file> <path_to_triple>/<sty_file> <path_to_triple>/<dsl_file>
  ```

For example, if I was testing my code for Tutorial 1, and I had cloned this repo at `~/repos` on my computer, then my `path_to_triple` would be `~/repos/penrose/packages/examples/src/tutorials/code/tutorial1`, and I would run:

```bash
npx roger watch ~/repos/penrose/packages/examples/src/tutorials/code/tutorial1/twoSets.sub ~/repos/penrose/packages/examples/src/tutorials/code/tutorial1/twoSets.sty ~/repos/penrose/packages/examples/src/tutorials/code/tutorial1/setTheory.dsl
```
