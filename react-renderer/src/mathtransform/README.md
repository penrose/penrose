# Math Transformer

## Prerequisities
You must install [jscodeshift](https://www.npmjs.com/package/jscodeshift).

## Current functionality

Currently the program only transforms binary operators (`+ - * /`) and unary operators (`-`) to call expressions, as well as any type to any other type. 

|Original   | AD |
|-----------|----|
| x + y     | add(x, y)|
| x - y     | sub(x, y)|
| x * y     | mul(x, y)|
| x / y     | div(x, y)|
| -x        | neg(x)|
| x: number | x: VarAD|

The names to transform to can be modified by editing `toCustomAD.ts`. For instance, to replace instances of `x+y` with `x.sum(y)`, replace the `+: add` entry in `BOPS` with `+: sum`. The program also supports custom type transforms as well as the built-in `number` to `VarAD` replacement (which can, of course, be easily altered or removed). For example, to replace all mentions of type `Tensor` with type `VarAD`, add the line `"Tensor": ["VarAD", TYPSUB]` to the `TYPS` map in `toCustomAD.ts`. 

NB - Due to a bug in jscodeshift, the program currently does not transform type names when they appear in an array pattern (i.e. types declared like this: `([t1, s1]: [string, any], [t2, s2]: [string, any])`). You must manually translate these names yourself (though the program will translate the same type name for you in all other contexts)

To add more code modifications, just add the appropriate functions and call them in `toCustomAD.ts`.

## Marking nodes to transform

To mark an AST node (basically any unit in TS, e.g. function, string, variable, type, etc.) to be transformed, comment `// autodiff` immediately above the line containing your node. It will transform that node and any subnodes. For example, to transform all binary operations inside a function, commment `// autodiff` directly above the function definition. Your nodes can be as little or as big as you'd like. If you'd like to transform all functions inside a map of functions, for instance, comment right above the dictionary's definition. If you'd only like to transform certain functions within the map, commment above each individual function. Again, your desired nodes to transform DO NOT have to be top-level function definitions.

## Testing Paths

CLI usage for jscodeshift can be found [here](https://github.com/facebook/jscodeshift).

`jscodeshift bugtest.ts -t toCustomAD.ts -p -d`

`jscodeshift mathtest.ts -t toCustomAD.ts -p -d`

Template: `jscodeshift YOURFILE.ts -t toCustomAD.ts -p -d`

### IMPORTANT NOTE - JSCODESHIFT MODIFIES THE INPUT FILE!

JSCodeshift will modify your input file in place. If this is undesirable behavior, add `-d` to perform a dry-run, and `-p` to print the output. This will take your input file and prints what the modified file would be to the console without actually modifying the file (useful, but does not have syntax highlighting). If you want to most easily compare the input and output files, I suggest making a copy of the original file, and then running JSCodeshift on the copy.

## Sample Input

    export const objDict = {
        // autodiff
        equal: (x: number, y: number) => squared(x - y),

        // autodiff
        above: ([t1, top]: [string, any], [t2, bottom]: [string, any], offset = 100) =>
            // (getY top - getY bottom - offset) ^ 2
            squared(top.y.contents - bottom.y.contents - varOf(offset))
    }

## Sample Output
    export const objDict = {
        // autodiff
        equal: (x: VarAD, y: VarAD) => squared(sub(x, y)),

        // autodiff
        above: ([t1, top]: [string, any], [t2, bottom]: [string, any], offset = 100) =>
            // (getY top - getY bottom - offset) ^ 2
            squared(sub(sub(top.y.contents, bottom.y.contents), varOf(offset)))
    }