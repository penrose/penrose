# Math Transformer

Currently the program only transforms binary operators (`+ - * /`) and unary operators (`-`) to call expressions, as well as any type to any other type.

## Testing Paths

`jscodeshift bugtest.ts -t toCustomAD.ts -p -d`

`jscodeshift mathtest.ts -t toCustomAD.ts -p -d`

Template: `jscodeshift YOURFILE.ts -t toCustomAD.ts -p -d`

### IMPORTANT NOTE - JSCODESHIFT MODIFIES THE INPUT FILE!

JSCodeshift will modify your input file in place. If this is undesirable behavior, add `-d` to perform a dry-run, and `-p` to print the output. This will take your input file and prints what the modified file would be to the console without actually modifying the file (useful, but does not have syntax highlighting). If you want to most easily compare the input and output files, I suggest making a copy of the original file, and then running JSCodeshift on the copy.

