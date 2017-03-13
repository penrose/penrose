Penrose
=======

To compile both:
`ghc Runtime.hs`

To just compile Compiler:
`ghc Compiler.hs`

To use:
`./Runtime <filename>.sub <filename>.sty`

Existing pairs:
* twosets.sub twosets.sty
* continuousmap1.sub continuousmap1.sty
 
Design:
* Compiler parses the Substance and Style programs and combines their abstract syntax trees into Layout (the intermediate layout representation).
* Runtime calls Compiler on the input files, and transforms the data in Layout to Opt (the representations used by the optimization code).
* Runtime imports Compiler as a module.