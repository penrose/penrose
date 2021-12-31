This directory contains tools to generate autodiff code from functions expressed via Mathematica.

Evaluating the notebook `GenerateDerivatves.nb` will compute derivatives for all functions in the list `functions`, and write these derivatives as plain text to `derivatives.txt`.  Executing the Perl script `GenerateCode.pl` then transforms this file into TypeScript code that can be added to `Autodiff.ts`.
