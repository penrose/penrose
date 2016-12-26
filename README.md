# diagrams

Directories:

`optimization` contains the code I'm currently working on, as well as GIFs. `layout.hs` contains the cleaned-up and commented code. The rest of the files contain irrelevant code relating to the autodiff library.

`constraint-code` contains the old code for the set theory DSL and simple constraint satisfaction.

`documentation` only contains the system diagram in Keynote.

`byhand` contains diagrams made by hand, e.g. in Keynote.

----

Usage for current code:

Compile: `ghc layout.hs`

Install any relevant packages: `cabal install $PACKAGE_NAME` (though I'm told the Haskell community has moved on to the better `stack` package manager?)

Run: `./layout.hs`

Pressing the `R` key will resample the configuration. You can also click and drag the objects. The object on top is semi-arbitrary, decided by the order of the objects in the internal list.

----

Functionality of the current code:

* 
* 
* 

Limitations: 

* 
* 
* 

Parameters: 

* 
* 
* 


Debugging:

* 
* 
* 

----

Usage for old code:

Compile: `ghc settheory.hs`

Create SVG: `./settheory -w 500 -h 500 -o set.svg`
(The parameters are the width and height of the rendered picture.)

Open SVG: `chrome set.svg`
