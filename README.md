# Penrose

Note: this README is obsolete! 

TODO: redocument the code after the dust settles.

### Directories

`optimization` contains the code I'm currently working on, as well as GIFs. `layout.hs` contains the cleaned-up and commented code. The rest of the files contain irrelevant code relating to the autodiff library.

`constraint-code` contains the old code for the set theory DSL and simple constraint satisfaction.

`documentation` only contains the system diagram in Keynote.

`byhand` contains diagrams made by hand, e.g. in Keynote.

----

#### More information

I use the [gloss library](
I use the following library for the graphics/animation/input: https://hackage.haskell.org/package/gloss-1.10.2.3/docs/Graphics-Gloss-Interface-Pure-Game.html) to handle graphics, animation, and user input. 

Functionality of the current code:

* gradient-descent-based layout 
* with backtracking line search 
* for sets only 
* with very simple objective functions provided (e.g. centering)
* where the layout is animated and interactive (v. useful for debugging)

Pressing limitations: 

* need to add autodiff
* need to debug line search and add Wolfe condition
* does not handle arbitrary numbers of objects
* does not handle labels, points, boxes, or maps (labels were handled at one point, but I removed the functionality)
* needs more objective functions to handle other cases
* needs to generalize to sums of objective functions and ones with arbitrary numbers of parameters, including object size
* need a better debugging interface for optimization, e.g. live parameter tuning

Parameters: 

* picWidth, picHeight: canvas dimensions
* stepFlag: turns stepping the simulation on and off for debugging (no stepping = objects don't move)
* clampFlag: turns clamping gradient values on and off for debugging
* debug: turns on/off the debug print functions
* constraintFlag: turns constraint satisfaction on/off (currently off because we're doing unconstrained optimization)
* objFn1: objective function to use
* btls: turn on/off the backtracking line search for debugging (off = use a fixed timestep specified in the code)
* alpha and beta: parameters for the backtracking line search (see code for a more detailed description)
* stopEps: stopping condition sensitivity for gradient descent. Stop when magnitude of gradient is less than stopEps.

Debugging:

* Use the flags above.
* I also use `ghci`, the Haskell REPL. To load the file, do `:l filename.hs`. To import a library, paste in the normal import statement. To declare something, start with a `let` statement, e.g. `let x = 5`.
* For printing internal values, I use the [Debug.Trace](https://hackage.haskell.org/package/base-4.9.0.0/docs/Debug-Trace.html) library.

----

### Usage for current code

Compile: `ghc layout.hs`

Install any relevant packages: `cabal install $PACKAGE_NAME` (though I'm told the Haskell community has moved on to the better `stack` package manager?)

Run: `./layout.hs`

Pressing the `R` key will resample the configuration. You can also click and drag the objects. The object on top is semi-arbitrary, decided by the order of the objects in the internal list.

----

### Usage for old code

Compile: `ghc settheory.hs`

Create SVG: `./settheory -w 500 -h 500 -o set.svg`
(The parameters are the width and height of the rendered picture.)

Open SVG: `chrome set.svg`
