# Penrose

Not ready for contributions or public use yet, but hopefully will be soon! See [the site](penrose.ink) for more information and examples.

Quick start:

* Navigate to `src/`
* For Alloy support: execute `make` in `src/`
* Compile the system: `stack ghc Main.hs` (with the latest GHC, 8.0.2)
* Start a server: `python -m SimpleHTTPServer`
* Start the Penrose runtime, providing a pair of Substance/Style programs: `./Main snap sub/surjection.sub sty/surjection.sty `
* View the visualization with interactive optimization: in your browser, navigate to http://localhost:8000/client.html
* In the UI, try stepping, autostepping, and resampling the state. You can also drag objects to set a new initial state for the optimization.

For more thorough documentation, see Nimo Ni's [README](https://github.com/wodeni/notes-pub/blob/master/penrose/ramp-down.md).

----
### Example

Consider the following Substance and Style programs for set theory:

- `tree.sub`
    ```
    Set A
    Set B
    Set C
    Set D
    Set E
    Set F
    Set G
    Subset B A
    Subset C A 
    Subset D B
    Subset E B
    Subset F C
    Subset G C
    NoIntersect E D
    NoIntersect F G
    NoIntersect B C
    ```
- `venn.sty`
    ```
    Set x {
        shape = Circle { }
        constraint contains(x, x.label)
    }

    Intersect x y {
        constraint overlapping(x, y)
        constraint outsideOf(y.label, x)
        constraint outsideOf(x.label, y)
    }

    NoIntersect x y {
        constraint nonOverlapping(x, y)
    }

    Subset x y {
        constraint contains(y, x)
        constraint smallerThan(x, y)
        constraint outsideOf(y.label, x)
    }

    NoSubset x y {
        objective repel(x, y)
        constraint outsideOf(x, y)
        constraint outsideOf(y.label, x)
        constraint outsideOf(x.label, y)
        constraint nonOverlapping(x, y)
    }
    ```
Here is how Penrose visualizes this:

<img src="https://i.imgur.com/3JHZeaX.png" width=300>

And here's how the optimization looks live in the UI: blob:https://imgur.com/bca78213-a3db-4ccb-8c12-b7f569edd5a4

----

### More information (possibly outdated)

Parameters:

* stepsPerSecond: number of simulation steps for `gloss` to take for each second of real time
* picWidth, picHeight: canvas dimensions
* stepFlag: turns stepping the simulation on and off for debugging (no stepping = objects don't move)
* clampFlag: turns clamping gradient values on and off for debugging
* debug: turns on/off the debug print functions
* constraintFlag: turns constraint satisfaction on/off (currently off because we're doing unconstrained optimization)
* Default ambient objective functions are specified in `ambientObjFns`, and analogously for `ambientConstrFns`.
* Default objective functions are specified in `genObjsAndFns`.
* btls: turn on/off the backtracking line search for debugging (off = use a fixed timestep specified in the code)
* alpha and beta: parameters for the backtracking line search (see code for a more detailed description)
* stopEps: stopping condition sensitivity for gradient descent. Stop when magnitude of gradient is less than stopEps.

Debugging:

* Use the flags above.
* I also use `ghci`, the Haskell REPL. To load the file, do `:l filename.hs`. To import a library, paste in the normal import statement. To declare something, start with a `let` statement, e.g. `let x = 5`.
* For printing internal values, I use the [Debug.Trace](https://hackage.haskell.org/package/base-4.9.0.0/docs/Debug-Trace.html) library.
* ghci comes with a nice [debugger](https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/ghci-debugger.html).
