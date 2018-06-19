# Penrose [![Build Status](https://travis-ci.org/penrose/penrose.svg?branch=master)](https://travis-ci.org/penrose/penrose)

* See [the site](http://www.penrose.ink/) for more information and examples. 
* See the [wiki](https://github.com/penrose/penrose/wiki) for more system-specific information on building, running, testing, and debugging the system. 
* For even more documentation, see Nimo Ni's [README](https://github.com/wodeni/notes-pub/blob/master/penrose/ramp-down.md).

Our system is not ready for contributions or public use yet, but hopefully will be soon. Send us an email if you're interested in collaborating.

### Example

Here a Penrose visualization in the domain of set theory.

<img src="https://i.imgur.com/3JHZeaX.png" width=300>

It's specified by the following Substance and Style programs.

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

Here's how the optimization looks live in the UI. 

<img src="https://github.com/penrose/penrose/blob/master/assets/penrose_readme.gif?raw=true" width=500>
