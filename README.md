# Penrose

Not ready for contributions or public use yet, but hopefully will be soon! 

* See [the site](penrose.ink) for more information and examples. 
* See the [wiki](https://github.com/penrose/penrose/wiki) for more system-specific information on building, running, testing, and debugging the system. 
* For even more documentation, see Nimo Ni's [README](https://github.com/wodeni/notes-pub/blob/master/penrose/ramp-down.md).

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
