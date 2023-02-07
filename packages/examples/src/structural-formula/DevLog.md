## Modeling the domain

Found it hard to decide whether bonds should be a predicate or a type.
One idea is to have bonds be an explicit type, and then use a constructor to bond atoms together.
Another idea is to just have a predicate that says atoms should be bonded together.
Settled on the predicate solution because (annoyingly...) it seems that there's no such thing as an anonymous substance variable. Hence, I would have to explicitly name every bond I wanted to create. It might be useful to name bonds _sometimes_, but is overkill/annoying to have to name them all the time. On the other hand this is a bit sad, because I would have liked to have a type Bond and subtypes SingleBond, DoubleBond, etc., where I could just make slight modifications to how the subtypes are drawn in Style. As things stand, I have to replicate code for different bond types.

I also found it a little annoying that you have to give subtype relationships on a different line than the type declaration. E.g.,

```
type BaseType
type SubType
SubType <: BaseType
```

rather than just

```
type BaseType
type SubType <: BaseType
```

This is especially annoying in this context, since I want to have a type for every element on the periodic table, and each of those types needs to be a subtype of `Atom`. (E.g., `Carbon <: Atom`.)
Opened this issue: <https://github.com/penrose/penrose/issues/722>

## Implementing the Style

It was hard to understand how local variables currently work, whether they need types, whether they can be anonymous, etc.
I find it weird that local variables (seemingly?) can't be anonymous. E.g., if I want to associate a shape with a predicate, why do I have to name that shape? I created an issue here: <https://github.com/penrose/penrose/issues/720>

There still aren't clear error messages---for instance, I was stumped for a while about why my `Path` wasn't getting drawn. The reason is that I had set the field `path:` rather than `pathData:` (but no warning or error was issued).

I tried adding an SVG `Image` to my Style, by placing the SVG file in the same directory as the `.style` file, but nothing showed up. Seems it has to go in `penrose/packages/core/assets/`. And it's not even clear how to re-build the system so that it recognizes new SVGs in that directory. Added an issue here: <https://github.com/penrose/penrose/issues/723>

For one of my Styles, I needed rounded line caps, which is supported in SVG but was not yet exposed in Penrose. I was able to add line caps via "search-copy-paste-edit," but would be nice to think about how to expose a more comprehensive set of SVG features in a more orderly fashion.

At some point the layout engine barfed with the message

```
Error: no shape ordering possible from layering
```

While this is technically true, it's a bit sad that we don't fail gracefully here: issue a _warning_ rather than an error, and give up on finding a consistent ordering (but sort of keep whatever partial ordering we had up until that point).

## Writing the Substance

I found it hard to track down code for how to label things (i.e., just to locate the page in the docs). In general docs are a bit scattered around. Eventually I found this page: <https://github.com/penrose/penrose/wiki/Labels-in-Penrose>

However, I didn't realize until now that labels _have_ to be given in TeX; we can't just use plain-text labels. I created an issue here: <https://github.com/penrose/penrose/issues/721>

## Running the programs

Things worked out ok for a small example like a water molecule (just two hydrogens and an oxygen). But when I tried running a bigger molecule (caffeine) everything slowed down considerably. I think this has to do with having an all-pairs `encourage` statement, where every atom tries to reach a certain distance from every other. Here, it seems like _compiling_ is what is taking forever; once an initial compilation step has completed (it seems), it's fast to resample and optimize the diagram.

The actual layout also leaves something to be desired. Basically this is a graph layout problem, which I'm trying to hack into the Penrose optimizer. The way I do this currently is I ensure that I ask for atoms that are connected by a bond to be a target distance from each-other, then for all pairs of atoms (whether or not connected by a bond) I encourage a repulsive energy to be close to zero.

For small molecules (with fewer than 10 atoms, i.e., fewer than 10 nodes in the graph) this tends to work ok; for larger molecules, if I keep hitting "resample" and get very, very lucky, I eventually get a decent layout without confusing edge overlaps.

So, the next attempted improvement is to try to "encourage" edges not to overlap. To do this, I want to minimize a function of some signed areas defined by the four endpoints. But currently there's no way to simply encourage some quantity to take the smallest value it possibly can. So, let's try to define one.
