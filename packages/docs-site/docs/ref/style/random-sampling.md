# Random Sampling

Several functions allow values or points to be sampled at random. These values are effectively fixed constants that depend only on the current `variation` (which determines the random seed). Unlike unknown values `?`, they cannot be optimized—just as constants like `1.23` cannot be optimized. For instance, in the Style code

```
scalar x = random(0,100)
encourage x == 50
```

the `encourage` statement will have no effect on the value of `x`. Randomly-sampled constants can of course be used to define other quantities, which can in turn be optimized. For instance,

```
-- construct a vector v
scalar L = random(1,2) -- pick a random length between 1 and 2
scalar θ = ? -- angle is unknown
vec2 v = L * ( cos(θ),sin(θ) )

-- try to get this vector to meet the point `p`
encourage norm( v - p ) == 0
```

Here, Penrose can rotate the vector in order to make it reach `p`, but it cannot grow or shrink its length `L` (which is a random, but fixed constant value). In short, the guiding principle is that _randomly-sampled values will behave like constants_.

## Random Sampling Functions

Functions that perform random sampling generally have the suffix `random`. For instance:

- `random( minVal, maxVal )` — samples a random constant uniformly from the range `[minVal,maxVal]`
- `unitRandom()` — samples a random constant uniformly from the range `[0,1]`.
- `normalRandom()` — samples a random constant from a normal distribution with mean 0 and standard deviation 1.
- `diskRandom()` — samples a random point uniformly from the unit disk.
- `sphereRandom()` — samples a random point uniformly from the unit sphere.
- `triangleRandom(a,b,c)` — samples a random point uniformly from a triangle with vertices `a`, `b`, `c`.

Additional random sampling functions may be available—refer to the [function library](functions) for further information.
