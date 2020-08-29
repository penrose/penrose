# Test 1 with codegen

Input energy function:

```
const ref = markInput(variableAD(100.0), 0);
const head = squared(ref);
```


Generated energy:

```
generated f ƒ anonymous(x0
) {
const x1 = Math.pow(x0, 2);
return x1;
}
```

`evaluated: f(5) = 25`

`estimated gradient at 5: 9.999999999999787`

Generated gradient:

```
generated f ƒ anonymous(x1
) {
const x0 = 2;
const x2 = x0 * x1;
const x3 = 1;
const x4 = x2 * x3;
const x5 = x4;
return x5;
}
```

`evaluated: f'(5) = 10`

**Correct**: estimated gradient matches analytic gradient

# Test 2 with codegen

Input energy function:

```
const x0 = markInput(variableAD(-5.0), 0);
const x1 = markInput(variableAD(6.0), 1);
const a = sub(x0, x1);
const b = squared(a);
const c = sin(a);
const z = mul(b, c);
```

Diagram:

```
     z
   /  \
  b    c
    \ / 
     a   
    / \  
   x0  x1
```

Generated energy:

```
generated f ƒ anonymous(x0,x1
) {
const x2 = x0 - x1;
const x3 = Math.pow(x2, 2);
const x4 = Math.sin(x2);
const x5 = x3 * x4;
return x5;
}
```

`f(3, 2) = -23.973106866578462`

`estimated gradient at [5, 8] = [-8.063212420845733, 8.063212420845733]`

(Using finite differences, EPSG = 10e-5)

Generated gradient:

```
(function anonymous(x0,x1
) {
const x2 = 1;
const x3 = 2;
const x4 = x0 - x1;
const x5 = x3 * x4;
const x6 = 3;
const x7 = x4 + x6;
const x8 = 1;
const x9 = x7 * x8;
const x10 = x5 * x9;
const x11 = 1;
const x12 = Math.pow(x4, 2);
const x13 = x12 * x8;
const x14 = x11 * x13;
const x15 = x10 + x14;
const x16 = x2 * x15;
const x17 = -1;
const x18 = x17 * x15;
return [x16, x18];
})
```

`grad f [5.0, 6.0] = 2.2232442754839328`

`df/dx at [5, 8] = [-8.063212421044806, 8.063212421044806]`

**Correct**: estimated gradient matches analytic gradient

# Test 4 (`toPenalty`)

This case tests generating a piecewise derivative (i.e. has an if-statement).

Generated energy

```
(function anonymous(x0
) {
const x1 = 0;
const x2 = Math.max(x0, x1);
const x3 = Math.pow(x2, 2);
return x3;
})
```

Generated gradient

```
(function anonymous(x0
) {
const x1 = 0;
const x2 = x0 > x1;
const x3 = 1;
const x4 = 0;
const x5 = x2 ? x3 : x4;
const x6 = 2;
const x7 = Math.max(x0, x1);
const x8 = x6 * x7;
const x9 = 1;
const x10 = x8 * x9;
const x11 = x5 * x10;
return [x11];
})
```

Passes tests (comparing finite differences with analytic gradients, evaluated at 5 random points in [-50, 50])

## Real test 0 (two `squared`s)

`runpenrose set-theory-domain/twosets-simple.sub set-theory-domain/venn-small.sty set-theory-domain/setTheory.dsl`

Only one objective with 10 vars total. Obj defined as (just for testing purposes):

```
    sameCenter: ([t1, s1]: [string, any], [t2, s2]: [string, any]) =>
        squared(s1.x.contents),
```

And it applies to two sets.

Gen. energy:

```
(function anonymous(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9
) {
const x10 = 0;
const x11 = Math.pow(x3, 2);
const x12 = x10 + x11;
const x13 = Math.pow(x8, 2);
const x14 = x12 + x13;
const x15 = 0;
const x16 = 100000;
const x17 = 0.01;
const x18 = x16 * x17;
const x19 = x15 * x18;
const x20 = x14 + x19;
return x20;
})
```

Gen. gradient

```
(function anonymous(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9
) {
const x10 = 0;
const x11 = 0;
const x12 = 0;
const x13 = 2;
const x14 = x13 * x3;
const x15 = 1;
const x16 = 1;
const x17 = 1;
const x18 = 1;
const x19 = x17 * x18;
const x20 = x16 * x19;
const x21 = x15 * x20;
const x22 = x14 * x21;
const x23 = 0;
const x24 = 0;
const x25 = 0;
const x26 = 0;
const x27 = 2;
const x28 = x27 * x8;
const x29 = 1;
const x30 = x29 * x19;
const x31 = x28 * x30;
const x32 = 0;
return [x10, x11, x12, x22, x23, x24, x25, x26, x31, x32];
})
```

Passes tests

## Real test 1 (two `sameCenter`s)

`runpenrose set-theory-domain/twosets-simple.sub set-theory-domain/venn-small.sty set-theory-domain/setTheory.dsl`

Only one objective with 10 vars total. Real obj this time:

```
    sameCenter: ([t1, s1]: [string, any], [t2, s2]: [string, any]) =>
        distsq(center(s1), center(s2)),

```

And it applies to two sets.

Gen. energy:

```
(function anonymous(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9
) {
const x10 = 0;
const x11 = 0;
const x12 = x3 - x1;
const x13 = Math.pow(x12, 2);
const x14 = x11 + x13;
const x15 = x4 - x2;
const x16 = Math.pow(x15, 2);
const x17 = x14 + x16;
const x18 = x10 + x17;
const x19 = 0;
const x20 = x8 - x6;
const x21 = Math.pow(x20, 2);
const x22 = x19 + x21;
const x23 = x9 - x7;
const x24 = Math.pow(x23, 2);
const x25 = x22 + x24;
const x26 = x18 + x25;
const x27 = 0;
const x28 = 100000;
const x29 = 0.01;
const x30 = x28 * x29;
const x31 = x27 * x30;
const x32 = x26 + x31;
return x32;
})
```

Gen. gradient

```
(function anonymous(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9
) {
const x10 = 0;
const x11 = -1;
const x12 = 2;
const x13 = x3 - x1;
const x14 = x12 * x13;
const x15 = 1;
const x16 = 1;
const x17 = 1;
const x18 = 1;
const x19 = 1;
const x20 = 1;
const x21 = x19 * x20;
const x22 = x18 * x21;
const x23 = x17 * x22;
const x24 = x16 * x23;
const x25 = x15 * x24;
const x26 = x14 * x25;
const x27 = x11 * x26;
const x28 = -1;
const x29 = 2;
const x30 = x4 - x2;
const x31 = x29 * x30;
const x32 = 1;
const x33 = x32 * x23;
const x34 = x31 * x33;
const x35 = x28 * x34;
const x36 = 1;
const x37 = x36 * x26;
const x38 = 1;
const x39 = x38 * x34;
const x40 = 0;
const x41 = -1;
const x42 = 2;
const x43 = x8 - x6;
const x44 = x42 * x43;
const x45 = 1;
const x46 = 1;
const x47 = 1;
const x48 = x47 * x21;
const x49 = x46 * x48;
const x50 = x45 * x49;
const x51 = x44 * x50;
const x52 = x41 * x51;
const x53 = -1;
const x54 = 2;
const x55 = x9 - x7;
const x56 = x54 * x55;
const x57 = 1;
const x58 = x57 * x48;
const x59 = x56 * x58;
const x60 = x53 * x59;
const x61 = 1;
const x62 = x61 * x51;
const x63 = 1;
const x64 = x63 * x59;
return [x10, x27, x35, x37, x39, x40, x52, x60, x62, x64];
})
```

Passes tests

## Performance for two `sameCenters`, 10 DOF

### Without line search

**10k steps in 44 ms = 227,272 steps/second**

does not converge (but that's fine since no line search)
||grad f(x)|| = 29.296048379727498
energy = 107.2823063334168

### With line search

10k steps in 155ms
converges with energy 0

(10k steps / 155ms) = 64.5 steps/ms = 64516 steps/second
