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
(function anonymous(x2,x3
) {
const x0 = 1;
const x1 = 2;
const x4 = x2 - x3;
const x5 = x1 * x4;
const x6 = Math.sin(x4);
const x7 = 1;
const x8 = x6 * x7;
const x9 = x8;
const x10 = x5 * x9;
const x11 = Math.cos(x4);
const x12 = Math.pow(x4, 2);
const x13 = x12 * x7;
const x14 = x13;
const x15 = x11 * x14;
const x16 = x10 + x15;
const x17 = x0 * x16;
const x18 = x17;
const x19 = -1;
const x20 = x19 * x16;
const x21 = x20;
return [x18, x21];
})
```

`grad f [5.0, 6.0] = 2.2232442754839328`

`df/dx at [5, 8] = [-8.063212421044806, 8.063212421044806]`

**Correct**: estimated gradient matches analytic gradient
