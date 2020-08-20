# Test case 1

Input energy function:

```
energyGraph.gradNode = { tag: "Just", contents: variableAD(1.0) };
const x0 = markInput(variableAD(-5.0), 0);
const x1 = markInput(variableAD(6.0), 1);
const a = sub(x0, x1);
const b = squared(a);
const c = add(a, variableAD(3.0)); // const
const z = mul(b, c);
```

Diagram:

```
     z
   /  \
  b    c
    \ / \
     a   3
    / \  
   x0  x1
```

Output energy code:

```
generated f ƒ anonymous(x0,x1
) {
const x2 = x0 - x1;
const x3 = Math.pow(x2, 2);
const x4 = 3;
const x5 = x2 + x4;
const x6 = x3 * x5;
return x6;
}
```
