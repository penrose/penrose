(Don't review this file; turning it into documentation)

# Old comments

NOTE: Only when you apply a special operation, it mutates the variable(s) (IVarAD reference(s)) to add a reference to its parent (the result of the op) to both. That means the rest of the code doesn't matter, only the ops do (except for keeping the objfn's form constant between iterations). 
You just need to hold onto the references to your vars

You will need to zero grads on all the bottom nodes (varying vars) because they will still have the parent refs and grad vals attached (Unless that's done automatically by the grad function)

NOTE: `evalFn` calls `evalExpr` with `autodiff = true`. It makes everything (base vals) differentiable when encountered

# API

Interpret energy: xs -> list of xs vars
Compile energy: the var result (z) -> function ([x] -> x)
Grad (f, xs): takes the list of xs vars, (which carry f) as well as a list of scalars
  zeroes the whole graph's sensitivities, vals, gradVals
  TODO: Fill in with the new pipeline

# Examples


```
     Z (+)
    / \
   v   v
X (^2)  Y (-)
   \   / \
    v v   v
     A    B
```

  That generates the code: (TODO: name vars correctly in example)

```
Z := X + Y
X := A^2
Y := A - B
A := 5.0
B := 2.4
```

`Grad Z(A, B) = [dZ/dA, dZ/dB]`

`Z = X + Y = A^2 + (A - B) ==> Grad Z(A, B) = [2A + 1, -1]`
