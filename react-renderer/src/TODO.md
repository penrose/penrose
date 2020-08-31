## TODO

- Generalize tests
  - Test at a couple of different points
  - Factor out graph building code from test1
- Convert test2 to check grad results programmatically
- Check the tests
- Look at more concise generated code

               INPUTS           OUTPUTS
    Energy   | known          | only 1
    Gradient | ENERGY inputs  | grad nodes (attached to the original energy input nodes)

>>> Get gradGraph4 to work
    Put gradGraph 4 back in the test
    Evaluate ifcond and gt
    Where did the Math.max go?
    Is there a problem with evalEnergyOnCustom?
    for grad of max, needs to support IfCond generation / evaluation

>>> Also revert the sameCenter thing, test fully
>>> Also revert the objfn stuff, test fully

- Check grad results for programmatic function
  remove throw Error in `energyAndGradDynamic`
      also rename this function

- Write more hardcoded tests

- Lay out diagram
  - Only compile gradient and energy once, in the beginning, and store it
  - Profile this
  - Use it in the opt and line search

======

- account for the weights of the optimization as parameters to the function, and partially apply it
- add back the constraints in venn-small.sty
  - fix n-ary problem
- lay out and benchmark

- figure out why line search is behaving oddly on venn-small.sty

- make this work for resampling

===

revert sameCenter and venn-small.sty again
swap default hasWeight from false to true

TODO: fix the design for hyperparameters

- types:
  GradGraphs should have a field for hyperparameters, 
  weight: Maybe<VarAD>

- to generate fns: IF there is a weight,
  hyperparameters are just treated as inputs that come before all the other ones
      f0(x0, x1...xn) <-- x0 might be a hyperparameter, 
        { x_{n+1} = ... x0 + x1 ... } <-- the body of the function might refer to the hyperparameter just like a normal input
      similar for grads:
      gradf0(x0, x1...) <-- x0 might be a hyperparameter, etc.

  then, each function is curried with its first argument only
      f(x0)(x1,...) = f0(x0, x1,...)
      gradf(x0)(x1,...) = f0(x0, x1,...)

  if no weight, just gen the fn w/ no currying

- to use: IF there is a weight,
  hyperparameters are treated as partial applications for both the energy and the gradient
  f(hs0)(xs0): number
  gradf(hs0)(xs0): number[]

  can (and should) be applied with different hyperparameters across EP runs

  f(hs1)(xs1)
  gradf(hs1)(xs1)

  if no weight, just apply the fn w/ no partial application
