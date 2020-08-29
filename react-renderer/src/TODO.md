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
