## TODO

- Generalize tests
  - Test at a couple of different points
  - Factor out graph building code from test1
- Convert test2 to check grad results programmatically
- Check the tests
- Look at more concise generated code

======

Debug this zero grad things
    Fix the minimal example (gradGraph3) to take in all the args
        Probably the args are misnumbered because the counter keeps going even when we encounter an unused var

               INPUTS           OUTPUTS
    Energy   | known          | only 1
    Gradient | ENERGY inputs  | grad nodes (attached to the original energy input nodes)

    Need to put optimizer l665 changes back in, also sameCenter change

    Why aren't there any args in the compiled energy?

    Nvm, after using debugger, seems to have energy just fine

    OK, the gradient is almost right, but in the wrong order b/t energy/estimated and grad?
    The energy is wrong; it needs to use all the inputs

>>> Fixed, looks good. Just clean up the inputs/outputs code in genCode
>>> Get gradGraph4 to work
>>> Also revert the sameCenter thing, test fully
>>> Also revert the objfn stuff, test fully

    Put gradGraph 4 back in the test
    Evaluate ifcond and gt
    Where did the Math.max go?
    Is there a problem with evalEnergyOnCustom?
    for grad of max, needs to support IfCond generation / evaluation

- Check grad results for programmatic function
  remove throw Error in `energyAndGradDynamic`
      also rename this function

- Write more hardcoded tests
- Lay out diagram
  - Only compile gradient and energy once, in the beginning, and store it
  - Use it in the opt and line search
