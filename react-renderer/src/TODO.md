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

---

- make sure genCode deals with weight type
  get rid of hasWeight
  should it actually take graphs...
  and make a 'genGradient' wrapper?
- optimizer: store the partially applied fn in state
  - reapply with new weight in EP

---

works with `sameCenter`
works with `contains`, just doesn't satisfy `maxSize`

TODO: some combination of them results in a NaN

does work instantly to lay it out

TODO: Don't resynthesize code on resample

TODO: Check for convergence in inner loop

TODO: narrow down the NaN

we are in stepBasic not stepEP; rename functions to make it clearer

TODO: Re-profile this
TODO: Put in example synthesized code
TODO: Fix failing constraint tests -- due to number magnitude
TODO: Post results

TODO: Port tree.sty

---

works with `runpenrose set-theory-domain/tree.sub set-theory-domain/venn-opt-test.sty set-theory-domain/setTheory.dsl`

TODO: Fix that the resample hack breaks on switching examples since it saves the cached functions...
TODO: Also breaks if you resample without generating the function on first sample. Clearly this should be part of the state
TODO: The code below (the line) hasn't been ported to web-perf yet

TODO: (for tree.sty)
- repel (with padding) X
- centerArrow
- above
- equal

---

TODO: Document new available functions

Tree repel
    Need inverse

TODO: Document what I need to do to add a new operation
// ADDING A NEW OP: (TODO: document further)
// Add its definition above
// Add it to the opMap
// Add its js mapping (code) to traverseGraph

---

Tree works instantly. TODO: Document the GIF 
TODO: Also finds another symmetric result which TF didn't find earlier: https://www.dropbox.com/s/ef6gzkmra1zsa9q/Screen%20Shot%202020-09-05%20at%2011.38.49%20PM.png?dl=0
(does this satisfy the style program?)

---

TODO 9/14/20

- Port euclidean.sty
- Clean up web-perf code
- Look at what it takes to merge with web-runtime

## Port euclidean.sty

- Have to do it incrementally

Constraints
- atDist
- contains (rect, circle/rect, padding)
- perpendicular (3 tups)

Objectives
- repel (line, rect, weight)
- repel (circle, circle, weight)
- repel (line, rect, weight)
- repel (path, rect, weight)

---

- Need to evaluate lists, tuples

- Need to write evalExprs as a fold
  - nvm, evalExpr doesn't update the translation, and this is now run only once anyway

- Is the NaN problem that variable names don't work for fields?
  `variables:  (2) ["a.x.", "a.y."]`
No, actually the variable names don't matter because we only use JSON.stringify

Why didn't it catch the error in `get`'s type?

TODO: How to set up tide electric indent (C-j)? Enter seems to work, when all lines at end of ~/.emacs are commented out

- Write min X
- Write lt  X
- Write contains on rect-circ X
- Write atDist on labels
  Have to get polygon of text

- Write repel on labels

TODO 9/15/20

- can we even render beziers now?
- This style all requires inline computations
- Ok, I should merge w/ web-runtime
- will require implementing tensor-to-number conversion for paths 

Merge:
- Need to combine web-perf's use of DiffVar (but really, just VarAD) with web-runtime's factoring-out of numbers/autodiff from evaluations
- Probably need to change use of `scalar`, etc. (number <-> Tensor conversion) to number <-> varAD conversion
- Need to fix the linear-algebra-js dependency as suggested on slack

- Is numOf even necessary? I thought that the compiled stuff is just going to set the varying state as NUMBERS not variables
- Evaluator needs to use `VarAD` everywhere, not tensors

    // TODO: Evaluating the shapes for display is still done via interpretation on VarADs; not compiled

Check type of input in walkTranslationCovnert in EngineUtils

Distinguish all uses of varOf with constOf

TODO: throw Error("Pow op unimplemented");

Need to rename stepBasic, minimizeBasic, converged2, etc. to the right names

Delete or convert all the *old functions in constraints

Now:
- Tree works
- Geometry works

  TODO: there is a linalg error, maybe when the frontend is running but the running example (backend) changes
      > Unhandled Rejection (BindingError): Cannot pass deleted object as a pointer of type DenseMatrix*

- Venn steps but line search gets stuck
  step by step, you can see the line search ends w/ "stopping early"
  grad norm is still small 
  autostep: the browser is locked up, so much that the promise fails? why?
      > tslib.es6.js:94 Uncaught (in promise) 2130705432 - Exception catching is disabled, this exception cannot be caught. Compile with -s DISABLE_EXCEPTION_CATCHING=0 or DISABLE_EXCEPTION_CATCHING=2 to catch.

Did some objective/constraint become contradictory?

Overall, are there other obvious things that I missed in the merge?

venn-small doesn't work

could it be a state problem w/ the frontend?
works ok with the small sets example

tree-debug.sub + modified venn-opt-test.sty

Not sure why `contains` is acting up? Or just the combination of them?

Could it be uoStop? Yes (but doesn't rule out line search acting up in the future, for harder problems)

---

Still need to clean up Autodiff, Constraints, Evaluator, Optimizer

---

TODO: Does the new autodiff work on function calls and inline computations in Style??
TODO: Clean up Computations.ts
TODO: Port an example that "pushes back" thru a computation?

9/29/20

- Port DomainInterop.sty
  - Port triangle fn O
  - Port avg fn X
    - Port Path type conversion X
  - Support CompApp in pretty-printer X
  - Port centerLabel for Rect, Text X
  - Port near for 2 cases -- might need arg overload? X
    - near X
    - nearPt X
  - Port disjoint(text, line) X

---

On penrose master, mesh-set-domain
StepUntilConvergence (no display)
- 28 sec
- 28 sec
- 37 sec

Step
- 4-6s for the main inner loop (no display)

Speedup = 6x...

I wonder if the gradients i'm generating are just too large... like maybe it doesn't scale very well.

variables:  (55) ["A.shape.r", "A.text.x", "A.text.y", "B1.shape.r", "B1.shape.x", "B1.shape.y", "B1.text.x", "B1.text.y", "B2.shape.r", "B2.shape.x", "B2.shape.y", "B2.text.x", "B2.text.y", "B3.shape.r", "B3.shape.x", "B3.shape.y", "B3.text.x", "B3.text.y", "B4.shape.r", "B4.shape.x", "B4.shape.y", "B4.text.x", "B4.text.y", "e1.text.x", "e1.text.y", "e2.text.x", "e2.text.y", "e3.text.x", "e3.text.y", "e4.text.x", "e4.text.y", "p1.shape.x", "p1.shape.y", "p2.shape.x", "p2.shape.y", "p3.shape.x", "p3.shape.y", "p4.shape.x", "p4.shape.y", "v1.text.x", "v1.text.y", "v1.xpos.", "v1.ypos.", "v2.text.x", "v2.text.y", "v2.xpos.", "v2.ypos.", "v3.text.x", "v3.text.y", "v3.xpos.", "v3.ypos.", "v4.text.x", "v4.text.y", "v4.xpos.", "v4.ypos."]

fns:  :  1.shape, v2.shape, 1)"
22: "repel(v1.shape, v3.shape, 1)"
23: "repel(v4.shape, v2.shape, 1)"
24: "repel(v4.shape, v3.shape, 1)"
25: "repel(v1.shape, v4.shape, 1)"
26: "repel(v2.shape, v1.shape, 1)"
27: "repel(v2.shape, v3.shape, 1)"
28: "repel(v2.shape, v4.shape, 1)"
29: "repel(v3.shape, v1.shape, 1)"
30: "repel(v3.shape, v2.shape, 1)"
31: "repel(v3.shape, v4.shape, 1)"
32: "repel(v4.shape, v1.shape, 1)"
33: "repel(A.text, B1.text, const.repelWeight)"
34: "repel(A.text, B2.text, const.repelWeight)"
35: "repel(B2.text, B3.text, const.repelWeight)"
36: "repel(B2.text, B4.text, const.repelWeight)"
37: "repel(B3.text, A.text, const.repelWeight)"
38: "repel(B3.text, B1.text, const.repelWeight)"
39: "repel(B3.text, B2.text, const.repelWeight)"
40: "repel(B3.text, B4.text, const.repelWeight)"
41: "repel(B4.text, A.text, const.repelWeight)"
42: "repel(B4.text, B1.text, const.repelWeight)"
43: "repel(B4.text, B2.text, const.repelWeight)"
44: "repel(B4.text, B3.text, const.repelWeight)"
45: "repel(A.text, B3.text, const.repelWeight)"
46: "repel(A.text, B4.text, const.repelWeight)"
47: "repel(B1.text, A.text, const.repelWeight)"
48: "repel(B1.text, B2.text, const.repelWeight)"
49: "repel(B1.text, B3.text, const.repelWeight)"
50: "repel(B1.text, B4.text, const.repelWeight)"
51: "repel(B2.text, A.text, const.repelWeight)"
52: "repel(B2.text, B1.text, const.repelWeight)"
53: "repel(p1.text, p2.text, const.repelWeight)"
54: "repel(p1.text, p3.text, const.repelWeight)"
55: "repel(p4.text, p2.text, const.repelWeight)"
56: "repel(p4.text, p3.text, const.repelWeight)"
57: "repel(p1.text, p4.text, const.repelWeight)"
58: "repel(p2.text, p1.text, const.repelWeight)"
59: "repel(p2.text, p3.text, const.repelWeight)"
60: "repel(p2.text, p4.text, const.repelWeight)"
61: "repel(p3.text, p1.text, const.repelWeight)"
62: "repel(p3.text, p2.text, const.repelWeight)"
63: "repel(p3.text, p4.text, const.repelWeight)"
64: "repel(p4.text, p1.text, const.repelWeight)"
65: "nearPt(e1.text, average2(v2.xpos, v3.xpos), average2(v2.ypos, v3.ypos))"
66: "nearPt(e2.text, average2(v3.xpos, v1.xpos), average2(v3.ypos, v1.ypos))"
67: "nearPt(e3.text, average2(v1.xpos, v2.xpos), average2(v1.ypos, v2.ypos))"
68: "nearPt(e4.text, average2(v3.xpos, v4.xpos), average2(v3.ypos, v4.ypos))"
69: "contains(A.shape, A.text)"
70: "maxSize(A.shape)"
71: "minSize(A.shape)"
72: "contains(B1.shape, B1.text)"
73: "maxSize(B1.shape)"
74: "minSize(B1.shape)"
75: "contains(B2.shape, B2.text)"
76: "maxSize(B2.shape)"
77: "minSize(B2.shape)"
78: "contains(B3.shape, B3.text)"
79: "maxSize(B3.shape)"
80: "minSize(B3.shape)"
81: "contains(B4.shape, B4.text)"
82: "maxSize(B4.shape)"
83: "minSize(B4.shape)"
84: "contains(A.shape, B1.shape, 5)"
85: "outsideOf(A.text, B1.shape)"
86: "smallerThan(B1.shape, A.shape)"
87: "contains(A.shape, B2.shape, 5)"
88: "outsideOf(A.text, B2.shape)"
89: "smallerThan(B2.shape, A.shape)"
90: "contains(A.shape, B3.shape, 5)"
91: "outsideOf(A.text, B3.shape)"
92: "smallerThan(B3.shape, A.shape)"
93: "contains(B2.shape, p1.shape, {"tag":"BinOp","contents":["Multiply",{"tag":"AFloat","contents":{"tag":"Fix","contents":0.3}},{"tag":"EPath","contents":{"tag":"PropertyPath","contents":[{"tag":"BSubVar","contents":"B2"},"shape","r"]}}]})"
94: "contains(B3.shape, p1.shape, {"tag":"BinOp","contents":["Multiply",{"tag":"AFloat","contents":{"tag":"Fix","contents":0.3}},{"tag":"EPath","contents":{"tag":"PropertyPath","contents":[{"tag":"BSubVar","contents":"B3"},"shape","r"]}}]})"
95: "contains(B1.shape, p2.shape, {"tag":"BinOp","contents":["Multiply",{"tag":"AFloat","contents":{"tag":"Fix","contents":0.3}},{"tag":"EPath","contents":{"tag":"PropertyPath","contents":[{"tag":"BSubVar","contents":"B1"},"shape","r"]}}]})"
96: "contains(B3.shape, p2.shape, {"tag":"BinOp","contents":["Multiply",{"tag":"AFloat","contents":{"tag":"Fix","contents":0.3}},{"tag":"EPath","contents":{"tag":"PropertyPath","contents":[{"tag":"BSubVar","contents":"B3"},"shape","r"]}}]})"
97: "contains(B1.shape, p3.shape, {"tag":"BinOp","contents":["Multiply",{"tag":"AFloat","contents":{"tag":"Fix","contents":0.3}},{"tag":"EPath","contents":{"tag":"PropertyPath","contents":[{"tag":"BSubVar","contents":"B1"},"shape","r"]}}]})"
98: "contains(B2.shape, p3.shape, {"tag":"BinOp","contents":["Multiply",{"tag":"AFloat","contents":{"tag":"Fix","contents":0.3}},{"tag":"EPath","contents":{"tag":"PropertyPath","contents":[{"tag":"BSubVar","contents":"B2"},"shape","r"]}}]})"
99: "contains(B4.shape, p3.shape, {"tag":"BinOp","contents":["Multiply",{"tag":"AFloat","contents":{"tag":"Fix","contents":0.3}},{"tag":"EPath","contents":{"tag":"PropertyPath","contents":[{"tag":"BSubVar","contents":"B4"},"shape","r"]}}]})"
[100 … 122]
100: "contains(B4.shape, p4.shape, {"tag":"BinOp","contents":["Multiply",{"tag":"AFloat","contents":{"tag":"Fix","contents":0.3}},{"tag":"EPath","contents":{"tag":"PropertyPath","contents":[{"tag":"BSubVar","contents":"B4"},"shape","r"]}}]})"
101: "disjoint(B1.shape, p1.shape)"
102: "disjoint(B4.shape, p1.shape)"
103: "disjoint(B2.shape, p2.shape)"
104: "disjoint(B4.shape, p2.shape)"
105: "disjoint(B3.shape, p3.shape)"
106: "disjoint(A.shape, p4.shape)"
107: "disjoint(e1.text, e1.shape, 5)"
108: "disjoint(e2.text, e2.shape, 5)"
109: "disjoint(e3.text, e3.shape, 5)"
110: "disjoint(e4.text, e4.shape, 5)"
111: "contains(K.shape, v1.text, 0)"
112: "atDist(v1.shape, v1.text, global.labelPadding2)"
113: "contains(K.shape, v1.shape, v1.shape)"
114: "contains(K.shape, v2.text, 0)"
115: "atDist(v2.shape, v2.text, global.labelPadding2)"
116: "contains(K.shape, v2.shape, v2.shape)"
117: "contains(K.shape, v3.text, 0)"
118: "atDist(v3.shape, v3.text, global.labelPadding2)"
119: "contains(K.shape, v3.shape, v3.shape)"
120: "contains(K.shape, v4.text, 0)"
121: "atDist(v4.shape, v4.text, global.labelPadding2)"
122: "contains(K.shape, v4.shape, v4.shape)"
