// Use: open index.html in browser; check console

// Goals:
// Define clear interface to translation (What operations does the optimizer want to perform on the translation?)
// (Opt types = optimizer types)
// Define boundary between "Style types" and opt types 
// Define boundary between system internal types (i.e. those in the translation) and opt types
// Figure out what conversion needs to happen between Style functions and tfjs functions
// Figure out the speed hit for conversion
// Benchmark tfjs solver (speed/perf) vs. Penrose solver

// TODO: Scope the goal
// Port objectives/constraints for just the one example and write them in terms of the tfjs syntax, then optimize and lay out diagram
// Port evalEnergyOn function from GenOptProblem
// Port Optimizer module
    // Port step fn, stepShapes, gradient descent, line search, L-BFGS
// Port Functions and Utils modules
// What is useful for testing the evaluator ASAP?

// Who else has written code like this in js? What does it look like, and how to make it go fast?
// What does it take to compile Style functions into tfjs functions?

// First steps?
// Maybe solve a more general problem, that's not hardcoded?
// Get more general gradients? 
// Port more of the functions library to tfjs format?

// ------------------------------

// SYSTEM COMPILER + OPTIMIZATION CODE

// TODO: remove the first script, which clobbers the scope

// CONSTS
// const learningRate = 0.01; // TODO Try different learning rates
// const optimizer1 = tf.train.sgd(learningRate);
// const optimizer2 = tf.train.adam(learningRate);

const assertEq = (x, y) => {
    if (x === y) { return; }

    console.log("assert failed: x === y", x, y); 
    return undefined[0]; 
}

// e : Num
const tfVar = e => tf.scalar(e).variable();

const tfStr = x => x.dataSync()[0];
const tfsStr = xs => xs.map(e => tfStr(e));

// [a] -> [b] -> { a : b }
const tups2obj = (xs, ys) => Object.fromEntries(_.zip(xs, ys));

const getVaryingState = (paths, graph) => paths.map(path => graph[path]);

const genState = (objFns, compGraph, varyingPaths) => {
    console.log("vp", varyingPaths);
    // Look up in comp graph
    let varyingState = getVaryingState(varyingPaths, compGraph);

    let state = {
	compGraph : compGraph,
	objFns : objFns,
	varyingPaths : varyingPaths,
	varyingState : varyingState
    }

    return state;
};

// Minimize f over x
const minimize = (f, xs, names) => {
    let res;

    console.log("f", f);
    console.log("xs", tfsStr(xs));

    // Use included tf.js optimizer for now
    // TODO profile this; change # iterations (test for convergence) and LR
    // TODO: Calculate + print norm of gradient; evaluate until convergence, and check against the right answer
    const n = 300;
    for (let i = 0; i < n; i++) {
	// TODO: use/revert spread, also doesn't work with varList=xs and compGraph1
    	res = optimizer2.minimize(() => f(xs), returnCost=true);

	// TODO: note clearly that `xs` is mutable; maybe make a copy of it at beginning. Does it do it for speed?
	// TODO: this printing could tank the performance
	let vals = xs.map(v => v.dataSync()[0]);
	console.log("state", tups2obj(names, vals));
	console.log(`f(xs): ${res}`);
    }

    return res;
};

// expr should be an expr (of the kind stored in the compgraph -- either string or number)
// returns a tfvar (of float)
// NOTE: DO NOT MAKE NEW TFVARS
let evalExprVarying = (compGraph, varyingMap, expr) => {
    console.log("eval", expr);

    // NOTE: tfvars are made on the fly from consts so we can do math with them, but they are not (should not be) mutable vars, since they are not in mutableState

    // Evaluate a literal (e.g. tfvar number)
    if (typeof(expr) === "number") { return tfVar(expr); }

    // Evaluate a path
    // TODO: Paths treated as single strings; we don't distinguish path types from string literals yet
    assertEq(typeof(expr), "string"); 

    // Short-circuit: look up path in varyingMap
    // Leaf node, e.g. A.shape.x -> val
    // This is not just an optimization; the values in compGraph for a varying value will be stale. So you have to find them in varyingMap.
    let varyingVal = varyingMap[expr];
    if (varyingVal) { return varyingVal; } // already tfvar

    let exprDef = compGraph[expr]; // otherwise, in comp graph

    // Base case: expr path points to literal (tfvar number)
    if (typeof(exprDef) === "number") { return tfVar(expr); }

    // Recursive case, e.g. B.shape.y -> C.shape.r + 4.0
    // Assuming exprs have schema { fnName, args }
    let exprFn = compFn_library[exprDef.fnName];
    // Evaluate each arg, then apply the function and return the result
    // TODO: cache the results (do a fold with compGraph)
    let exprArgVals = exprDef.args.map(arg => evalExprVarying(compGraph, varyingMap, arg));
    console.log("exprArgValsTF", tfsStr(exprArgVals));
    let val = exprFn(...exprArgVals);
    // TODO: fix/note spread

    return val;
};

// Compile the opt problem in `objFns_input` (which uses names) into a multivariate function on floats
// Corresponds to `evalEnergyOn`
let evalOptProblem = (state) => { // State -> ([a] -> a)
    ({compGraph, objFns, varyingPaths, varyingState} = state);

    return (vstate) => { // [a] (tfvars)
	console.log("vstate", vstate);
	console.log("vstate str", tfsStr(vstate));
	let varyingMap = tups2obj(varyingPaths, vstate); // NOTE: Need to use vstate, obviously, otherwise tfjs will complain
	console.log("varyingMap", varyingMap);

	let objFn = objFns[0]; // TODO: write evalFns (across multiple fns)
	let objFnCode = objFn_library[objFn.fnName];
	let argNames = objFn.args;

	// TODO: eval each arg individually, rather than just doing a lookup
	// TODO: compGraph might be stale?
	let argValsTF = argNames.map(argName => evalExprVarying(compGraph, varyingMap, argName)); // NOTE: These are already tfvars

	console.log("argValsTF", tfsStr(argValsTF));

	return objFnCode(...argValsTF);
    };
};

// ----------------------

// SYSTEM LIBRARIES
// All currently written with tfjs types

// TODO: Figure out how to schedule computations as tensors?
// objFn :: [a] -> a
// TODO: Make the input size and type richer. Should they take tuples or lists?

let objFn_library = { 
    min2 : (x, y) => x.square().add(y.square()), // x^2 + y^2
    equal2 : (x, y) => x.sub(y).square() // (x - y)^2
};

let compFn_library = {
    plus2 : (x, y) => x.add(y),
    mul2 : (x, y) => x.mul(y),
};

// -----------------------

// POSSIBLE STYLE INPUTS

// TODO: Make this more generic (multiple fns and args) and test the compilation behaves correctly programmatically
// (With test cases)
let objFns0 = [ { fnName: "min2",
		  args: ["A.shape.x", "B.shape.y"] }
	      ];

// No intermediate nodes
let compGraph0 = {
    "A.shape.x" : 1.0, // TODO: Initialize varying vars in the right way
    "B.shape.y" : 5.0
};

let varyingPaths0 = ["A.shape.x", "B.shape.y"];

let state0 = genState(objFns0, compGraph0, varyingPaths0);
    
// ------

// NOTE: state1 doesn't work yet

let objFns1 = [ { fnName: "min2",
		  args: ["A.shape.x", "B.shape.y"] }
	      ];

// One intermediate node
// (Combined with varyingMap)

// gradient of f(x, y) = x^2 + (4 * y)^2 = (2x, 32y)
// evaluated at (1, 5) = (2, 160)

let compGraph1 = {
    "A.shape.x" : 1.0,
    // "B.shape.y" : { fnName: "plus2", args: ["C.shape.r", tfVar(4.0)] }, 
    "B.shape.y" : { fnName: "mul2", args: ["C.shape.r", 4.0] }, 
    // B.shape.y := C.shape.r + 4.0
    "C.shape.r" : 5.0
};

let varyingPaths1 = ["A.shape.x", "C.shape.r"];

let state1 = genState(objFns1, compGraph1, varyingPaths1);

// ----------------------

// CHOSEN STYLE INPUT (State)

let state = state1; // <--- This is the only changeable input

// SYSTEM RUNTIME CODE

// Compile out all names/references? F : [a] -> a, X0 : [a]
let pure_opt_problem = evalOptProblem(state);
// Note: only the varying vals have a top-level tfvar reference
// Consts are converted to tfvals on the fly to evaluate expressions
// Values in the compgraph are intentionally consts/js vals only
let mutableState = state.varyingState.map(e => tfVar(e)); // [a] (tfvar)

console.log("opt problem", pure_opt_problem);
console.log("mutableState", tfsStr(mutableState));
console.log("f(x0) = ", tfStr(pure_opt_problem(mutableState)));

// ------------------ Grads only with tf

// TODO: change it so mutablestate contains tfvars (w/ starting grad 0) and compgraph contains no tfvars?

// TODO: check if the compgraph and varyingState are stale or updated

// TODO: Note conventions: compgraph can't be copied if it contains tfvars

// TODO: manually opt w/ gradients

// TODO: when manually optimizing w/ gradients, do tfvars' gradients need to be zeroed after each round? what's the best way to do that?

// TODO: Fix arg spreading/arity for grads. Seems like we need to know the number of vars ahead of time

// NOTE: grad works for state1; haven't checked opt for state1

let gradF = tf.grads((x, y) => pure_opt_problem([x, y]));
let gradFx0 = gradF(mutableState);
console.log("f'(x0)", tfsStr(gradFx0));

// ------------------ Opt with tf

// TODO: Check whether the opt results are right
// TODO: For instance, equal2 on state1 doesn't seem to make A.shape.x = B.shape.y?

// the mutableState gets mutated by the tf optimization on each round
let lastEnergy = minimize(pure_opt_problem, mutableState, varyingPaths);
// let lastEnergy = minimize(objFn_library["min2"], mutableState, varyingPaths);
// let lastEnergy = minimize(objFn_library["min2"], [tfVar(1.0), tfVar(5.0)], varyingPaths);

let varyingStateFinal = mutableState;
let varyingMap = tups2obj(state.varyingPaths, varyingStateFinal);

// For some reason it comes out as a list of single-elem-list of Float32Array
console.log("varyingMap", Object.values(varyingMap).map(v => v.dataSync()[0]));
console.log(`f(x_f): ${lastEnergy}`);
