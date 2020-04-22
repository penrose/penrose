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

// e : Num
const tfVar = e => tf.scalar(e).variable();

// [a] -> [b] -> { a : b }
const tups2obj = (xs, ys) => Object.fromEntries(_.zip(xs, ys));

const getVaryingState = (paths, graph) => paths.map(path => graph[path]);

const genState = (objFns, compGraph, varyingPaths) => {
    console.log("vp", varyingPaths);
    // Look up in comp graph
    let varyingState = getVaryingState(varyingPaths, compGraph); // these are NOT tf vars

    let state = {
	compGraph : compGraph,
	objFns : objFns,
	varyingPaths : varyingPaths,
	varyingState : varyingState
    }

    return state;
};

const evalEnergyOn = (compGraph, varying) => {
    console.log("TODO: evalEnergyOn");
};

// Minimize f over x
const minimize = (f, xs, names) => {
    let res;

    // Use included tf.js optimizer for now
    // TODO profile this; change # iterations (test for convergence) and LR
    // TODO: Calculate + print norm of gradient; evaluate until convergence, and check against the right answer
    for (let i = 0; i < 100; i++) {
    	res = optimizer2.minimize(() => f(...xs), returnCost=true, varList=xs);

	// TODO: note clearly that `xs` is mutable; maybe make a copy of it at beginning. Does it do it for speed?
	// TODO: this printing could tank the performance
	let vals = xs.map(v => v.dataSync()[0]);
	console.log("state", tups2obj(names, vals));
	console.log(`F[v]: ${res}`);
    }

    return res;
};

// NOTE: This compiles the arg into code ... by evaluating it? or returning it as data? or? (TODO figure out what's going on here)
// WN suggestion: just have a dictionary and look up as if already evaluated. Just want entries in compGraph of different data types, and make sure it works. Plain js `typeof` is fine?
let compileArg = (argName, compGraph) => {
    let res = compGraph[argName];
    // Problem: Need to discriminate on type
    return res;
};

// type Translation<T> = ITrans<T>;
// interface ITrans<T> {
//     trMap: Map<string, Map<string, FieldExpr<T>>>;
//     warnings: string[];
// }

// This should really be evalEnergyOn, which requires functions: evalFns, mkVaryMap, applyCombined

// Compile the opt problem in `objFns_input` (which uses names) into a multivariate function on floats, and look up the initial state
let compileOptProblem = (state) => { // State -> ([a] -> a)
    ({compGraph, objFns, varyingPaths, varyingState} = state);

    let varyingMap = tups2obj(varyingPaths, varyingState);

    // Look up function name in function library
    // TODO: look up all functions, not just the first
    let overallFn = objFn_library[objFns[0].fnName];

    // TODO: for each function's argument, look it up / evaluate it in compGraph
    // TODO: Make the evaluation of `F` richer to account for the compGraph, using the three inputs
    // Need to eval all args, create composite function

    // TODO: Make the varyingMap from the compGraph
    // Need to write a lookupPath function and all of the helpers

    // Look up each argument's varying value in varyingMap (currently a TF variable, TODO make this an expr?)
    // NOTE: These are the tfVars, so only *these* will be mutated by the TF optimization
    let argVals = objFns[0].args.map(argName => compileArg(argName, compGraph));
    let argValsTF = argVals.map(tfVar);

    return { F: overallFn,
	     x: argValsTF }; // TODO: When should things become TF-ified?
};

// ----------------------

// SYSTEM LIBRARIES
// All currently written with tfjs types

// TODO: Figure out how to schedule computations as tensors?
// objFn :: [a] -> a
// TODO: Make the input size and type richer
let objFn_library = { 
    min2 : (x, y) => x.square().add(y.square()), // x^2 + y^2
    equal2 : (x, y) => x.sub(y).square() // (x - y)^2
};

let compFn_library = {
    plus2 : (x, y) => x.add(y)
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
let compGraph1 = {
    "A.shape.x" : 1.0,
    "B.shape.y" : ["plus2", "C.shape.r", 4.0], // B.shape.y := C.shape.r + 4.0
    "C.shape.r" : 5.0
};

let varyingPaths1 = ["A.shape.x", "C.shape.r"];

let state1 = genState(objFns1, compGraph1, varyingPaths1);

// ----------------------

// CHOSEN STYLE INPUT (State)

let state = state0; // <--- This is the only changeable input

// SYSTEM RUNTIME CODE

// Compile out all names/references? F : [a] -> a, X0 : [a]
let pure_opt_problem = compileOptProblem(state)
console.log("opt problem", pure_opt_problem);

// the pure_opt_problem.x gets mutated by the tf optimization on each round
let lastEnergy = minimize(pure_opt_problem.F, pure_opt_problem.x, varyingPaths);

// NOTE: The varyingState in the state is going to be stale, as is the compGraph, since they are not tfVars they don't get updated
let varyingStateFinal = pure_opt_problem.x;
let varyingMap = tups2obj(state.varyingPaths, varyingStateFinal);

console.log("varyingMap", varyingMap);

// For some reason it comes out as a list of single-elem-list of Float32Array
console.log("varyingMap", Object.values(varyingMap).map(v => v.dataSync()[0]));
console.log(`F[v]: ${lastEnergy}`);
