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

const evalEnergyOn = (compGraph, varying) => {
    console.log("TODO: evalEnergyOn");
};

// Minimize f over x
const minimize = (f, x) => {
    let res;

    // Use included tf.js optimizer for now
    // TODO profile this; change # iterations (test for convergence) and LR
    for (let i = 0; i < 100; i++) {
    	res = optimizer2.minimize(() => f(...x), returnCost=true, varList=x);
    }

    return res;
};

// NOTE: This compiles the arg into code ... by evaluating it? or returning it as data? or? (TODO figure out what's going on here)
let compileArg = (argName, compGraph) => {
    
    let res = compGraph[argName];

    // Problem: Need to disciminate on type

    return res;
};

// type Translation<T> = ITrans<T>;
// interface ITrans<T> {
//     trMap: Map<string, Map<string, FieldExpr<T>>>;
//     warnings: string[];
// }

// This should really be evalEnergyOn, which requires functions: evalFns, mkVaryMap, applyCombined

// Compile the opt problem in `objFns_input` (which uses names) into a multivariate function on floats, and look up the initial state
let compileOptProblem = (compGraph, objFns_input, varyingMap) => {
    // Look up function name in function library
    let overallFn = objFn_library[objFns_input[0].fnName];

    // TODO: for each function's argument, look it up / evaluate it in compGraph
    // TODO: Make the evaluation of `F` richer to account for the compGraph, using the three inputs
    // Need to eval all args, create composite function

    // TODO: Make the varyingMap from the compGraph
    // Need to write a lookupPath function and all of the helpers

    // Look up each argument's varying value in varyingMap (currently a TF variable, TODO make this an expr?)
    let argVals = objFns_input[0].args.map(argName => compileArg(argName, compGraph));

    return { F: overallFn,
	     x: argVals };
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
    plus2 : (x, y) => x.add(y);
}

// -----------------------

// POSSIBLE STYLE INPUTS

// TODO: Make this more generic (multiple fns and args) and test the compilation behaves correctly programmatically
// (With test cases)
let objFns_input0 = [ { fnName: "min2",
		       args: ["A.shape.x", "B.shape.y"] }
		    ];

// No intermediate nodes
let compGraph0 = {};

// Varying leaf nodes of computational graph
let varyingMap0 =
    { "A.shape.x" : tf.scalar(Math.random()).variable(),
      "B.shape.y" : tf.scalar(Math.random()).variable()
    };

// ------

let objFns_input1 = [ { fnName: "min2",
			args: ["A.shape.x", "B.shape.y"] }
		    ];

// One intermediate node
// (Combined with varyingMap)
let compGraph1 = {
    "A.shape.x" : tf.scalar(Math.random()).variable(),
    "B.shape.y" : ["plus", "C.shape.r", 4.0], // B.shape.y := C.shape.r + 4.0
    "C.shape.r" : tf.scalar(Math.random()).variable()
};

let varyingMap1 = {};

// ----------------------

// CHOSEN STYLE INPUT

let compGraph = compGraph1;
let varyingMap = varyingMap1;
let objFns_input = objFns_input1;

// SYSTEM RUNTIME CODE

let varyingVals = Object.values(varyingMap); // TODO standardize order of values (alphabetical by varyingMap order? use a list?)

// Compile out all names/references? F : [a] -> a, X0 : [a]
let pure_opt_problem = compileOptProblem(compGraph, objFns_input, varyingMap)
console.log(pure_opt_problem);

// TODO: Compose `f` from smaller functions
let lastEnergy = minimize(pure_opt_problem.F, pure_opt_problem.x);

// TODO: do generically
// For some reason it comes out as a list of single-elem-list of Float32Array
console.log("varyingMap", Object.values(varyingMap).map(v => v.dataSync()[0]));
console.log(`F[v]: ${lastEnergy}`);
