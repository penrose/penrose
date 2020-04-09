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

// TODO: remove the first script, which clobbers the scope

// CONSTS
// const learningRate = 0.01; // TODO Try different learning rates
// const optimizer1 = tf.train.sgd(learningRate);
// const optimizer2 = tf.train.adam(learningRate);

const evalEnergyOn = (tr, varying) => {
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

// UTILS

// TODO: Figure out how to schedule computations as tensors?
// objFn :: [a] -> a
// TODO: Make the input size and type richer
let objFn_library = { 
    min2 : (x, y) => x.square().add(y.square()), // x^2 + y^2
    equal2 : (x, y) => x.sub(y).square() // (x - y)^2
};

// TODO: Make this more generic (multiple fns and args) and test the compilation behaves correctly programmatically
// (With test cases)
let objFns_input = [ { fnName: "min2",
		       args: ["A.shape.x", "B.shape.y"] }
		   ];

// Compile the opt problem in `objFns_input` (which uses names) into a multivariate function on floats, and look up the initial state
let compileOptProblem = (tr, objFns_input, varyingMap) => {
    // Look up function name in function library
    let overallFn = objFn_library[objFns_input[0].fnName];
    // Look up each argument's varying value in varyingMap (currently a TF variable, TODO make this an expr?)
    let argVals = objFns_input[0].args.map(argName => varyingMap[argName]);

    return { F: overallFn,
	     x: argVals };
};

let varyingMap =
    { "A.shape.x" : tf.scalar(Math.random()).variable(),
      "B.shape.y" : tf.scalar(Math.random()).variable()
    };

let tr = {}; // TODO: fill in
let varyingVals = Object.values(varyingMap); // TODO standardize order of values (alphabetical by varyingMap order? use a list?)

// Compile out all names/references? F : [a] -> a, X0 : [a]
let pure_opt_problem = compileOptProblem(tr, objFns_input, varyingMap)
console.log(pure_opt_problem);

// TODO: Compose `f` from smaller functions
let lastEnergy = minimize(pure_opt_problem.F, pure_opt_problem.x);

// TODO: do generically
// For some reason it comes out as a list of single-elem-list of Float32Array
console.log("varyingMap", Object.values(varyingMap).map(v => v.dataSync()[0]));
console.log(`F[v]: ${lastEnergy}`);
