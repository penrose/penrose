// Use: open index.html in browser; check console

// Goals:
// Define clear interface to translation (What operations does the optimizer want to perform on the translation?)
// (Opt types = optimizer types)
// Define boundary between "Style types" and opt types 
// Define boundary between system internal types (i.e. those in the translation) and opt types
// Figure out what conversion needs to happen between Style functions and tfjs functions
// Figure out the speed hit for conversion
// Benchmark tfjs solver (speed/perf) vs. Penrose solver

// TODO: Scope the goal for tomorrow. 
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

// Optimize f over x
const optimizeTr = (tr, f, x) => {
    let res;

    // Use included tf.js optimizer for now
    // TODO profile this; change # iterations (test for convergence) and LR
    for (let i = 0; i < 100; i++) {
    	res = optimizer2.minimize(() => f(...x), returnCost=true, varList=x);
    }

    return res;
};

const v0 = tf.scalar(Math.random()).variable();
const v1 = tf.scalar(Math.random()).variable();

let tr = {}; // TODO: fill in
let F = (x, y) => x.square().add(y.square()); // TODO: make generic
// TODO: Write a realistic `F` and use `x` realistically (and standardize names)
let varying = [v0, v1];

// TODO: Compose `f` from smaller functions
let lastEnergy = optimizeTr(tr, F, varying);

// TODO: do generically
console.log(`v0: ${v0.dataSync()}, v1: ${v1.dataSync()}`);
console.log(`F[v]: ${lastEnergy}`);
