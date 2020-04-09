// Use: open index.html in browser; check console

// Simple example of computing grad of a function evaluated at a point

// f(x) = x ^ 2
const f = x => x.square();
// f'(x) = 2x
const g = tf.grad(f);

const x = tf.tensor1d([2, 3]); // Note automatic size casting
console.log("f'(x) = ");
g(x).print();

// Optimize this function on variable `a` only

const a = tf.scalar(Math.random()).variable();

const learningRate = 0.01; // TODO Try different learning rates
const optimizer1 = tf.train.sgd(learningRate);
const optimizer2 = tf.train.adam(learningRate);

let res;

// TODO: Test which of these optimizers converge quickest
// Should it really take 100 iterations to minimize f(x)=x^2?!
for (let i = 0; i < 100; i++) {
    // Note minimize has params returnCost and varList (for specifying variables)
    res = optimizer2.minimize(() => f(a), returnCost=true);
}

console.log(`a: ${a.dataSync()}`);
console.log(`f(a): ${res}`);

// ------------------------------

// More realistic example, closer to Penrose:
// (Not yet compiled; data transformed by hand)

// Style: encourage center(x.shape)
// Functions: center(s : GPI) => (s.x)^2 + (s.y)^2
// GenOptProblem: F[X] := 
// Grad: dF[X]/d[X] = [dF/dX1, ... dF/dXn]

// NOTE: tf.js can't necessarily opt. WRT constraints

let num = x => tf.scalar(x, 'int32'); // What numeric format do we want?

let gpi1 = { x : 10.0, y : 25.0, r : 3.0, name : "circA" };
let gpi2 = { x : num(10.0), y : num(25.0), r : num(3.0), name : "circA" };
let gpi2_varying_vals = [num(10.0), num(25.0)]; // TODO do this programmatically, also these need to be tensors

let centerFn1 = shape => shape.x * shape.x + shape.y * shape.y;

// You can't eval these functions with normal floats
let centerFn2 = shape => (shape.x).square().add((shape.y).square());

// Be careful not to use normal ops in any tf code!
// This is (x,y) not ([x,y])
let centerFn3 = (x, y) => x.square().add(y.square());

console.log("centerFn2(gpi2) = ");
centerFn2(gpi2).print();

let objFn = { name: "centerFn", args: "circA" };

const gradFn2 = tf.grads(centerFn3); // Use grads for multi-args
console.log("centerFn3'(gpi2) = ");
let [dx, dy] = gradFn2(gpi2_varying_vals);
// the x passed in grad(f)(x) must be a tensor
dx.print();
dy.print();

// Optimize params (c,d)

const c = tf.scalar(Math.random()).variable();
const d = tf.scalar(Math.random()).variable();
const vars = [c, d];

const optimize = (vars) => {
    // Using the same optimizer from above
    // TODO benchmark how long these take (it pauses a bit in the browser)
    let res2;

    for (let i = 0; i < 100; i++) {
	res2 = optimizer2.minimize(() => centerFn3(...vars), returnCost=true, varList=vars);
    }

    return res2;
};

let res3 = optimize(vars);

console.log(`c: ${c.dataSync()}, d: ${d.dataSync()}`);
console.log(`f(a): ${res3}`);
