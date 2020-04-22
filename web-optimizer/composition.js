// This example shows that tf.js deals with function composition out of the box, written in js notation.
// We define functions f and j, and tf can evaluate (j . f)'(x) with no problem.

// f(x) = x ^ 2
const f = x => x.square();
// f'(x) = 2x
const g = tf.grad(f);

// j(x) = x ^ 3
const j = x => x.pow(tf.scalar(3, 'int32'));
// j'(x) = 3x ^ 2
const k = tf.grad(j);

// j(f(x)) = (x^2)^3 = x^6
const jf = x => j(f(x));
// (j.f)'(x) = 6x^5
const jf_grad = tf.grad(jf);

// 6 * 2^5 = 192
const x = tf.tensor1d([2]);
jf_grad(x).print();
