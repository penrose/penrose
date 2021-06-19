// const NUM_PRE_VARS = 6000;
const NUM_PRE_VARS = 0;
const NUM_REDUCE_VARS = 2;
// const NUM_POST_VARS = 115000;
const NUM_POST_VARS = 121000; // Threshold appears to be ~120k whether it's in pre or post vars
// const NUM_POST_VARS = 1000000;
// At 1 million vars, the js stack size is exceeded and the error is thrown on x0

const NUM_ARGS = 1;

let progStr = "";
let xs = "";

for (let i = 0; i < NUM_PRE_VARS; i++) {
  progStr += `const x${i} = ${i};\n`;
}

for (let i = NUM_PRE_VARS; i < NUM_PRE_VARS + NUM_REDUCE_VARS; i++) {
  progStr += `const x${i} = ${i};\n`;
  xs += `x${i}, `;
}

// Function actually works totally fine without the reduce...
// progStr += `const res = 0;`

progStr += `debugger;\n`;
progStr += `const res = [${xs}].reduce((x, y) => x + y);\n`;
progStr += `console.log("res", res);\n`;

for (let i = NUM_PRE_VARS + NUM_REDUCE_VARS; i < NUM_PRE_VARS + NUM_REDUCE_VARS + NUM_POST_VARS; i++) {
  progStr += `const x${i} = ${i};\n`;
}

// TODO: Return several vars?
progStr += `return res;`;

// f(x) => { do all the stuff with xs and ignore the args, for now }

let args = [];
let inputs = [];

for (let i = 0; i < NUM_ARGS; i++) {
  args.push(`c${i}`);
  inputs.push(i);
}

// console.log("body", progStr);

const f = new Function(...args, progStr);
// console.log(f);
console.log("function", f);
console.log("args", args);
console.log("inputs", inputs);
console.log("f(inputs)", f(...inputs));
