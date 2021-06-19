const NUM_PRE_VARS = 6000;
const NUM_VARS = 600;
const NUM_POST_VARS = 115000;

let progStr = "";
let xs = "";

for (let i = 0; i < NUM_PRE_VARS; i++) {
  progStr += `const x${i} = ${i};\n`;
}

for (let i = NUM_PRE_VARS; i < NUM_PRE_VARS + NUM_VARS; i++) {
  progStr += `const x${i} = ${i};\n`;
  xs += `x${i}, `;
}

progStr += `const res = [${xs}].reduce((x, y) => x + y);\n`;
progStr += `console.log("res", res);\n`;

for (let i = NUM_PRE_VARS + NUM_VARS; i < NUM_PRE_VARS + NUM_VARS + NUM_POST_VARS; i++) {
  progStr += `const x${i} = ${i};\n`;
}

// TODO: Return several vars?
progStr += `return res;`;

// f(x) => { do all the stuff with xs and ignore the args, for now }

const NUM_ARGS = 60;
let args = [];
let inputs = [];

for (let i = 0; i < 60; i++) {
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
