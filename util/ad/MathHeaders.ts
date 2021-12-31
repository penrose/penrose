acosh: {
   fn: (x: number): number => Math.acosh(x),
},
acos: {
   fn: (x: number): number => Math.acos(x),
},
asin: {
   fn: (x: number): number => Math.asin(x),
},
asinh: {
   fn: (x: number): number => Math.asinh(x),
},
atan: {
   fn: (x: number): number => Math.atan(x),
},
atanh: {
   fn: (x: number): number => Math.atanh(x),
},
cbrt: {
   fn: (x: number): number => Math.cbrt(x),
},
cos: {
   fn: (x: number): number => Math.cos(x),
},
cosh: {
   fn: (x: number): number => Math.cosh(x),
},
exp: {
   fn: (x: number): number => Math.exp(x),
},
expm1: {
   fn: (x: number): number => Math.expm1(x),
},
ln: {
   fn: (x: number): number => Math.log(x),
},
log2: {
   fn: (x: number): number => Math.log2(x),
},
log10: {
   fn: (x: number): number => Math.log10(x),
},
log1p: {
   fn: (x: number): number => Math.log1p(x),
},
sin: {
   fn: (x: number): number => Math.sin(x),
},
sinh: {
   fn: (x: number): number => Math.sinh(x),
},
tan: {
   fn: (x: number): number => Math.tan(x),
},
tanh: {
   fn: (x: number): number => Math.tanh(x),
},


    } else if (z.op === "acosh") {
      stmt = `const ${parName} = Math.acosh(${childName});`;
    } else if (z.op === "acos") {
      stmt = `const ${parName} = Math.acos(${childName});`;
    } else if (z.op === "asin") {
      stmt = `const ${parName} = Math.asin(${childName});`;
    } else if (z.op === "asinh") {
      stmt = `const ${parName} = Math.asinh(${childName});`;
    } else if (z.op === "atan") {
      stmt = `const ${parName} = Math.atan(${childName});`;
    } else if (z.op === "atanh") {
      stmt = `const ${parName} = Math.atanh(${childName});`;
    } else if (z.op === "cbrt") {
      stmt = `const ${parName} = Math.cbrt(${childName});`;
    } else if (z.op === "cos") {
      stmt = `const ${parName} = Math.cos(${childName});`;
    } else if (z.op === "cosh") {
      stmt = `const ${parName} = Math.cosh(${childName});`;
    } else if (z.op === "exp") {
      stmt = `const ${parName} = Math.exp(${childName});`;
    } else if (z.op === "expm1") {
      stmt = `const ${parName} = Math.expm1(${childName});`;
    } else if (z.op === "log") {
      stmt = `const ${parName} = Math.log(${childName});`;
    } else if (z.op === "log2") {
      stmt = `const ${parName} = Math.log2(${childName});`;
    } else if (z.op === "log10") {
      stmt = `const ${parName} = Math.log10(${childName});`;
    } else if (z.op === "log1p") {
      stmt = `const ${parName} = Math.log1p(${childName});`;
    } else if (z.op === "sin") {
      stmt = `const ${parName} = Math.sin(${childName});`;
    } else if (z.op === "sinh") {
      stmt = `const ${parName} = Math.sinh(${childName});`;
    } else if (z.op === "tan") {
      stmt = `const ${parName} = Math.tan(${childName});`;
    } else if (z.op === "tanh") {
      stmt = `const ${parName} = Math.tanh(${childName});`;
