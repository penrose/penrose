/**
 * Return `acosh(v)`.
 */
export const acosh = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.acosh(x.val), "acosh");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(div(gvarOf(1.0),mul(sqrt(sub(x,gvarOf(1.0),false),false),sqrt(add(x,gvarOf(1.0),false),false),false),false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `acos(v)`.
 */
export const acos = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.acos(x.val), "acos");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(neg(div(gvarOf(1.0),sqrt(sub(gvarOf(1.0),mul(x,x,false),false),false),false),false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `asin(v)`.
 */
export const asin = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.asin(x.val), "asin");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(div(gvarOf(1.0),sqrt(sub(gvarOf(1.0),mul(x,x,false),false),false),false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `asinh(v)`.
 */
export const asinh = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.asinh(x.val), "asinh");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(div(gvarOf(1.0),sqrt(add(gvarOf(1.0),mul(x,x,false),false),false),false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `atan(v)`.
 */
export const atan = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.atan(x.val), "atan");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(div(gvarOf(1.0),add(gvarOf(1.0),mul(x,x,false),false),false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `atanh(v)`.
 */
export const atanh = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.atanh(x.val), "atanh");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(div(gvarOf(1.0),sub(gvarOf(1.0),mul(x,x,false),false),false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `cbrt(v)`.
 */
export const cbrt = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.cbrt(x.val), "cbrt");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(div(gvarOf(1.0),mul(gvarOf(3.0),squared(cbrt(x,false),false),false),false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `cos(v)`.
 */
export const cos = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.cos(x.val), "cos");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(neg(sin(x,false),false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `cosh(v)`.
 */
export const cosh = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.cosh(x.val), "cosh");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(sinh(x,false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `exp(v)`.
 */
export const exp = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.exp(x.val), "exp");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(exp(x,false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `expm1(v)`.
 */
export const expm1 = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.expm1(x.val), "expm1");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(exp(x,false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `ln(v)`.
 */
export const ln = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.log(x.val), "log");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(div(gvarOf(1.0),x,false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `log2(v)`.
 */
export const log2 = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.log2(x.val), "log2");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(div(gvarOf(1.0),mul(x,gvarOf(0.6931471805599453),false),false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `log10(v)`.
 */
export const log10 = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.log10(x.val), "log10");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(div(gvarOf(1.0),mul(x,gvarOf(2.302585092994046),false),false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `log1p(v)`.
 */
export const log1p = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.log1p(x.val), "log1p");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(div(gvarOf(1.0),add(gvarOf(1.0),x,false),false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `sin(v)`.
 */
export const sin = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.sin(x.val), "sin");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(cos(x,false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `sinh(v)`.
 */
export const sinh = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.sinh(x.val), "sinh");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(cosh(x,false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `tan(v)`.
 */
export const tan = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.tan(x.val), "tan");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(squared(div(gvarOf(1.0),cos(x,false),false),false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

/**
 * Return `tanh(v)`.
 */
export const tanh = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.tanh(x.val), "tanh");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = just(squared(div(gvarOf(1.0),cosh(x,false),false),false));
    x.parents.push({ node: y, sensitivityNode: node });
    y.children.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsGrad.push({ node: y, sensitivityNode: none });
    y.childrenGrad.push({ node: x, sensitivityNode: none });
  }

  return y;
};

