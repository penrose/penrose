// This module implements an L-BFGS optimizer using techniques from the
// following three books:
//
// - _Convex Optimization_ by Boyd and Vandenberghe, 2009 edition
// - _Engineering Optimization_ by Rao, 2009 edition
// - _Numerical Optimization_ by Nocedal and Wright, 1999 edition

/**
 * The first parameter is the point at which to evaluate the objective function,
 * and the second parameter is an output parameter to hold the gradient at that
 * point. The function should store the gradient in that output parameter and
 * then return the objective value.
 */
export type Fn = (x: Float64Array, grad: Float64Array) => number;

/** Configuration options for L-BFGS. */
export interface Config {
  /**
   * The number of vector pairs to store for L-BFGS. See page 224 of Nocedal and
   * Wright.
   */
  m: number;

  /** Constant for the Armijo condition. See page 37 of Nocedal and Wright. */
  armijo: number;

  /** Constant for the Wolfe condition. See page 39 of Nocedal and Wright. */
  wolfe: number;

  /** The minimum interval size for line search. */
  minInterval: number;

  /** The maximum number of steps for line search. */
  maxSteps: number;

  /** A small positive constant to add to a denominator that might be zero. */
  epsd: number;
}

/**
 * All L-BFGS state that needs to be kept between iterations, other than the
 * current point.
 */
export interface State {
  /** The previous point. */
  x: Float64Array;

  /** The previous gradient. */
  grad: Float64Array;

  /** See page 224 of Nocedal and Wright. */
  s: Float64Array[];

  /** See page 224 of Nocedal and Wright. */
  y: Float64Array[];
}

const dot = (u: Float64Array, v: Float64Array): number => {
  let z = 0;
  for (let i = 0; i < u.length; i++) z += u[i] * v[i];
  return z;
};

/**
 * Return the line search step size, having set `x` to the new point.
 *
 * @param x0 the current point.
 * @param r the descent direction, preconditioned by L-BFGS.
 * @param fx0 the objective at `x0`.
 * @param grad the gradient at `x0`, and is then used as scratch space; don't
 *  depend on its value.
 * @param x the new point. */
const lineSearch = (
  cfg: Config,
  f: Fn,
  x0: Float64Array,
  r: Float64Array,
  fx0: number,
  grad: Float64Array,
  x: Float64Array,
): number => {
  const n = x.length;

  const dufAtx0 = -dot(r, grad);

  let a = 0;
  let b = Infinity;
  let t = 1;
  let j = 0;

  for (;;) {
    for (let i = 0; i < n; i++) x[i] = x0[i] - t * r[i];

    if (Math.abs(a - b) < cfg.minInterval || j > cfg.maxSteps) break;

    const fx = f(x, grad);
    const isArmijo = fx <= fx0 + cfg.armijo * t * dufAtx0;
    const isWolfe = -dot(r, grad) >= cfg.wolfe * dufAtx0; // weak Wolfe condition

    if (!isArmijo) b = t;
    else if (!isWolfe) a = t;
    else break; // found good interval

    if (b < Infinity) t = (a + b) / 2; // already found Armijo
    else t = 2 * a; // did not find Armijo

    j++;
  }

  return t;
};

/**
 * Perform the first step of L-BFGS at point `x`, updating it and returning the
 * initial `State`.
 */
export const firstStep = (cfg: Config, f: Fn, x: Float64Array): State => {
  const n = x.length;
  const x0 = new Float64Array(x);

  const grad = new Float64Array(n);
  const fx = f(x, grad);

  const r = new Float64Array(grad);
  lineSearch(cfg, f, x0, r, fx, grad, x);

  return { x: x0, grad: r, s: [], y: [] };
};

/** Information after a step of L-BFGS. */
export interface Info {
  /** Data about previous steps. */
  state: State;

  /** The objective value at the current point. */
  fx: number;

  /** The preconditioned descent direction. */
  r: Float64Array;

  /** The current point. */
  x: Float64Array;

  /** The line search step size. */
  t: number;
}

/** Perform L-BFGS steps on `x`, giving `stop`'s first non-`undefined` value. */
export const stepUntil = <T>(
  cfg: Config,
  f: Fn,
  x: Float64Array,
  state: State,
  stop: (info: Info) => T | undefined,
): T => {
  const n = x.length;
  const grad = new Float64Array(n);

  const rho = new Float64Array(cfg.m);
  const alpha = new Float64Array(cfg.m);
  const q = new Float64Array(n);
  const r = new Float64Array(n);

  for (;;) {
    const fx = f(x, grad);

    let m = state.s.length;
    if (m < cfg.m) {
      m++;

      const s = new Float64Array(n);
      for (let i = 0; i < n; i++) s[i] = x[i] - state.x[i];
      state.s.push(s);

      const y = new Float64Array(n);
      for (let i = 0; i < n; i++) y[i] = grad[i] - state.grad[i];
      state.y.push(y);
    } else {
      const s = state.s[m - 1];
      const y = state.y[m - 1];
      for (let i = 0; i < n; i++) {
        s[i] = x[i] - state.x[i];
        y[i] = grad[i] - state.grad[i];
      }
    }
    state.s.unshift(state.s.pop()!);
    state.y.unshift(state.y.pop()!);

    state.x.set(x);
    state.grad.set(grad);

    for (let j = 0; j < m; j++)
      rho[j] = 1 / (dot(state.y[j], state.s[j]) + cfg.epsd);

    q.set(grad);

    for (let j = 0; j < m; j++) {
      const s_j = state.s[j];
      const y_j = state.y[j];
      const alpha_j = rho[j] * dot(s_j, q);
      alpha[j] = alpha_j;
      for (let i = 0; i < n; i++) q[i] -= alpha_j * y_j[i];
    }

    // see page 226 of Nocedal and Wright
    const s_k = state.s[0];
    const y_k = state.y[0];
    const gamma = dot(s_k, y_k) / (dot(y_k, y_k) + cfg.epsd);
    for (let i = 0; i < n; i++) r[i] = gamma * q[i];

    for (let j = m - 1; j >= 0; j--) {
      const s_j = state.s[j];
      const y_j = state.y[j];
      const alpha_j = alpha[j];
      const beta = rho[j] * dot(y_j, r);
      for (let i = 0; i < n; i++) r[i] += s_j[i] * (alpha_j - beta);
    }

    const t = lineSearch(cfg, f, state.x, r, fx, grad, x);

    const msg = stop({ state, fx, r, t, x });
    if (msg !== undefined) return msg;
  }
};
