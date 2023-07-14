//! This module implements an L-BFGS optimizer using techniques from the following three books:
//!
//! - _Convex Optimization_ by Boyd and Vandenberghe, 2009 edition
//! - _Engineering Optimization_ by Rao, 2009 edition
//! - _Numerical Optimization_ by Nocedal and Wright, 1999 edition
//!
//! A few functions in this module take a parameter of type `impl FnMut(&[f64], &mut [f64]) -> f64`.
//! The first parameter is the point at which to evaluate the objective function, and the second
//! parameter is an output parameter to hold the gradient at that point. The function should store
//! the gradient in that output parameter and then return the objective value.

/// Configuration options for L-BFGS.
#[derive(Clone, Copy, Debug)]
pub struct Config {
    /// The number of vector pairs to store for L-BFGS. See page 224 of Nocedal and Wright.
    pub m: usize,

    /// Constant for the Armijo condition. See page 37 of Nocedal and Wright.
    pub armijo: f64,

    /// Constant for the Wolfe condition. See page 39 of Nocedal and Wright.
    pub wolfe: f64,

    /// The minimum interval size for line search.
    pub min_interval: f64,

    /// The maximum number of steps for line search.
    pub max_steps: usize,

    /// A small positive constant to add to a denominator that might be zero.
    pub epsd: f64,
}

/// All L-BFGS state that needs to be kept between iterations, other than the current point.
#[derive(Clone, Debug)]
pub struct State {
    /// The previous point.
    pub x: Vec<f64>,

    /// The previous gradient.
    pub grad: Vec<f64>,

    /// See page 224 of Nocedal and Wright.
    pub s_y: Vec<(Vec<f64>, Vec<f64>)>,
}

/// Return the dot product of `u` and `v`.
fn dot(u: &[f64], v: &[f64]) -> f64 {
    u.iter().zip(v).map(|(a, b)| a * b).sum()
}

/// Return the line search step size, having set `x` to the new point.
///
/// - `x0` is the current point.
/// - `r` is the descent direction, preconditioned by L-BFGS.
/// - `fx0` is the objective at `x0`.
/// - `grad` is the gradient at `x0`, and is then used as scratch space; don't depend on its value.
/// - `x` will hold the new point.
fn line_search(
    cfg: Config,
    mut f: impl FnMut(&[f64], &mut [f64]) -> f64,
    x0: &[f64],
    r: &[f64],
    fx0: f64,
    grad: &mut [f64],
    x: &mut [f64],
) -> f64 {
    let n = x.len();

    let duf_at_x0 = -dot(r, grad);

    let mut a = 0.;
    let mut b = f64::INFINITY;
    let mut t = 1.;
    let mut j = 0;

    loop {
        for i in 0..n {
            x[i] = x0[i] - t * r[i];
        }

        if (a - b).abs() < cfg.min_interval || j > cfg.max_steps {
            break;
        }

        let fx = f(x, grad);
        let is_armijo = fx <= fx0 + cfg.armijo * t * duf_at_x0;
        let is_wolfe = -dot(r, grad) >= cfg.wolfe * duf_at_x0; // weak Wolfe condition

        if !is_armijo {
            b = t;
        } else if !is_wolfe {
            a = t;
        } else {
            break; // found good interval
        }

        if b < f64::INFINITY {
            t = (a + b) / 2.; // already found Armijo
        } else {
            t = 2. * a; // did not find Armijo
        }

        j += 1;
    }

    t
}

/// Perform the first step of L-BFGS at point `x`, updating it and returning the initial `State`.
pub fn first_step(
    cfg: Config,
    mut f: impl FnMut(&[f64], &mut [f64]) -> f64,
    x: &mut [f64],
) -> State {
    let n = x.len();
    let x0 = x.to_vec();

    let mut grad = vec![0.; n];
    let fx = f(x, &mut grad);

    let r = grad.clone();
    line_search(cfg, f, &x0, &r, fx, &mut grad, x);

    State {
        x: x0,
        grad: r,
        s_y: vec![],
    }
}

/// Information after a step of L-BFGS.
#[derive(Clone, Copy, Debug)]
pub struct Info<'a> {
    /// Data about previous steps.
    pub state: &'a State,

    /// The objective value at the current point.
    pub fx: f64,

    /// The preconditioned descent direction.
    pub r: &'a [f64],

    /// The current point.
    pub x: &'a [f64],

    /// The line search step size.
    pub t: f64,
}

/// Perform L-BFGS steps on `x`, returning once `stop` returns `Some`.
pub fn step_until<T>(
    cfg: Config,
    mut f: impl FnMut(&[f64], &mut [f64]) -> f64,
    x: &mut [f64],
    state: &mut State,
    mut stop: impl FnMut(Info) -> Option<T>,
) -> T {
    let n = x.len();
    let mut grad = vec![0.; n];

    let mut rho = vec![0.; cfg.m];
    let mut alpha = vec![0.; cfg.m];
    let mut q = vec![0.; n];
    let mut r = vec![0.; n];

    loop {
        let fx = f(x, &mut grad);

        if state.s_y.len() < cfg.m {
            state.s_y.push((
                x.iter().zip(&state.x).map(|(a, b)| a - b).collect(),
                grad.iter().zip(&state.grad).map(|(a, b)| a - b).collect(),
            ));
        } else {
            let (s, y) = state.s_y.last_mut().expect("m > 0");
            for i in 0..n {
                s[i] = x[i] - state.x[i];
                y[i] = grad[i] - state.grad[i];
            }
        }
        state.s_y.rotate_right(1);

        state.x.copy_from_slice(x);
        state.grad.copy_from_slice(&grad);

        for (j, (s_j, y_j)) in state.s_y.iter().enumerate() {
            rho[j] = 1. / (dot(y_j, s_j) + cfg.epsd);
        }

        q.copy_from_slice(&grad);

        for (j, (s_j, y_j)) in state.s_y.iter().enumerate() {
            let alpha_j = rho[j] * dot(s_j, &q);
            alpha[j] = alpha_j;
            for i in 0..n {
                q[i] -= alpha_j * y_j[i];
            }
        }

        // see page 226 of Nocedal and Wright
        let (s_k, y_k) = &state.s_y[0];
        let gamma = dot(s_k, y_k) / (dot(y_k, y_k) + cfg.epsd);
        for i in 0..n {
            r[i] = gamma * q[i];
        }

        for (j, (s_j, y_j)) in state.s_y.iter().enumerate().rev() {
            let alpha_j = alpha[j];
            let beta = rho[j] * dot(y_j, &r);
            for i in 0..n {
                r[i] += s_j[i] * (alpha_j - beta);
            }
        }

        let t = line_search(cfg, &mut f, &state.x, &r, fx, &mut grad, x);

        if let Some(msg) = stop(Info {
            state,
            fx,
            r: &r,
            t,
            x,
        }) {
            return msg;
        }
    }
}
