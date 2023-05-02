//! This module implements an L-BFGS optimizer using techniques from the following three books:
//!
//! - _Convex Optimization_ by Boyd and Vandenberghe, 2009 edition
//! - _Engineering Optimization_ by Rao, 2009 edition
//! - _Numerical Optimization_ by Nocedal and Wright, 1999 edition

#[derive(Clone, Copy, Debug)]
pub struct Config {
    /// The number of vector pairs to store for L-BFGS. See page 224 of Nocedal and Wright.
    pub m: usize,

    /// Constant for the Armijo condition. See TODO.
    pub armijo: f64,

    /// Constant for the Wolfe condition. See TODO.
    pub wolfe: f64,

    pub min_interval: f64,

    /// The maximum number of steps for line search. See TODO.
    pub max_steps: usize,

    pub epsd: f64,
}

#[derive(Clone, Debug)]
pub struct State {
    /// The previous point.
    pub x: Vec<f64>,

    /// The previous gradient.
    pub grad: Vec<f64>,

    /// See page 224 of Nocedal and Wright.
    pub s_y: Vec<(Vec<f64>, Vec<f64>)>,
}

fn dot(u: &[f64], v: &[f64]) -> f64 {
    u.iter().zip(v).map(|(a, b)| a * b).sum()
}

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
            break;
        }

        if b < f64::INFINITY {
            t = (a + b) / 2.;
        } else {
            t = 2. * a;
        }

        j += 1;
    }

    t
}

pub fn first_step(
    cfg: Config,
    mut f: impl FnMut(&[f64], &mut [f64]) -> f64,
    x: &mut [f64],
) -> State {
    let n = x.len();
    let x0 = x.to_vec();

    let mut grad = vec![0.; n];
    let fx = f(&x, &mut grad);

    let r = grad.clone();
    line_search(cfg, f, &x0, &r, fx, &mut grad, x);

    State {
        x: x0,
        grad: r,
        s_y: vec![],
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Info<'a> {
    pub state: &'a State,

    pub fx: f64,

    pub r: &'a [f64],

    pub x: &'a [f64],

    pub t: f64,
}

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
            state: &state,
            fx,
            r: &r,
            t,
            x,
        }) {
            return msg;
        }
    }
}
