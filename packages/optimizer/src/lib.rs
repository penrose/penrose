use log::Level;
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use wasm_bindgen::{prelude::wasm_bindgen, JsValue};

type Bool = i32; // 0 or 1 (`wasm-bindgen` doesn't support `bool` well)

#[wasm_bindgen(module = "/call.js")]
extern "C" {
    fn call_grad(
        f: JsValue,
        inputs: &[f64],
        mask: &[Bool],
        gradient: &mut [f64],
        secondary: &mut [f64],
    ) -> f64;
}

type Vector = nalgebra::DVector<f64>;

// the ts-rs crate defines a `TS` trait and `ts` macro which generate Rust tests that, when run,
// generate TypeScript definitions in the `bindings/` directory of this package; that's why the
// `build-decls` script for this package is `cargo test`

#[derive(Clone, Deserialize, Serialize, TS)]
#[ts(export)]
enum OptStatus {
    NewIter,
    UnconstrainedRunning,
    UnconstrainedConverged,
    EPConverged,
    Error,
}

// `n` is the size of the varying state
#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[ts(export)]
struct LbfgsParams {
    #[serde(rename = "lastState")]
    last_state: Option<Vec<f64>>, // nx1 (col vec)
    #[serde(rename = "lastGrad")]
    last_grad: Option<Vec<f64>>, // nx1 (col vec)
    s_list: Vec<Vec<f64>>, // list of nx1 col vecs
    y_list: Vec<Vec<f64>>, // list of nx1 col vecs
    #[serde(rename = "numUnconstrSteps")]
    num_unconstr_steps: i32,
    #[serde(rename = "memSize")]
    mem_size: i32,
}

struct LbfgsAnswer {
    gradfxs_preconditioned: Vec<f64>,
    updated_lbfgs_info: LbfgsParams,
}

struct FnEvaled {
    f: f64,
    gradf: Vec<f64>,
    obj_engs: Vec<f64>,
    constr_engs: Vec<f64>,
}

#[derive(Clone, Deserialize, Serialize, TS)]
#[ts(export)]
struct Params {
    #[serde(rename = "gradMask")]
    grad_mask: Vec<bool>,
    #[serde(rename = "objMask")]
    obj_mask: Vec<bool>,
    #[serde(rename = "constrMask")]
    constr_mask: Vec<bool>,

    #[serde(rename = "optStatus")]
    opt_status: OptStatus,
    /// Constraint weight for exterior point method
    weight: f64,
    /// Info for unconstrained optimization
    #[serde(rename = "UOround")]
    uo_round: i32,
    #[serde(rename = "lastUOstate")]
    last_uo_state: Option<Vec<f64>>,
    #[serde(rename = "lastUOenergy")]
    last_uo_energy: Option<f64>,
    #[serde(rename = "lastObjEnergies")]
    last_obj_energies: Option<Vec<f64>>,
    #[serde(rename = "lastConstrEnergies")]
    last_constr_energies: Option<Vec<f64>>,

    /// Info for exterior point method
    #[serde(rename = "EPround")]
    ep_round: i32,
    #[serde(rename = "lastEPstate")]
    last_ep_state: Option<Vec<f64>>,
    #[serde(rename = "lastEPenergy")]
    last_ep_energy: Option<f64>,

    #[serde(rename = "lastGradient")]
    last_gradient: Vec<f64>, // Value of gradient evaluated at the last state
    #[serde(rename = "lastGradientPreconditioned")]
    last_gradient_preconditioned: Vec<f64>, // Value of gradient evaluated at the last state, preconditioned by LBFGS
    // ^ Those two are stored to make them available to Style later

    // For L-BFGS
    #[serde(rename = "lbfgsInfo")]
    lbfgs_info: LbfgsParams,
}

// Just the compiled function and its grad, with no weights for EP/constraints/penalties, etc.
#[derive(Clone)]
struct FnCached<'a> {
    f: JsValue,
    grad_mask: &'a [bool],
    obj_mask: &'a [bool],
    constr_mask: &'a [bool],
}

#[derive(Deserialize, Serialize, TS)]
#[ts(export)]
struct OptState {
    #[serde(rename = "varyingValues")]
    varying_values: Vec<f64>,
    params: Params,
}

// Returned after a call to `minimize`
struct OptInfo {
    xs: Vec<f64>,
    energy_val: f64,
    obj_engs: Vec<f64>,
    constr_engs: Vec<f64>,
    norm_grad: f64,
    new_lbfgs_info: LbfgsParams,
    gradient: Vec<f64>,
    gradient_preconditioned: Vec<f64>,
    failed: bool,
}

// Intial weight for constraints
const INIT_CONSTRAINT_WEIGHT: f64 = 10e-3;

const DEFAULT_LBFGS_MEM_SIZE: i32 = 17;

const DEFAULT_LBFGS_PARAMS: LbfgsParams = LbfgsParams {
    last_state: None,
    last_grad: None,
    s_list: vec![],
    y_list: vec![],
    num_unconstr_steps: 0,
    mem_size: DEFAULT_LBFGS_MEM_SIZE,
};

// growth factor for constraint weights
const WEIGHT_GROWTH_FACTOR: f64 = 10.;

// EP method convergence criteria
const EP_STOP: f64 = 1e-3;
// const EP_STOP: f64 = 1e-5;
// const EP_STOP: f64 = 1e-7;

const EPSD: f64 = 1e-11;

// Unconstrained method convergence criteria
// TODO. This should REALLY be 10e-10
// NOTE: The new autodiff + line search seems to be really sensitive to this parameter (`UO_STOP`). It works for 1e-2, but the line search ends up with too-small intervals with 1e-5
const UO_STOP: f64 = 1e-2;
// const UO_STOP: f64 = 1e-3;
// const UO_STOP: f64 = 1e-5;
// const UO_STOP: f64 = 10;

// const DEBUG_GRAD_DESCENT = true;
const DEBUG_GRAD_DESCENT: bool = false;
const USE_LINE_SEARCH: bool = true;
const BREAK_EARLY: bool = true;
const DEBUG_LBFGS: bool = false;
const DEBUG_LINE_SEARCH: bool = false;

////////////////////////////////////////////////////////////////////////////////

fn bools(v: Vec<Bool>) -> Vec<bool> {
    v.into_iter().map(|n| n > 0).collect()
}

fn norm_list(xs: &[f64]) -> f64 {
    let sum_squares: f64 = xs.iter().map(|e| e * e).sum();
    sum_squares.sqrt()
}

fn scalev(c: f64, xs: &[f64]) -> Vec<f64> {
    xs.iter().map(|x| c * x).collect()
}

fn addv(xs: &[f64], ys: &[f64]) -> Vec<f64> {
    xs.iter().zip(ys).map(|(x, y)| x + y).collect()
}

fn subv(xs: &[f64], ys: &[f64]) -> Vec<f64> {
    xs.iter().zip(ys).map(|(x, y)| x - y).collect()
}

fn negv(xs: &[f64]) -> Vec<f64> {
    xs.iter().map(|e| -e).collect()
}

fn dot(xs: &[f64], ys: &[f64]) -> f64 {
    xs.iter().zip(ys).map(|(x, y)| x * y).sum()
}

fn unconstrained_converged(norm_grad: f64) -> bool {
    if DEBUG_GRAD_DESCENT {
        log::info!("UO convergence check: ||grad f(x)|| {norm_grad}");
    }
    norm_grad < UO_STOP
}

fn objective_and_gradient(f: FnCached, weight: f64, xs: &[f64]) -> FnEvaled {
    let len_inputs = xs.len();
    let len_gradient = len_inputs + 1;
    let len_secondary = f.obj_mask.len() + f.constr_mask.len();

    let mut inputs = vec![0.; len_gradient];
    inputs[..len_inputs].copy_from_slice(xs);
    inputs[len_inputs] = weight;
    let mask: Vec<i32> = f
        .obj_mask
        .iter()
        .chain(f.constr_mask)
        .map(|&b| if b { 1 } else { 0 })
        .collect();
    let mut gradient = vec![0.; len_gradient];
    let mut secondary = vec![0.; len_secondary];

    let energy = call_grad(f.f, &inputs, &mask, &mut gradient, &mut secondary);

    gradient.pop();

    FnEvaled {
        f: energy,
        gradf: gradient
            .into_iter()
            .zip(f.grad_mask)
            .map(|(x, &b)| if b { x } else { 0. })
            .collect(),
        obj_engs: secondary[..f.obj_mask.len()].to_vec(),
        constr_engs: secondary[f.obj_mask.len()..].to_vec(),
    }
}

fn ep_converged(xs0: &[f64], xs1: &[f64], fxs0: f64, fxs1: f64) -> bool {
    // TODO: These dx and dfx should really be scaled to account for magnitudes
    let state_change = norm_list(&subv(xs1, xs0));
    let energy_change = (fxs1 - fxs0).abs();
    log::info!("epConverged?: stateChange: {state_change} | energyChange: {energy_change}");

    state_change < EP_STOP || energy_change < EP_STOP
}

// TODO. Annotate the return type: a new (copied?) state with the varyingState and opt params set?

// NOTE: `stepEP` implements the exterior point method as described here:
// https://www.me.utexas.edu/~jensen/ORMM/supplements/units/nlp_methods/const_opt.pdf (p7)

// Things that we should do programmatically improve the conditioning of the objective function:
// 1) scale the constraints so that the penalty generated by each is about the same magnitude
// 2) fix initial value of the penalty parameter so that the magnitude of the penalty term is not much smaller than the magnitude of objective function

/// Given a `State`, take n steps by evaluating the overall objective function
fn step(state: OptState, f: JsValue, steps: i32) -> OptState {
    let mut opt_params = state.params.clone();
    let Params {
        opt_status, weight, ..
    } = opt_params;
    let mut xs = state.varying_values.clone();

    log::info!("===============");
    log::info!(
        "step | weight: {weight} | EP round: {} | UO round: {}",
        opt_params.ep_round,
        opt_params.uo_round,
    );

    match opt_status {
        OptStatus::NewIter => {
            return OptState {
                params: Params {
                    weight: INIT_CONSTRAINT_WEIGHT,
                    uo_round: 0,
                    ep_round: 0,
                    opt_status: OptStatus::UnconstrainedRunning,
                    lbfgs_info: DEFAULT_LBFGS_PARAMS,
                    ..state.params
                },
                ..state
            }
        }

        OptStatus::UnconstrainedRunning => {
            // NOTE: use cached varying values

            let res = minimize(
                &xs,
                FnCached {
                    f,
                    grad_mask: &state.params.grad_mask,
                    obj_mask: &state.params.obj_mask,
                    constr_mask: &state.params.constr_mask,
                },
                state.params.weight,
                state.params.lbfgs_info,
                steps,
            );
            xs = res.xs;

            // the new `xs` is put into the `newState`, which is returned at end of function
            // we don't need the updated xsVars and energyGraph as they are always cleared on evaluation; only their structure matters
            let OptInfo {
                energy_val,
                norm_grad,
                new_lbfgs_info,
                gradient,
                gradient_preconditioned,
                failed,
                obj_engs,
                constr_engs,
                ..
            } = res;

            opt_params.last_uo_state = Some(xs.clone());
            opt_params.last_uo_energy = Some(energy_val);
            opt_params.uo_round = opt_params.uo_round + 1;
            opt_params.lbfgs_info = new_lbfgs_info;
            opt_params.last_gradient = gradient;
            opt_params.last_gradient_preconditioned = gradient_preconditioned;
            opt_params.last_constr_energies = Some(constr_engs);
            opt_params.last_obj_energies = Some(obj_engs);

            // NOTE: `varyingValues` is updated in `state` after each step by putting it into `newState` and passing it to `evalTranslation`, which returns another state

            // TODO. In the original optimizer, we cheat by using the EP cond here, because the UO cond is sometimes too strong.
            if unconstrained_converged(norm_grad) {
                opt_params.opt_status = OptStatus::UnconstrainedConverged;
                opt_params.lbfgs_info = DEFAULT_LBFGS_PARAMS;
                log::info!(
                    "Unconstrained converged with energy {energy_val} gradient norm {norm_grad}",
                );
            } else {
                opt_params.opt_status = OptStatus::UnconstrainedRunning;
                // Note that lbfgs prams have already been updated
                log::info!(
                    "Took {steps} steps. Current energy {energy_val} gradient norm {norm_grad}",
                );
            }
            if failed {
                log::warn!("Error detected after stepping");
                opt_params.opt_status = OptStatus::Error;
                return OptState {
                    params: opt_params,
                    ..state
                };
            }
        }

        OptStatus::UnconstrainedConverged => {
            // No minimization step should be taken. Just figure out if we should start another UO round with higher EP weight.
            // We are using the last UO state and energy because they serve as the current EP state and energy, and comparing it to the last EP stuff.

            // Do EP convergence check on the last EP state (and its energy), and curr EP state (and its energy)
            // (There is no EP state or energy on the first round)
            // Note that lbfgs params have already been reset to default

            // TODO. Make a diagram to clarify vocabulary

            // We force EP to run at least two rounds (State 0 -> State 1 -> State 2; the first check is only between States 1 and 2)
            if opt_params.ep_round > 1
                && ep_converged(
                    &opt_params.last_ep_state.unwrap(),
                    &opt_params.last_uo_state.as_ref().unwrap(),
                    opt_params.last_ep_energy.unwrap(),
                    opt_params.last_uo_energy.unwrap(),
                )
            {
                opt_params.opt_status = OptStatus::EPConverged;
                log::info!(
                    "EP converged with energy {}",
                    opt_params.last_uo_energy.unwrap(),
                );
            } else {
                // If EP has not converged, increase weight and continue.
                // The point is that, for the next round, the last converged UO state becomes both the last EP state and the initial state for the next round--starting with a harsher penalty.
                log::info!("step: UO converged but EP did not converge; starting next round");
                opt_params.opt_status = OptStatus::UnconstrainedRunning;

                opt_params.weight = WEIGHT_GROWTH_FACTOR * weight;
                opt_params.ep_round = opt_params.ep_round + 1;
                opt_params.uo_round = 0;

                log::info!(
                    "increased EP weight to {} in compiled energy and gradient",
                    opt_params.weight,
                );
            }

            // Done with EP check, so save the curr EP state as the last EP state for the future.
            opt_params.last_ep_state = opt_params.last_uo_state.clone();
            opt_params.last_ep_energy = opt_params.last_uo_energy;
        }

        OptStatus::EPConverged => {
            // do nothing if converged
            log::info!("step: EP converged");
            return state;
        }
        OptStatus::Error => {
            log::warn!("step: Error");
            return state;
        }
    }

    OptState {
        varying_values: xs.clone(),
        params: opt_params,
        ..state
    }
}

// Note: line search seems to be quite sensitive to the maxSteps parameter; with maxSteps=25, the line search might

fn aw_line_search(xs0: &[f64], f: FnCached, weight: f64, gradfxs0: &[f64], fxs0: f64) -> f64 {
    let max_steps = 10;

    let descent_dir = negv(gradfxs0); // This is preconditioned by L-BFGS

    let duf_at_x0 = dot(
        &descent_dir,
        &objective_and_gradient(f.clone(), weight, xs0).gradf,
    );
    let min_interval = 10e-10;

    // Hyperparameters
    let c1 = 0.001; // Armijo
    let c2 = 0.9; // Wolfe

    // Armijo condition
    // f(x0 + t * descentDir) <= (f(x0) + c1 * t * <grad(f)(x0), x0>)
    let armijo = |ti: f64, objective: f64| -> bool {
        // take in objective instead of calling f here, because we compute objective
        // and gradient at the same time and then pass them separately to armijo and
        // wolfe
        let cond1 = objective;
        let cond2 = fxs0 + c1 * ti * duf_at_x0;
        cond1 <= cond2
    };

    // D(u) := <grad f, u>
    // D(u, f, x) = <grad f(x), u>
    // u is the descentDir (i.e. -grad(f)(x))

    // Strong Wolfe condition
    // |<grad(f)(x0 + t * descentDir), u>| <= c2 * |<grad f(x0), u>|
    // let strong_wolfe = |_ti: f64, gradient: &[f64]| {
    //     let cond1 = dot(&descent_dir, gradient).abs();
    //     let cond2 = c2 * duf_at_x0.abs();
    //     cond1 <= cond2
    // };

    // Weak Wolfe condition
    // <grad(f)(x0 + t * descentDir), u> >= c2 * <grad f(x0), u>
    let weak_wolfe = |_ti: f64, gradient: &[f64]| {
        // take in gradient here, because we compute
        // objective and gradient at the same time and then pass them separately to
        // armijo and wolfe
        let cond1 = dot(&descent_dir, gradient);
        let cond2 = c2 * duf_at_x0;
        cond1 >= cond2
    };

    let wolfe = weak_wolfe; // Set this if using strong_wolfe instead

    // Interval check
    let should_stop = |num_updates: i32, ai: f64, bi: f64, t: f64| {
        let interval_too_small = (ai - bi).abs() < min_interval;
        let too_many_steps = num_updates > max_steps;

        if interval_too_small && DEBUG_LINE_SEARCH {
            log::info!("line search stopping: interval too small");
        }
        if too_many_steps && DEBUG_LINE_SEARCH {
            log::info!("line search stopping: step count exceeded");
        }

        let need_to_stop = interval_too_small || too_many_steps;

        if need_to_stop && DEBUG_LINE_SEARCH {
            log::info!("stopping early: (i, a, b, t) = {num_updates} {ai} {bi} {t}");
        }
        need_to_stop
    };

    // Consts / initial values
    // TODO: port comments from original

    // const t = 0.002; // for venn_simple.style
    // const t = 0.1; // for tree.style

    let mut a = 0.;
    let mut b = f64::INFINITY;
    let mut t = 1.;
    let mut i = 0;

    if DEBUG_LINE_SEARCH {
        log::info!("line search {xs0:?} {gradfxs0:?}");
    }

    // Main loop + update check
    while !should_stop(i, a, b, t) {
        let FnEvaled {
            f: obj,
            gradf: grad,
            ..
        } = objective_and_gradient(f.clone(), weight, &addv(xs0, &scalev(t, &descent_dir)));
        let is_armijo = armijo(t, obj);
        let is_wolfe = wolfe(t, &grad);
        if DEBUG_LINE_SEARCH {
            log::info!("(i, a, b, t), armijo, wolfe {i} {a} {b} {t} {is_armijo} {is_wolfe}");
        }

        if !is_armijo {
            if DEBUG_LINE_SEARCH {
                log::info!("not armijo");
            }
            b = t;
        } else if !is_wolfe {
            if DEBUG_LINE_SEARCH {
                log::info!("not wolfe");
            }
            a = t;
        } else {
            if DEBUG_LINE_SEARCH {
                log::info!("found good interval");
                log::info!("stopping: (i, a, b, t) = {i} {a} {b} {t}");
            }
            break;
        }

        if b < f64::INFINITY {
            if DEBUG_LINE_SEARCH {
                log::info!("already found armijo");
            }
            t = (a + b) / 2.;
        } else {
            if DEBUG_LINE_SEARCH {
                log::info!("did not find armijo");
            }
            t = 2. * a;
        }

        i += 1;
    }

    t
}

// Precondition the gradient:
// Approximate the inverse of the Hessian times the gradient
// Only using the last `m` gradient/state difference vectors, not building the full h_k matrix (Nocedal p226)

// "Nocedal" refers to the 1999 edition of "Numerical Optimization" by Nocedal and Wright
// https://www.ime.unicamp.br/~pulino/MT404/TextosOnline/NocedalJ.pdf

fn lbfgs_inner(grad_fx_k: &Vector, ss: &[Vector], ys: &[Vector]) -> Vector {
    // TODO: See if rewriting outside the functional style yields speedup (e.g. less copying of matrix objects -> less garbage collection)

    // BACKWARD: for i = k-1 ... k-m
    // The length of any list should be the number of stored vectors
    let rhos: Vec<f64> = ss
        .iter()
        .zip(ys)
        .map(|(s, y)| 1. / (y.dot(s) + EPSD))
        .collect();
    let q_k = grad_fx_k;

    let mut q_i = q_k.clone();
    let mut alphas: Vec<f64> = vec![];
    for ((rho_i, s_i), y_i) in rhos.iter().zip(ss).zip(ys) {
        let q_i_plus_1 = q_i;

        let alpha_i: f64 = rho_i * s_i.dot(&q_i_plus_1);
        q_i = q_i_plus_1 - (y_i * alpha_i);
        alphas.push(alpha_i);
    }
    let q_k_minus_m = q_i;

    let y_km1 = &ys[0];
    let s_km1 = &ss[0];
    let gamma_k = s_km1.dot(y_km1) / (y_km1.dot(y_km1) + EPSD); // according to Nocedal p226, eqn 9.6

    // FORWARD: for i = k-m .. k-1
    let r_k_minus_m = gamma_k * q_k_minus_m;

    // Note that rhos, alphas, ss, and ys are all in order from `k-1` to `k-m` so we just reverse all of them together to go from `k-m` to `k-1`
    let mut r_i = r_k_minus_m;
    for ((rho_i, alpha_i), (s_i, y_i)) in rhos.iter().zip(alphas).zip(ss.iter().zip(ys)).rev() {
        let beta_i: f64 = rho_i * y_i.dot(&r_i);
        let r_i_plus_1 = r_i + s_i * (alpha_i - beta_i);
        r_i = r_i_plus_1;
    }
    let r_k = r_i;

    // result r_k is H_k * grad f(x_k)
    r_k
}

// Outer loop of lbfgs
// See Optimizer.hs for any add'l comments
fn lbfgs(xs: &[f64], gradfxs: &[f64], lbfgs_info: LbfgsParams) -> LbfgsAnswer {
    // Comments for normal BFGS:
    // For x_{k+1}, to compute H_k, we need the (k-1) info
    // Our convention is that we are always working "at" k to compute k+1
    // x_0 doesn't require any H; x_1 (the first step) with k = 0 requires H_0
    // x_2 (the NEXT step) with k=1 requires H_1. For example>
    // x_2 = x_1 - alpha_1 H_1 grad f(x_1)   [GD step]
    // H_1 = V_0 H_0 V_0 + rho_0 s_0 s_0^T   [This is confusing because the book adds an extra +1 to the H index]
    // V_0 = I - rho_0 y_0 s_0^T
    // rho_0 = 1 / y_0^T s_0
    // s_0 = x_1 - x_0
    // y_0 = grad f(x_1) - grad f(x_0)

    if DEBUG_LBFGS {
        log::info!(
            "Starting lbfgs calculation with xs {:?} gradfxs {:?} lbfgs params {:?}",
            xs,
            gradfxs,
            lbfgs_info,
        );
    }

    if lbfgs_info.num_unconstr_steps == 0 {
        // Initialize state
        // Perform normal gradient descent on first step
        // Store x_k, grad f(x_k) so we can compute s_k, y_k on next step

        LbfgsAnswer {
            gradfxs_preconditioned: gradfxs.to_vec(),
            updated_lbfgs_info: LbfgsParams {
                last_state: Some(xs.to_vec()),
                last_grad: Some(gradfxs.to_vec()),
                s_list: vec![],
                y_list: vec![],
                num_unconstr_steps: 1,
                ..lbfgs_info
            },
        }
    } else if lbfgs_info.last_state != None && lbfgs_info.last_grad != None {
        // Our current step is k; the last step is km1 (k_minus_1)
        let x_k = Vector::from_column_slice(xs);
        let grad_fx_k = Vector::from_column_slice(gradfxs);

        let km1 = lbfgs_info.num_unconstr_steps;
        let x_km1 = Vector::from_vec(lbfgs_info.last_state.clone().unwrap());
        let grad_fx_km1 = Vector::from_vec(lbfgs_info.last_grad.clone().unwrap());
        let mut ss_km2 = lbfgs_info
            .s_list
            .clone()
            .into_iter()
            .map(Vector::from_vec)
            .collect();
        let mut ys_km2 = lbfgs_info
            .y_list
            .clone()
            .into_iter()
            .map(Vector::from_vec)
            .collect();

        // Compute s_{k-1} = x_k - x_{k-1} and y_{k-1} = (analogous with grads)
        // Unlike Nocedal, compute the difference vectors first instead of last (same result, just a loop rewrite)
        // Use the updated {s_i} and {y_i}. (If k < m, this reduces to normal BFGS, i.e. we use all the vectors so far)
        // Newest vectors added to front

        let s_km1 = &x_k - x_km1;
        let y_km1 = &grad_fx_k - grad_fx_km1;

        // The limited-memory part: drop stale vectors
        // Haskell `ss` -> JS `ss_km2`; Haskell `ss'` -> JS `ss_km1`
        let mut ss_km1 = vec![s_km1];
        ss_km1.append(&mut ss_km2);
        ss_km1.truncate(lbfgs_info.mem_size.try_into().unwrap());
        let mut ys_km1 = vec![y_km1];
        ys_km1.append(&mut ys_km2);
        ys_km1.truncate(lbfgs_info.mem_size.try_into().unwrap());
        let grad_preconditioned = lbfgs_inner(&grad_fx_k, &ss_km1, &ys_km1);

        // Reset L-BFGS if the result is not a descent direction, and use steepest descent direction
        // https://github.com/JuliaNLSolvers/Optim.jl/issues/143
        // https://github.com/JuliaNLSolvers/Optim.jl/pull/144
        // A descent direction is a vector p s.t. <p `dot` grad_fx_k> < 0
        // If P is a positive definite matrix, then p = -P grad f(x) is a descent dir at x
        let descent_dir_check = -1. * grad_preconditioned.dot(&grad_fx_k);

        if descent_dir_check > 0. {
            log::info!(
                "L-BFGS did not find a descent direction. Resetting correction vectors. {:?}",
                lbfgs_info,
            );
            return LbfgsAnswer {
                gradfxs_preconditioned: gradfxs.to_vec(),
                updated_lbfgs_info: LbfgsParams {
                    last_state: Some(x_k.data.into()),
                    last_grad: Some(grad_fx_k.data.into()),
                    s_list: vec![],
                    y_list: vec![],
                    num_unconstr_steps: 1,
                    ..lbfgs_info
                },
            };
        }

        // Found a direction; update the state
        // TODO: check the curvature condition y_k^T s_k > 0 (8.7) (Nocedal 201)
        // https://github.com/JuliaNLSolvers/Optim.jl/issues/26
        if DEBUG_LBFGS {
            log::info!("Descent direction found. {grad_preconditioned}");
        }

        LbfgsAnswer {
            gradfxs_preconditioned: grad_preconditioned.into_iter().copied().collect(),
            updated_lbfgs_info: LbfgsParams {
                last_state: Some(x_k.data.into()),
                last_grad: Some(grad_fx_k.data.into()),
                s_list: ss_km1.into_iter().map(|v| v.data.into()).collect(),
                y_list: ys_km1.into_iter().map(|v| v.data.into()).collect(),
                num_unconstr_steps: km1 + 1,
                ..lbfgs_info
            },
        }
    } else {
        log::info!("State: {lbfgs_info:?}");
        panic!("Invalid L-BFGS state");
    }
}

fn minimize(
    xs0: &[f64],
    f: FnCached,
    weight: f64,
    lbfgs_info: LbfgsParams,
    num_steps: i32,
) -> OptInfo {
    // TODO: Do a UO convergence check here? Since the EP check is tied to the render cycle...

    log::info!("-------------------------------------");
    log::info!("minimize, num steps, {num_steps}");

    let min_steps = 1;
    if num_steps < min_steps {
        panic!("must step at least {min_steps} times in the optimizer");
    }

    // (10,000 steps / 100ms) * (10 ms / s) (???) = 100k steps/s (on this simple problem (just `sameCenter` or just `contains`, with no line search, and not sure about mem use)
    // this is just a factor of 5 slowdown over the compiled energy function

    let mut xs = xs0.to_vec(); // Don't use xs
    let mut fxs = 0.;
    let mut gradfxs = vec![0.; xs0.len()];
    let mut gradient_preconditioned = gradfxs.clone();
    let mut norm_gradfxs = 0.;
    let mut i = 0;
    let mut t = 0.0001; // NOTE: This const setting will not necessarily work well for a given opt problem.
    let mut failed = false;

    // these will be overwritten so it's OK that they're the wrong length at first
    let mut obj_engs = vec![];
    let mut constr_engs = vec![];

    let mut new_lbfgs_info = lbfgs_info.clone();

    while i < num_steps {
        if contains_nan(&xs) {
            log::info!("xs {xs:?}");
            panic!("NaN in xs");
        }
        FnEvaled {
            f: fxs,
            gradf: gradfxs,
            obj_engs,
            constr_engs,
        } = objective_and_gradient(f.clone(), weight, &xs);
        if contains_nan(&gradfxs) {
            log::info!("gradfxs {gradfxs:?}");
            panic!("NaN in gradfxs");
        }

        LbfgsAnswer {
            gradfxs_preconditioned: gradient_preconditioned,
            updated_lbfgs_info: new_lbfgs_info,
        } = lbfgs(&xs, &gradfxs, new_lbfgs_info.clone());

        // Don't take the Euclidean norm. According to Boyd (485), we should use the Newton descent check, with the norm of the gradient pulled back to the nicer space.
        norm_gradfxs = dot(&gradfxs, &gradient_preconditioned);

        if BREAK_EARLY && unconstrained_converged(norm_gradfxs) {
            // This is on the original gradient, not the preconditioned one
            log::info!(
                "descent converged early, on step {} of {} (per display cycle); stopping early",
                i,
                num_steps,
            );
            break;
        }

        if USE_LINE_SEARCH {
            // The search direction is conditioned (here, by an approximation of the inverse of the Hessian at the point)
            t = aw_line_search(&xs, f.clone(), weight, &gradient_preconditioned, fxs);
        }

        let norm_grad = norm_list(&gradfxs);

        if DEBUG_GRAD_DESCENT {
            log::info!("-----");
            log::info!("i {i}");
            log::info!("num steps per display cycle {num_steps}");
            log::info!("input (xs): {xs:?}");
            log::info!("energy (f(xs)): {fxs}");
            log::info!("grad (grad(f)(xs)) : {gradfxs:?}");
            log::info!("|grad f(x)|: {norm_grad}");
            log::info!("t {t} use line search: {USE_LINE_SEARCH}");
        }

        if fxs.is_nan() || norm_grad.is_nan() {
            log::info!("-----");

            let path_map = xs.iter().zip(&gradfxs);

            log::info!("[current val, gradient of val] {path_map:?}");

            for (x, dx) in path_map {
                if dx.is_nan() {
                    log::info!("NaN in varying val's gradient (current val): {x}");
                }
            }

            log::info!("i {i}");
            log::info!("num steps per display cycle {num_steps}");
            log::info!("input (xs): {xs:?}");
            log::info!("energy (f(xs)): {fxs:?}");
            log::info!("grad (grad(f)(xs)): {gradfxs:?}");
            log::info!("|grad f(x)|: {norm_grad}");
            log::info!("t {t}, use line search: {USE_LINE_SEARCH}");
            failed = true;
            break;
            //panic!("NaN reached in optimization energy or gradient norm!");
        }

        xs = subv(&xs, &scalev(t, &gradient_preconditioned)); // The GD update uses the conditioned search direction, as well as the timestep found by moving along it
        i += 1;
    }

    // TODO: Log stats for last one?

    return OptInfo {
        xs,
        energy_val: fxs,
        norm_grad: norm_gradfxs,
        new_lbfgs_info,
        gradient: gradfxs,
        gradient_preconditioned,
        failed,
        obj_engs,
        constr_engs,
    };
}

fn gen_opt_problem(grad_mask: Vec<bool>, obj_mask: Vec<bool>, constr_mask: Vec<bool>) -> Params {
    let last_gradient = vec![0.; grad_mask.len()];
    let last_gradient_preconditioned = vec![0.; grad_mask.len()];
    Params {
        grad_mask,
        obj_mask,
        constr_mask,

        last_gradient,
        last_gradient_preconditioned,

        weight: INIT_CONSTRAINT_WEIGHT,
        uo_round: 0,
        ep_round: 0,
        opt_status: OptStatus::UnconstrainedRunning,

        lbfgs_info: DEFAULT_LBFGS_PARAMS,

        last_uo_state: None,
        last_uo_energy: None,
        last_obj_energies: None,
        last_constr_energies: None,

        last_ep_state: None,
        last_ep_energy: None,
    }
}

fn contains_nan(number_list: &[f64]) -> bool {
    number_list.iter().any(|n| n.is_nan())
}

fn to_js_value(value: &(impl Serialize + ?Sized)) -> Result<JsValue, serde_wasm_bindgen::Error> {
    // ts-rs expects `Option::None` to become `null` instead of `undefined`
    value.serialize(&serde_wasm_bindgen::Serializer::new().serialize_missing_as_null(true))
}

#[wasm_bindgen]
pub fn penrose_init() {
    // https://docs.rs/console_error_panic_hook/0.1.7/console_error_panic_hook/#usage
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    // https://docs.rs/console_log/0.2.0/console_log/#example
    console_log::init_with_level(Level::Warn).unwrap();
}

#[wasm_bindgen]
pub fn penrose_poly_roots(v: &mut [f64]) {
    let n = v.len();
    // https://en.wikipedia.org/wiki/Companion_matrix
    let mut m = nalgebra::DMatrix::<f64>::zeros(n, n);
    for i in 0..(n - 1) {
        m[(i + 1, i)] = 1.;
        m[(i, n - 1)] = -v[i];
    }
    m[(n - 1, n - 1)] = -v[n - 1];

    // the characteristic polynomial of the companion matrix is equal to the original polynomial, so
    // by finding the eigenvalues of the companion matrix, we get the roots of its characteristic
    // polynomial and thus of the original polynomial
    let r = m.complex_eigenvalues();
    for i in 0..n {
        let z = r[i];
        // as mentioned in the `polyRoots` docstring in `engine/AutodiffFunctions`, we discard any
        // non-real root and replace with `NaN`
        v[i] = if z.im == 0. { z.re } else { f64::NAN };
    }
}

#[wasm_bindgen]
pub fn penrose_gen_opt_problem(
    grad_mask: Vec<Bool>,
    obj_mask: Vec<Bool>,
    constr_mask: Vec<Bool>,
) -> JsValue {
    let params: Params = gen_opt_problem(bools(grad_mask), bools(obj_mask), bools(constr_mask));
    to_js_value(&params).unwrap()
}

#[wasm_bindgen]
pub fn penrose_step(state: JsValue, f: JsValue, steps: i32) -> JsValue {
    let unstepped: OptState = serde_wasm_bindgen::from_value(state).unwrap();
    let stepped: OptState = step(unstepped, f, steps);
    to_js_value(&stepped).unwrap()
}
