mod lbfgs;

use log::Level;
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use wasm_bindgen::{prelude::wasm_bindgen, JsValue};

// we take in functions living in JavaScript-land; the `js-sys` crate does define a `Function` type
// with `call0` and `call3` methods, but those require us to deal with the parameters and return
// values as `JsValue`s, so the easiest way to call these functions is to instead define our own
// typed `call_grad` and `call_stop` functions in JavaScript and then use `wasm-bindgen` to import
// them into Rust here; as far as I know, the `extern "C"` syntax is historical and in this case
// we're not actually doing anything related to C, but if you try to write `extern` without "C" then
// `rustfmt` adds it back
#[wasm_bindgen(module = "/call.js")]
extern "C" {
    fn call_grad(f: JsValue, x: &[f64], weight: f64, grad: &mut [f64]) -> f64;
    fn call_stop(stop: JsValue) -> bool;
}

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
}

#[derive(Clone, Deserialize, Serialize, TS)]
#[ts(export)]
struct Params {
    #[serde(rename = "optStatus")]
    opt_status: OptStatus,
    /// Constraint weight for exterior point method
    weight: f64,
    /// Info for unconstrained optimization
    #[serde(rename = "UOround")]
    uo_round: usize,
    #[serde(rename = "lastUOstate")]
    last_uo_state: Option<Vec<f64>>,
    #[serde(rename = "lastUOenergy")]
    last_uo_energy: Option<f64>,

    /// Info for exterior point method
    #[serde(rename = "EPround")]
    ep_round: usize,
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

// Returned after a call to `minimize`
struct OptInfo {
    xs: Vec<f64>,
    energy_val: f64,
    norm_grad: f64,
    new_lbfgs_info: LbfgsParams,
    gradient: Vec<f64>,
    gradient_preconditioned: Vec<f64>,
    failed: bool,
}

// Intial weight for constraints
const INIT_CONSTRAINT_WEIGHT: f64 = 1e3;

const DEFAULT_LBFGS_PARAMS: LbfgsParams = LbfgsParams {
    last_state: None,
    last_grad: None,
    s_list: vec![],
    y_list: vec![],
};

// growth factor for constraint weights
const WEIGHT_GROWTH_FACTOR: f64 = 10.;

// EP method convergence criteria
const EP_STOP: f64 = 1e-3;
// const EP_STOP: f64 = 1e-5;
// const EP_STOP: f64 = 1e-7;

// Unconstrained method convergence criteria
// TODO. This should REALLY be 10e-10
// NOTE: The new autodiff + line search seems to be really sensitive to this parameter (`UO_STOP`). It works for 1e-2, but the line search ends up with too-small intervals with 1e-5
const UO_STOP: f64 = 1e-2;
// const UO_STOP: f64 = 1e-3;
// const UO_STOP: f64 = 1e-5;
// const UO_STOP: f64 = 10;

const DEBUG_GRAD_DESCENT: bool = false;
const BREAK_EARLY: bool = true;

////////////////////////////////////////////////////////////////////////////////

fn norm_list(xs: &[f64]) -> f64 {
    let sum_squares: f64 = xs.iter().map(|e| e * e).sum();
    sum_squares.sqrt()
}

fn subv(xs: &[f64], ys: &[f64]) -> Vec<f64> {
    xs.iter().zip(ys).map(|(x, y)| x - y).collect()
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

fn step_until(
    f: impl FnMut(&[f64], f64, &mut [f64]) -> f64,
    x: &mut [f64],
    state: Params,
    stop: impl FnMut() -> bool,
) -> Params {
    let mut opt_params = state.clone();
    let Params {
        opt_status, weight, ..
    } = opt_params;
    let mut xs = x.to_vec();

    log::info!("===============");
    log::info!(
        "step | weight: {weight} | EP round: {} | UO round: {}",
        opt_params.ep_round,
        opt_params.uo_round,
    );

    match opt_status {
        OptStatus::NewIter => {
            return Params {
                weight: INIT_CONSTRAINT_WEIGHT,
                uo_round: 0,
                ep_round: 0,
                opt_status: OptStatus::UnconstrainedRunning,
                lbfgs_info: DEFAULT_LBFGS_PARAMS,
                ..state
            }
        }

        OptStatus::UnconstrainedRunning => {
            // NOTE: use cached varying values

            let res = minimize(f, &xs, state.weight, state.lbfgs_info, stop);
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
                ..
            } = res;

            opt_params.last_uo_state = Some(xs.clone());
            opt_params.last_uo_energy = Some(energy_val);
            opt_params.uo_round += 1;
            opt_params.lbfgs_info = new_lbfgs_info;
            opt_params.last_gradient = gradient;
            opt_params.last_gradient_preconditioned = gradient_preconditioned;

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
                    "Took some steps. Current energy {energy_val} gradient norm {norm_grad}",
                );
            }
            if failed {
                log::warn!("Error detected after stepping");
                opt_params.opt_status = OptStatus::Error;
                return opt_params;
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
                    opt_params.last_uo_state.as_ref().unwrap(),
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
                opt_params.ep_round += 1;
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

    x.copy_from_slice(&xs);
    opt_params
}

fn minimize(
    mut f: impl FnMut(&[f64], f64, &mut [f64]) -> f64,
    xs0: &[f64],
    weight: f64,
    lbfgs_info: LbfgsParams,
    mut stop: impl FnMut() -> bool,
) -> OptInfo {
    // TODO: Do a UO convergence check here? Since the EP check is tied to the render cycle...

    log::info!("-------------------------------------");
    log::info!("minimize");

    let mut xs = xs0.to_vec(); // Don't use xs
    let mut fxs = 0.;
    let mut gradfxs = vec![0.; xs0.len()];
    let mut gradient_preconditioned = gradfxs.clone();
    let mut norm_gradfxs = 0.;

    let cfg = lbfgs::Config {
        m: 17,
        armijo: 0.001,
        wolfe: 0.9,
        min_interval: 1e-9,
        max_steps: 10,
        epsd: 1e-11,
    };

    let mut state = if let (Some(x), Some(grad)) = (lbfgs_info.last_state, lbfgs_info.last_grad) {
        lbfgs::State {
            x,
            grad,
            s_y: lbfgs_info
                .s_list
                .into_iter()
                .zip(lbfgs_info.y_list)
                .collect(),
        }
    } else {
        lbfgs::first_step(cfg, |x, grad| f(x, weight, grad), &mut xs)
    };

    let failed = lbfgs::step_until(
        cfg,
        |x, grad| f(x, weight, grad),
        &mut xs,
        &mut state,
        |info| {
            if stop() {
                return Some(false);
            }

            if contains_nan(&info.state.x) {
                log::info!("xs {:?}", info.state.x);
                panic!("NaN in xs");
            }
            fxs = info.fx;
            gradfxs.copy_from_slice(&info.state.grad);
            if contains_nan(&gradfxs) {
                log::info!("gradfxs {gradfxs:?}");
                panic!("NaN in gradfxs");
            }

            gradient_preconditioned.copy_from_slice(info.r);

            // Don't take the Euclidean norm. According to Boyd (485), we should use the Newton descent check, with the norm of the gradient pulled back to the nicer space.
            norm_gradfxs = dot(&gradfxs, &gradient_preconditioned);

            if BREAK_EARLY && unconstrained_converged(norm_gradfxs) {
                // This is on the original gradient, not the preconditioned one
                log::info!("descent converged, stopping early");
                return Some(false);
            }

            let norm_grad = norm_list(&gradfxs);

            if DEBUG_GRAD_DESCENT {
                log::info!("-----");
                log::info!("input (xs): {:?}", info.state.x);
                log::info!("energy (f(xs)): {fxs}");
                log::info!("grad (grad(f)(xs)) : {gradfxs:?}");
                log::info!("|grad f(x)|: {norm_grad}");
                log::info!("t {}", info.t);
            }

            if fxs.is_nan() || norm_grad.is_nan() {
                log::info!("-----");

                let path_map = info.state.x.iter().zip(&gradfxs);

                log::info!("[current val, gradient of val] {path_map:?}");

                for (x, dx) in path_map {
                    if dx.is_nan() {
                        log::info!("NaN in varying val's gradient (current val): {x}");
                    }
                }

                log::info!("input (xs): {:?}", info.state.x);
                log::info!("energy (f(xs)): {fxs:?}");
                log::info!("grad (grad(f)(xs)): {gradfxs:?}");
                log::info!("|grad f(x)|: {norm_grad}");
                log::info!("t {}", info.t);
                return Some(true);
            }

            None
        },
    );

    // TODO: Log stats for last one?

    let (s_list, y_list) = state.s_y.into_iter().unzip();
    OptInfo {
        xs,
        energy_val: fxs,
        norm_grad: norm_gradfxs,
        new_lbfgs_info: LbfgsParams {
            last_state: Some(state.x),
            last_grad: Some(state.grad),
            s_list,
            y_list,
        },
        gradient: gradfxs,
        gradient_preconditioned,
        failed,
    }
}

fn start(n: usize) -> Params {
    let last_gradient = vec![0.; n];
    let last_gradient_preconditioned = vec![0.; n];
    Params {
        last_gradient,
        last_gradient_preconditioned,

        weight: INIT_CONSTRAINT_WEIGHT,
        uo_round: 0,
        ep_round: 0,
        opt_status: OptStatus::UnconstrainedRunning,

        lbfgs_info: DEFAULT_LBFGS_PARAMS,

        last_uo_state: None,
        last_uo_energy: None,

        last_ep_state: None,
        last_ep_energy: None,
    }
}

fn contains_nan(number_list: &[f64]) -> bool {
    number_list.iter().any(|n| n.is_nan())
}

fn to_js_value(value: &(impl Serialize + ?Sized)) -> Result<JsValue, serde_wasm_bindgen::Error> {
    // ts-rs expects us to produce data that looks like JSON
    value.serialize(&serde_wasm_bindgen::Serializer::json_compatible())
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
pub fn penrose_start(n: usize) -> JsValue {
    let params: Params = start(n);
    to_js_value(&params).unwrap()
}

#[wasm_bindgen]
pub fn penrose_step_until(f: JsValue, x: &mut [f64], state: JsValue, stop: JsValue) -> JsValue {
    let unstepped: Params = serde_wasm_bindgen::from_value(state).unwrap();
    // a `JsValue` is basically just an integer representing an index into an array that
    // `wasm-bindgen` maintains on the JavaScript side, so they're pretty much free to `.clone()`
    let stepped: Params = step_until(
        |x, weight, grad| call_grad(f.clone(), x, weight, grad),
        x,
        unstepped,
        || call_stop(stop.clone()),
    );
    to_js_value(&stepped).unwrap()
}
