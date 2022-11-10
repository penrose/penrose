mod builtins;
mod memory;

type Vector = nalgebra::DVector<f64>;
type Matrix = nalgebra::DMatrix<f64>;

type Compiled = fn(inputs: *const f64, gradient: *mut f64, secondary: *mut f64) -> f64;

#[derive(Clone, PartialEq)]
#[repr(u8)]
enum InputMeta {
    Sampler,
    Pending,
}

#[derive(Clone, Copy, PartialEq)]
enum OptStatus {
    UnconstrainedRunning,
    UnconstrainedConverged,
    EPConverged,
    Error,
}

#[derive(Clone, Debug)]
struct LbfgsParams {
    last_state: Option<Vector>,
    last_grad: Option<Vector>,
    s_list: Vec<Vector>,
    y_list: Vec<Vector>,
    num_unconstr_steps: i32,
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

#[derive(Clone)]
struct Params {
    opt_status: OptStatus,
    weight: f64,
    uo_round: i32,
    last_uo_state: Option<Vec<f64>>,
    last_uo_energy: Option<f64>,
    last_obj_energies: Option<Vec<f64>>,
    last_constr_energies: Option<Vec<f64>>,

    ep_round: i32,
    last_ep_state: Option<Vec<f64>>,
    last_ep_energy: Option<f64>,

    last_gradient: Vec<f64>,
    last_gradient_preconditioned: Vec<f64>,

    lbfgs_info: LbfgsParams,

    objective_and_gradient: FnCached,
}

#[derive(Clone)]
struct FnCached {
    inputs: Vec<InputMeta>,
    len_obj_engs: usize,
    len_constr_engs: usize,
    f: Compiled,
}

struct State {
    varying_values: Vec<f64>,
    params: Params,
}

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

const WEIGHT_GROWTH_FACTOR: f64 = 10.;

const EP_STOP: f64 = 1e-3;

const EPSD: f64 = 1e-11;

const UO_STOP: f64 = 1e-2;

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
    norm_grad < UO_STOP
}

fn objective_and_gradient(f: &FnCached, weight: f64, xs: &[f64]) -> FnEvaled {
    let len_inputs = xs.len();
    let len_gradient = len_inputs + 1;
    let len_secondary = f.len_obj_engs + f.len_constr_engs;

    let mut inputs = vec![0.; len_gradient];
    inputs[..len_inputs].copy_from_slice(xs);
    inputs[len_inputs] = weight;
    let mut gradient = vec![0.; len_gradient];
    let mut secondary = vec![0.; len_secondary];

    let energy = (f.f)(
        inputs.as_ptr(),
        gradient.as_mut_ptr(),
        secondary.as_mut_ptr(),
    );
    FnEvaled {
        f: energy,
        gradf: gradient
            .into_iter()
            .zip(&f.inputs)
            .map(|(x, meta)| if *meta == InputMeta::Sampler { x } else { 0. })
            .collect(),
        obj_engs: secondary[..f.len_obj_engs].to_vec(),
        constr_engs: secondary[f.len_obj_engs..].to_vec(),
    }
}

fn ep_converged(xs0: &[f64], xs1: &[f64], fxs0: f64, fxs1: f64) -> bool {
    let state_change = norm_list(&subv(xs1, xs0));
    let energy_change = (fxs1 - fxs0).abs();
    log::info!(
        "epConverged?: stateChange: {} | energyChange: {}",
        state_change,
        energy_change,
    );

    state_change < EP_STOP || energy_change < EP_STOP
}

fn step(state: State, steps: i32) -> State {
    let mut opt_params = state.params.clone();
    let Params {
        opt_status, weight, ..
    } = opt_params;
    let mut xs = state.varying_values.clone();

    log::info!("===============");
    log::info!(
        "step | weight: {} | EP round: {} | UO round: {}",
        weight,
        opt_params.ep_round,
        opt_params.uo_round,
    );

    match opt_status {
        OptStatus::UnconstrainedRunning => {
            let res = minimize(
                &xs,
                state.params.objective_and_gradient,
                state.params.weight,
                state.params.lbfgs_info,
                steps,
            );
            xs = res.xs;

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

            if unconstrained_converged(norm_grad) {
                opt_params.opt_status = OptStatus::UnconstrainedConverged;
                opt_params.lbfgs_info = DEFAULT_LBFGS_PARAMS;
                log::info!(
                    "Unconstrained converged with energy {} gradient norm {}",
                    energy_val,
                    norm_grad,
                );
            } else {
                opt_params.opt_status = OptStatus::UnconstrainedRunning;
                log::info!(
                    "Took {} steps. Current energy {} gradient norm {}",
                    steps,
                    energy_val,
                    norm_grad,
                );
            }
            if failed {
                log::warn!("Error detected after stepping");
                opt_params.opt_status = OptStatus::Error;
                return State {
                    params: opt_params,
                    ..state
                };
            }
        }
        OptStatus::UnconstrainedConverged => {
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

            opt_params.last_ep_state = opt_params.last_uo_state.clone();
            opt_params.last_ep_energy = opt_params.last_uo_energy;
        }
        OptStatus::EPConverged => {
            log::info!("step: EP converged");
            return state;
        }
        OptStatus::Error => {
            log::warn!("step: Error");
            return state;
        }
    }

    State {
        varying_values: xs.clone(),
        params: opt_params,
        ..state
    }
}

fn aw_line_search(xs0: &[f64], f: &FnCached, weight: f64, gradfxs0: &[f64], fxs0: f64) -> f64 {
    let max_steps = 10;

    let descent_dir = negv(gradfxs0);

    let duf_at_x0 = dot(&descent_dir, &objective_and_gradient(f, weight, xs0).gradf);
    let min_interval = 10e-10;

    let c1 = 0.001;
    let c2 = 0.9;

    let armijo = |ti: f64, objective: f64| -> bool {
        let cond1 = objective;
        let cond2 = fxs0 + c1 * ti * duf_at_x0;
        cond1 <= cond2
    };

    let weak_wolfe = |_ti: f64, gradient: &[f64]| {
        let cond1 = dot(&descent_dir, gradient);
        let cond2 = c2 * duf_at_x0;
        cond1 >= cond2
    };

    let wolfe = weak_wolfe;

    let should_stop = |num_updates: i32, ai: f64, bi: f64, _t: f64| {
        let interval_too_small = (ai - bi).abs() < min_interval;
        let too_many_steps = num_updates > max_steps;

        let need_to_stop = interval_too_small || too_many_steps;

        need_to_stop
    };

    let mut a = 0.;
    let mut b = f64::INFINITY;
    let mut t = 1.;
    let mut i = 0;

    while !should_stop(i, a, b, t) {
        let FnEvaled {
            f: obj,
            gradf: grad,
            ..
        } = objective_and_gradient(f, weight, &addv(xs0, &scalev(t, &descent_dir)));
        let is_armijo = armijo(t, obj);
        let is_wolfe = wolfe(t, &grad);

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

        i += 1;
    }

    t
}

fn lbfgs_inner(grad_fx_k: &Vector, ss: &[Vector], ys: &[Vector]) -> Vector {
    fn estimate_hess(y_km1: &Vector, s_km1: &Vector) -> Matrix {
        let gamma_k = s_km1.dot(y_km1) / (y_km1.dot(y_km1) + EPSD);
        let n = y_km1.len();
        Matrix::identity(n, n) * gamma_k
    }

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

    let h_0_k = estimate_hess(&ys[0], &ss[0]);

    let r_k_minus_m = h_0_k * q_k_minus_m;

    let mut r_i = r_k_minus_m;
    for ((rho_i, alpha_i), (s_i, y_i)) in rhos.iter().zip(alphas).zip(ss.iter().zip(ys)).rev() {
        let beta_i: f64 = rho_i * y_i.dot(&r_i);
        let r_i_plus_1 = r_i + s_i * (alpha_i - beta_i);
        r_i = r_i_plus_1;
    }
    let r_k = r_i;

    r_k
}

fn lbfgs(xs: &[f64], gradfxs: &[f64], lbfgs_info: LbfgsParams) -> LbfgsAnswer {
    if lbfgs_info.num_unconstr_steps == 0 {
        LbfgsAnswer {
            gradfxs_preconditioned: gradfxs.to_vec(),
            updated_lbfgs_info: LbfgsParams {
                last_state: Some(Vector::from_column_slice(xs)),
                last_grad: Some(Vector::from_column_slice(gradfxs)),
                s_list: vec![],
                y_list: vec![],
                num_unconstr_steps: 1,
                ..lbfgs_info
            },
        }
    } else if lbfgs_info.last_state != None && lbfgs_info.last_grad != None {
        let x_k = Vector::from_column_slice(xs);
        let grad_fx_k = Vector::from_column_slice(gradfxs);

        let km1 = lbfgs_info.num_unconstr_steps;
        let x_km1 = lbfgs_info.last_state.clone().unwrap();
        let grad_fx_km1 = lbfgs_info.last_grad.clone().unwrap();
        let mut ss_km2 = lbfgs_info.s_list.clone();
        let mut ys_km2 = lbfgs_info.y_list.clone();

        let s_km1 = &x_k - x_km1;
        let y_km1 = &grad_fx_k - grad_fx_km1;

        let mut ss_km1 = vec![s_km1];
        ss_km1.append(&mut ss_km2);
        ss_km1.truncate(lbfgs_info.mem_size.try_into().unwrap());
        let mut ys_km1 = vec![y_km1];
        ys_km1.append(&mut ys_km2);
        ys_km1.truncate(lbfgs_info.mem_size.try_into().unwrap());
        let grad_preconditioned = lbfgs_inner(&grad_fx_k, &ss_km1, &ys_km1);

        let descent_dir_check = -1. * grad_preconditioned.dot(&grad_fx_k);

        if descent_dir_check > 0. {
            log::info!(
                "L-BFGS did not find a descent direction. Resetting correction vectors. {:?}",
                lbfgs_info,
            );
            return LbfgsAnswer {
                gradfxs_preconditioned: gradfxs.to_vec(),
                updated_lbfgs_info: LbfgsParams {
                    last_state: Some(x_k),
                    last_grad: Some(grad_fx_k),
                    s_list: vec![],
                    y_list: vec![],
                    num_unconstr_steps: 1,
                    ..lbfgs_info
                },
            };
        }

        LbfgsAnswer {
            gradfxs_preconditioned: grad_preconditioned.into_iter().copied().collect(),
            updated_lbfgs_info: LbfgsParams {
                last_state: Some(x_k),
                last_grad: Some(grad_fx_k),
                s_list: ss_km1,
                y_list: ys_km1,
                num_unconstr_steps: km1 + 1,
                ..lbfgs_info
            },
        }
    } else {
        log::info!("State: {:?}", lbfgs_info);
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
    log::info!("-------------------------------------");
    log::info!("minimize, num steps, {}", num_steps);

    let min_steps = 1;
    if num_steps < min_steps {
        panic!("must step at least {} times in the optimizer", min_steps);
    }

    let mut xs = xs0.to_vec();
    let mut fxs = 0.;
    let mut gradfxs = vec![0.; xs0.len()];
    let mut gradient_preconditioned = gradfxs.clone();
    let mut norm_gradfxs = 0.;
    let mut i = 0;
    let mut t;
    let mut failed = false;

    let mut obj_engs = vec![];
    let mut constr_engs = vec![];

    let mut new_lbfgs_info = lbfgs_info.clone();

    while i < num_steps {
        if contains_nan(&xs) {
            log::info!("xs {:?}", xs);
            panic!("NaN in xs");
        }
        FnEvaled {
            f: fxs,
            gradf: gradfxs,
            obj_engs,
            constr_engs,
        } = objective_and_gradient(&f, weight, &xs);
        if contains_nan(&gradfxs) {
            log::info!("gradfxs {:?}", gradfxs);
            panic!("NaN in gradfxs");
        }

        LbfgsAnswer {
            gradfxs_preconditioned: gradient_preconditioned,
            updated_lbfgs_info: new_lbfgs_info,
        } = lbfgs(&xs, &gradfxs, new_lbfgs_info.clone());

        norm_gradfxs = dot(&gradfxs, &gradient_preconditioned);

        if unconstrained_converged(norm_gradfxs) {
            log::info!(
                "descent converged early, on step {} of {} (per display cycle); stopping early",
                i,
                num_steps,
            );
            break;
        }

        t = aw_line_search(&xs, &f, weight, &gradient_preconditioned, fxs);

        let norm_grad = norm_list(&gradfxs);

        if fxs.is_nan() || norm_grad.is_nan() {
            log::info!("-----");

            let path_map = xs.iter().zip(&gradfxs);

            log::info!("[current val, gradient of val] {:?}", path_map);

            for (x, dx) in path_map {
                if dx.is_nan() {
                    log::info!("NaN in varying val's gradient (current val): {}", x);
                }
            }

            log::info!("i {}", i);
            log::info!("num steps per display cycle {}", num_steps);
            log::info!("input (xs): {:?}", xs);
            log::info!("energy (f(xs)): {:?}", fxs);
            log::info!("grad (grad(f)(xs)): {:?}", gradfxs);
            log::info!("|grad f(x)|: {}", norm_grad);
            log::info!("t {}", t);
            failed = true;
            break;
        }

        xs = subv(&xs, &scalev(t, &gradient_preconditioned));
        i += 1;
    }

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

fn gen_opt_problem(
    inputs: &[InputMeta],
    len_obj_engs: usize,
    len_constr_engs: usize,
    f: Compiled,
) -> Params {
    let weight = INIT_CONSTRAINT_WEIGHT;

    let objective_and_gradient = FnCached {
        inputs: inputs.to_vec(),
        len_obj_engs,
        len_constr_engs,
        f,
    };

    Params {
        last_gradient: vec![0.; inputs.len()],
        last_gradient_preconditioned: vec![0.; inputs.len()],

        objective_and_gradient,

        weight,
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

fn step_until_convergence(state: State, num_steps: i32) -> State {
    let mut current_state = state;
    while current_state.params.opt_status != OptStatus::Error && !state_converged(&current_state) {
        current_state = step(current_state, num_steps);
    }
    current_state
}

fn state_converged(state: &State) -> bool {
    state.params.opt_status == OptStatus::EPConverged
}

#[no_mangle]
fn penrose_input_meta_sampler_byte() -> u8 {
    InputMeta::Sampler as u8
}

#[no_mangle]
fn penrose_input_meta_pending_byte() -> u8 {
    InputMeta::Pending as u8
}

#[no_mangle]
fn penrose_converge(
    f: Compiled,
    inputs: &[InputMeta],
    len_obj_engs: usize,
    len_constr_engs: usize,
    varying_values: &mut [f64],
) {
    let initial_state = State {
        varying_values: varying_values.to_vec(),
        params: gen_opt_problem(inputs, len_obj_engs, len_constr_engs, f),
    };
    let optimized_state = step_until_convergence(initial_state, 10000);
    varying_values.copy_from_slice(&optimized_state.varying_values);
}
