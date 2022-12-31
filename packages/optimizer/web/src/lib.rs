pub mod builtins;

use log::Level;
use penrose_optimizer::{
    gen_opt_problem, step, Bool, Compiled, OptState, Params, INIT_CONSTRAINT_WEIGHT,
};
use serde::Serialize;
use wasm_bindgen::{prelude::wasm_bindgen, JsValue};

type Func =
    fn(inputs: *const f64, mask: *const Bool, gradient: *mut f64, secondary: *mut f64) -> f64;

#[derive(Clone)]
struct FuncPtr {
    f: Func,
}

impl Compiled for FuncPtr {
    fn call(
        &self,
        inputs: &[f64],
        mask: &[Bool],
        gradient: &mut [f64],
        secondary: &mut [f64],
    ) -> f64 {
        (self.f)(
            inputs.as_ptr(),
            mask.as_ptr(),
            gradient.as_mut_ptr(),
            secondary.as_mut_ptr(),
        )
    }
}

fn bools(v: Vec<Bool>) -> Vec<bool> {
    v.into_iter().map(|n| n > 0).collect()
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
pub fn penrose_call(
    p: usize,
    inputs: &[f64],
    mask: &[Bool],
    gradient: &mut [f64],
    secondary: &mut [f64],
) -> f64 {
    let f = unsafe { std::mem::transmute::<usize, Func>(p) };
    f(
        inputs.as_ptr(),
        mask.as_ptr(),
        gradient.as_mut_ptr(),
        secondary.as_mut_ptr(),
    )
}

// `wasm-bindgen` doesn't let us export constants
#[wasm_bindgen]
pub fn penrose_get_init_constraint_weight() -> f64 {
    INIT_CONSTRAINT_WEIGHT
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
pub fn penrose_step(state: JsValue, p: usize, steps: i32) -> JsValue {
    let unstepped: OptState = serde_wasm_bindgen::from_value(state).unwrap();
    let stepped: OptState = step(
        unstepped,
        &FuncPtr {
            f: unsafe { std::mem::transmute::<usize, Func>(p) },
        },
        steps,
    );
    to_js_value(&stepped).unwrap()
}
