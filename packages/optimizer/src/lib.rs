// WebAssembly doesn't have a lot of math functions built in, so we export all these to allow our
// JIT-compiled module to put them into its `table` and `call_indirect` them. We need to be very
// careful about the names of all the functions we export, though, because the Rust-to-Wasm compiler
// emits functions with the same names as these `f64` methods we're calling. For instance, if we
// name our exported function the same as the method it's calling, the emitted Wasm code will simply
// try to call itself recursively. To avoid this issue, we use elongated names.

#[no_mangle]
fn arccosine(x: f64) -> f64 {
    x.acos()
}

#[no_mangle]
fn hyperbolic_arccosine(x: f64) -> f64 {
    x.acosh()
}

#[no_mangle]
fn arcsine(x: f64) -> f64 {
    x.asin()
}

#[no_mangle]
fn hyperbolic_arcsine(x: f64) -> f64 {
    x.asinh()
}

#[no_mangle]
fn arctangent(x: f64) -> f64 {
    x.atan()
}

#[no_mangle]
fn hyperbolic_arctangent(x: f64) -> f64 {
    x.atanh()
}

#[no_mangle]
fn four_quadrant_arctangent(y: f64, x: f64) -> f64 {
    y.atan2(x)
}

#[no_mangle]
fn cube_root(x: f64) -> f64 {
    x.cbrt()
}

#[no_mangle]
fn cosine(x: f64) -> f64 {
    x.cos()
}

#[no_mangle]
fn hyperbolic_cosine(x: f64) -> f64 {
    x.cosh()
}

#[no_mangle]
fn exponential(x: f64) -> f64 {
    x.exp()
}

#[no_mangle]
fn exponential_minus_one(x: f64) -> f64 {
    x.exp_m1()
}

#[no_mangle]
fn natural_logarithm(x: f64) -> f64 {
    x.ln()
}

#[no_mangle]
fn natural_logarithm_one_plus(x: f64) -> f64 {
    x.ln_1p()
}

#[no_mangle]
fn base_ten_logarithm(x: f64) -> f64 {
    x.log10()
}

#[no_mangle]
fn base_two_logarithm(x: f64) -> f64 {
    x.log2()
}

#[no_mangle]
fn power(x: f64, y: f64) -> f64 {
    x.powf(y)
}

#[no_mangle]
fn sine(x: f64) -> f64 {
    x.sin()
}

#[no_mangle]
fn hyperbolic_sine(x: f64) -> f64 {
    x.sinh()
}

#[no_mangle]
fn tangent(x: f64) -> f64 {
    x.tan()
}

#[no_mangle]
fn hyperbolic_tangent(x: f64) -> f64 {
    x.tanh()
}

#[no_mangle]
fn step(f: fn(*const f64) -> f64, x: f64) -> f64 {
    let mut v = nalgebra::Vector1::new(x);
    let p = v.as_ptr();
    v.neg_mut();
    f(p)
}
