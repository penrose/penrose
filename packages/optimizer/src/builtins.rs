// WebAssembly doesn't have a lot of math functions built in, so we export all these to allow our
// JIT-compiled module to put them into its `table` and `call_indirect` them. We need to be very
// careful about the names of all the functions we export, though, because the Rust-to-Wasm compiler
// emits functions with the same names as these `f64` methods we're calling. For instance, if we
// name our exported function the same as the method it's calling, the emitted Wasm code will simply
// try to call itself recursively. To avoid this issue, we prefix each name with `penrose_`.

#[no_mangle]
fn penrose_acos(x: f64) -> f64 {
    x.acos()
}

#[no_mangle]
fn penrose_acosh(x: f64) -> f64 {
    x.acosh()
}

#[no_mangle]
fn penrose_asin(x: f64) -> f64 {
    x.asin()
}

#[no_mangle]
fn penrose_asinh(x: f64) -> f64 {
    x.asinh()
}

#[no_mangle]
fn penrose_atan(x: f64) -> f64 {
    x.atan()
}

#[no_mangle]
fn penrose_atanh(x: f64) -> f64 {
    x.atanh()
}

#[no_mangle]
fn penrose_atan2(y: f64, x: f64) -> f64 {
    y.atan2(x)
}

#[no_mangle]
fn penrose_cbrt(x: f64) -> f64 {
    x.cbrt()
}

#[no_mangle]
fn penrose_cos(x: f64) -> f64 {
    x.cos()
}

#[no_mangle]
fn penrose_cosh(x: f64) -> f64 {
    x.cosh()
}

#[no_mangle]
fn penrose_exp(x: f64) -> f64 {
    x.exp()
}

#[no_mangle]
fn penrose_expm1(x: f64) -> f64 {
    x.exp_m1()
}

#[no_mangle]
fn penrose_log(x: f64) -> f64 {
    x.ln()
}

#[no_mangle]
fn penrose_log1p(x: f64) -> f64 {
    x.ln_1p()
}

#[no_mangle]
fn penrose_log10(x: f64) -> f64 {
    x.log10()
}

#[no_mangle]
fn penrose_log2(x: f64) -> f64 {
    x.log2()
}

#[no_mangle]
fn penrose_pow(x: f64, y: f64) -> f64 {
    x.powf(y)
}

#[no_mangle]
fn penrose_sin(x: f64) -> f64 {
    x.sin()
}

#[no_mangle]
fn penrose_sinh(x: f64) -> f64 {
    x.sinh()
}

#[no_mangle]
fn penrose_tan(x: f64) -> f64 {
    x.tan()
}

#[no_mangle]
fn penrose_tanh(x: f64) -> f64 {
    x.tanh()
}
