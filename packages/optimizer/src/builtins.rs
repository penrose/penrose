use wasm_bindgen::prelude::wasm_bindgen;

// WebAssembly doesn't have a lot of math functions built in, so we export all these to allow our
// JIT-compiled module to put them into its `table` and `call_indirect` them. We need to be very
// careful about the names of all the functions we export, though, because the Rust-to-Wasm compiler
// emits functions with the same names as these `f64` methods we're calling. For instance, if we
// name our exported function the same as the method it's calling, the emitted Wasm code will simply
// try to call itself recursively. To avoid this issue, we prefix each name with `penrose_`.

#[wasm_bindgen]
pub fn penrose_acos(x: f64) -> f64 {
    x.acos()
}

#[wasm_bindgen]
pub fn penrose_acosh(x: f64) -> f64 {
    x.acosh()
}

#[wasm_bindgen]
pub fn penrose_asin(x: f64) -> f64 {
    x.asin()
}

#[wasm_bindgen]
pub fn penrose_asinh(x: f64) -> f64 {
    x.asinh()
}

#[wasm_bindgen]
pub fn penrose_atan(x: f64) -> f64 {
    x.atan()
}

#[wasm_bindgen]
pub fn penrose_atanh(x: f64) -> f64 {
    x.atanh()
}

#[wasm_bindgen]
pub fn penrose_atan2(y: f64, x: f64) -> f64 {
    y.atan2(x)
}

#[wasm_bindgen]
pub fn penrose_cbrt(x: f64) -> f64 {
    x.cbrt()
}

#[wasm_bindgen]
pub fn penrose_cos(x: f64) -> f64 {
    x.cos()
}

#[wasm_bindgen]
pub fn penrose_cosh(x: f64) -> f64 {
    x.cosh()
}

#[wasm_bindgen]
pub fn penrose_exp(x: f64) -> f64 {
    x.exp()
}

#[wasm_bindgen]
pub fn penrose_expm1(x: f64) -> f64 {
    x.exp_m1()
}

#[wasm_bindgen]
pub fn penrose_log(x: f64) -> f64 {
    x.ln()
}

#[wasm_bindgen]
pub fn penrose_log1p(x: f64) -> f64 {
    x.ln_1p()
}

#[wasm_bindgen]
pub fn penrose_log10(x: f64) -> f64 {
    x.log10()
}

#[wasm_bindgen]
pub fn penrose_log2(x: f64) -> f64 {
    x.log2()
}

#[wasm_bindgen]
pub fn penrose_pow(x: f64, y: f64) -> f64 {
    x.powf(y)
}

#[wasm_bindgen]
pub fn penrose_sin(x: f64) -> f64 {
    x.sin()
}

#[wasm_bindgen]
pub fn penrose_sinh(x: f64) -> f64 {
    x.sinh()
}

#[wasm_bindgen]
pub fn penrose_tan(x: f64) -> f64 {
    x.tan()
}

#[wasm_bindgen]
pub fn penrose_tanh(x: f64) -> f64 {
    x.tanh()
}

// we make this a function rather than inlining the definition when we generate code, because it
// includes a floating-point literal and those cost 8 bytes each to write into a WebAssembly binary
#[wasm_bindgen]
pub fn penrose_inverse(x: f64) -> f64 {
    1. / x
}

// before we switched to WebAssembly and Rust, we used JavaScript's `Math.sign` function, so this
// function is meant to provide the same behavior as that
#[wasm_bindgen]
pub fn penrose_sign(x: f64) -> f64 {
    if x == 0. {
        x
    } else {
        f64::copysign(1., x)
    }
}

// force `wasm-bindgen` to export `__wbindgen_add_to_stack_pointer`, which we will use to call the
// polynomial roots function below
#[wasm_bindgen]
pub fn penrose_empty_vec() -> Vec<usize> {
    Vec::new()
}

// just using `#[wasm_bindgen]` here doesn't work, because version 0.2.84 adds an extra parameter to
// take a reference to a JavaScript heap object, which isn't what we want here because this is meant
// to be directly consumed by the Wasm we generate from autodiff graphs
#[no_mangle]
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
