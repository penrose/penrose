#[no_mangle]
fn step(f: fn(*const f64) -> f64, x: f64) -> f64 {
    let mut v = nalgebra::Vector1::new(x);
    let p = v.as_ptr();
    v.neg_mut();
    f(p)
}
