#[no_mangle]
fn step(f: i32, x: f64) -> f64 {
    let op = unsafe { std::mem::transmute::<i32, fn(i32) -> f64>(f) };
    let mut v = nalgebra::Vector1::new(x);
    let p = unsafe { std::mem::transmute::<*const f64, i32>(v.as_ptr()) };
    v.neg_mut();
    op(p)
}
