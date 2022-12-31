use std::{
    cell::RefCell,
    env,
    fs::{read_to_string, File},
    io::Read,
    path::Path,
};

use byte_slice_cast::{AsByteSlice, AsMutByteSlice};
use penrose_optimizer::{inverse, sign, step, Bool, Compiled, OptState, OptStatus};
use wasmer::{imports, Function, Instance, Memory, MemoryType, Module, Store, Value};

struct FuncRef<'a> {
    store: RefCell<Store>,
    memory: Memory,
    f: &'a Function,
}

impl<'a> Compiled for FuncRef<'a> {
    fn call(
        &self,
        inputs: &[f64],
        mask: &[Bool],
        gradient: &mut [f64],
        secondary: &mut [f64],
    ) -> f64 {
        let store: &mut Store = &mut self.store.borrow_mut();
        let view = self.memory.view(store);

        let mut offset = 0;
        let mut alloc = |data| {
            let p = offset;
            view.write(p, data).unwrap();
            offset += u64::try_from(data.len()).unwrap();
            return p;
        };

        let offset_inputs = alloc(inputs.as_byte_slice());
        let offset_mask = alloc(mask.as_byte_slice());
        let offset_gradient = alloc(gradient.as_byte_slice());
        let offset_secondary = offset;

        let primary = self
            .f
            .call(
                store,
                &[
                    Value::I32(i32::try_from(offset_inputs).unwrap()),
                    Value::I32(i32::try_from(offset_mask).unwrap()),
                    Value::I32(i32::try_from(offset_gradient).unwrap()),
                    Value::I32(i32::try_from(offset_secondary).unwrap()),
                ],
            )
            .unwrap();

        view.read(offset_gradient, gradient.as_mut_byte_slice())
            .unwrap();
        view.read(offset_secondary, secondary.as_mut_byte_slice())
            .unwrap();

        primary[0].unwrap_f64()
    }
}

fn my_add_to_stack_pointer(p: i32) -> i32 {
    unimplemented!("__wbindgen_add_to_stack_pointer {p}")
}

fn my_poly_roots(p: i32, l: i32) {
    unimplemented!("penrose_poly_roots {p} {l}")
}

fn main() {
    let args: Vec<_> = env::args().collect();

    let dirname = Path::new(&args[1]);

    let mut bytes = vec![];
    File::open(dirname.join("gradient.wasm"))
        .unwrap()
        .read_to_end(&mut bytes)
        .unwrap();

    let mut store = Store::default();
    let module = Module::new(&store, bytes).unwrap();

    let memory = Memory::new(&mut store, MemoryType::new(1, None, false)).unwrap();
    let imp = imports! {
        "" => {
            "" => memory.clone(),

            "0" => Function::new_typed(&mut store, my_add_to_stack_pointer),

            "1" => Function::new_typed(&mut store, |x: f64| x.acos()),
            "2" => Function::new_typed(&mut store, |x: f64| x.acosh()),
            "3" => Function::new_typed(&mut store, |x: f64| x.asin()),
            "4" => Function::new_typed(&mut store, |x: f64| x.asinh()),
            "5" => Function::new_typed(&mut store, |x: f64| x.atan()),
            "6" => Function::new_typed(&mut store, |x: f64| x.atanh()),
            "7" => Function::new_typed(&mut store, |x: f64| x.cbrt()),
            "8" => Function::new_typed(&mut store, |x: f64| x.cos()),
            "9" => Function::new_typed(&mut store, |x: f64| x.cosh()),
            "a" => Function::new_typed(&mut store, |x: f64| x.exp()),
            "b" => Function::new_typed(&mut store, |x: f64| x.exp_m1()),
            "c" => Function::new_typed(&mut store, |x: f64| x.ln()),
            "d" => Function::new_typed(&mut store, |x: f64| x.ln_1p()),
            "e" => Function::new_typed(&mut store, |x: f64| x.log10()),
            "f" => Function::new_typed(&mut store, |x: f64| x.log2()),
            "g" => Function::new_typed(&mut store, |x: f64| x.sin()),
            "h" => Function::new_typed(&mut store, |x: f64| x.sinh()),
            "i" => Function::new_typed(&mut store, |x: f64| x.tan()),
            "j" => Function::new_typed(&mut store, |x: f64| x.tanh()),
            "k" => Function::new_typed(&mut store, |x: f64| inverse(x)),
            "l" => Function::new_typed(&mut store, |x: f64| sign(x)),

            "m" => Function::new_typed(&mut store, |y: f64, x: f64| y.atan2(x)),
            "n" => Function::new_typed(&mut store, |x: f64, y: f64| x.powf(y)),

            "o" => Function::new_typed(&mut store, my_poly_roots),
        },
    };
    let instance = Instance::new(&mut store, &module, &imp).unwrap();
    let f = instance.exports.get_function("").unwrap();

    let json = read_to_string(dirname.join("initial.json")).unwrap();
    let mut state: OptState = serde_json::from_str(&json).unwrap();
    let func = FuncRef {
        store: RefCell::new(store),
        memory,
        f,
    };
    while state.params.opt_status != OptStatus::EPConverged {
        state = step(state, &func, 1);
    }

    if let Some(p) = args.get(2) {
        let file = File::create(p).unwrap();
        serde_json::to_writer_pretty(file, &state).unwrap();
    }
}
