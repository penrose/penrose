// adapted from https://github.com/rustwasm/wasm-bindgen/blob/0.2.83/src/lib.rs#L1509-L1564

// Copyright (c) 2014 Alex Crichton
//
// Permission is hereby granted, free of charge, to any
// person obtaining a copy of this software and associated
// documentation files (the "Software"), to deal in the
// Software without restriction, including without
// limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software
// is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice
// shall be included in all copies or substantial portions
// of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
// ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
// TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
// SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

use std::alloc::{alloc, dealloc, Layout};

#[no_mangle]
fn __wbindgen_malloc(size: usize, align: usize) -> *mut u8 {
    if let Ok(layout) = Layout::from_size_align(size, align) {
        unsafe {
            if layout.size() > 0 {
                let ptr = alloc(layout);
                if !ptr.is_null() {
                    return ptr;
                }
            } else {
                return align as *mut u8;
            }
        }
    }

    malloc_failure();
}

#[cold]
fn malloc_failure() -> ! {
    std::process::abort();
}

#[no_mangle]
unsafe fn __wbindgen_free(ptr: *mut u8, size: usize, align: usize) {
    // This happens for zero-length slices, and in that case `ptr` is
    // likely bogus so don't actually send this to the system allocator
    if size == 0 {
        return;
    }
    let layout = Layout::from_size_align_unchecked(size, align);
    dealloc(ptr, layout);
}
