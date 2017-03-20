use std;
#[macro_export]
macro_rules! size_of {
    ($t:ty) => (mem::size_of::<$t>())
}
#[macro_export]
macro_rules! align_of {
    ($t:ty) => (mem::align_of::<$t>())
}
pub fn allocate<T>(count: usize) -> *mut T {
    let mut v = Vec::with_capacity(count);
    let ptr = v.as_mut_ptr();
    std::mem::forget(v);
    ptr
}

pub unsafe fn deallocate<T>(ptr: *mut T, count: usize) {
    std::mem::drop(Vec::from_raw_parts(ptr, 0, count));
}
