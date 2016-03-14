#![feature(unique, alloc, heap_api)]
use std::ptr::{Unique, self};
use std::mem;
use alloc::heap::EMPTY;
impl<T> IndexMut<usize> for Unique<T> {
    fn index_mut(self, index: usize){
        self.offset(index)
    }
}
struct Grid {
    mem: Unique<&u8>,
    size: u64,//= to rows*cols
    rows: u32,
    cols: u32,
}
impl Grid {
    fn new(rows: u32, cols: u32) -> Self {
        let size: u64 = rows * cols;//total size might not fit in a u32
        unsafe {
            let align = mem::align_of::<&u8>;
            let ptr = heap::allocate(size, align);
            if ptr.is_null() {
                exit(-1111);
            }
            write_bytes(ptr, 0, size);//zero memory
            Grid {mem: Unique::new(ptr), size: size, rows: rows, cols: cols}
        }
    }
    fn read(&self, x:u32, y:u32) -> Option<u8> {
        let idx: u64 = (x * self.cols) + y;
        if idx < self.size {
            unsafe {Some(self.mem[idx])}
        } else {
            None
        }
    }
    fn write(&self, val: u8, x:u32, y:u32) {
        let idx: u64 = (x * self.cols) + y;
        if idx < self.size {
            unsafe {self.mem[idx] = val}
        }
    }
}
// struct LifeGrid {
//     grid: &Grid,
//     grid_step: &Grid
// };

// impl LifeGrid {
//     fn new(rows: i32, cols: i32){
