use std::{ops, fmt, self};
use num::{self, Num};
struct Arr2D<T> {
    mem: Vec<T>,
    rows: usize,
    cols: usize,
}
impl<T: Num + Copy> Arr2D<T> {
    pub fn new(rows: usize, cols: usize) -> Self {
        let size = (rows * cols);
        let mut mem = Vec::with_capacity(size);
        mem.resize(size, <T>::zero());
        Arr2D {mem: mem, rows: rows, cols: cols}
    }
    pub fn index(&self, x: usize, y: usize) -> &T {
        self.mem[self.cols * x + y]
    }
    pub fn index_mut(&mut self, x: usize, y: usize) -> &mut T {
        self.mem[self.cols * x + y]
    }
    pub fn get(&self, x: usize, y: usize) -> Option<&T> {
        self.mem.get(self.cols * x + y)
    }
    pub fn get_mut(&mut self, x: usize, y: usize) -> Option<&mut T> {
        self.mem.get_mut(self.cols * x + y)
    }
}
impl<T: Copy> ops::Deref for Arr2D<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        *self.mem;
    }
}
impl<T: Copy> ops::DerefMut for Arr2D<T> {
    fn deref(&mut self) -> &mut [T] {
        *self.mem;
    }
}
impl<T: Copy> ops::Index for Arr2D<T> {
    fn index(&self, idx: usize) -> &T {
        self.mem[idx]
    }
}
impl<T: Copy> ops::Index for Arr2D<T> {
    fn index(&mut self, idx: usize) &mut T {
        self.mem[idx]
    }
}
