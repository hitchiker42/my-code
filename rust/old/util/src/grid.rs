use std::{ops, fmt, self};
use num::{self, Num};
use std::fmt::{Display, Error, Write};
pub struct Grid<T> {
    mem: Box<[T]>,
    rows: usize,
    cols: usize,
}
impl<T: Num + Copy> Grid<T> {
    pub fn new(rows: usize, cols: usize) -> Self {
        let size = (rows * cols);
        let mem = vec!(<T>::zero(); size);
        Grid {mem: mem.into_boxed_slice(), rows: rows, cols: cols}
    }
    //index the underlying memory directly
    pub fn idx(&self, idx: usize) -> T {
        self.mem[idx]
    }
    pub fn idx_opt(&self, idx: usize) -> Option<&T> {
        self.mem.get(idx)
    }
    pub fn get(&self, x: usize, y: usize) -> Option<&T> {
        self.mem.get(self.cols * x + y)
    }
    pub fn get_mut(&mut self, x: usize, y: usize) -> Option<&mut T> {
        self.mem.get_mut(self.cols * x + y)
    }
    pub fn set(&mut self, val: T, x: usize, y: usize) {
        self.mem[self.cols * x + y] = val;
    }
    pub fn rows(&self) -> usize {
        self.rows
    }
    pub fn cols(&self) -> usize {
        self.cols
    }
    pub fn size(&self) -> usize {
        self.rows * self.cols
    }
}
impl<T: Copy> ops::Deref for Grid<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        &*self.mem
    }
}
impl<T: Copy> ops::DerefMut for Grid<T> {
    fn deref_mut(&mut self) -> &mut [T] {
        &mut *self.mem
    }
}
//Indexing is defined in such a way that arr[row][col] works as expected
impl<T: Copy> ops::Index<usize> for Grid<T> {
    type Output = [T];
    fn index(&self, idx: usize) -> &[T] {
        assert!(idx < self.rows);
        let ptr: *const T = (*self.mem).as_ptr();
        unsafe {std::slice::from_raw_parts(ptr.offset((idx*self.cols) as isize),
                                           self.cols)}
    }
}
impl<T: Copy> ops::IndexMut<usize> for Grid<T> {
    fn index_mut(&mut self, idx: usize) -> &mut [T] {
                assert!(idx < self.rows);
        let ptr: *mut T = (*self.mem).as_mut_ptr();
        unsafe {std::slice::from_raw_parts_mut(ptr.offset((idx*self.cols) as isize),
                                               self.cols)}
    }
}
impl<T: Num + Copy + Display> fmt::Display for Grid<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), Error>{
        for i in 0..self.rows {
            for j in 0..self.cols {
                try!(f.write_str(&self[i][j].to_string()));
                if j < (self.cols -1){
                    try!(f.write_char(' '));
                }
            }
            if i < (self.cols-1) {
                try!(f.write_char('\n'));
            }
        }
        Ok(())
    }
}
