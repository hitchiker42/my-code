extern crate alloc;
extern crate rand;
use std::ptr::{Unique, self};
use std::{mem, ops, num, fmt, self};
use std::fmt::{Display, Error, Write};
use self::alloc::heap;

pub struct Grid<T> {
    pub mem: Unique<T>,
    pub size: usize,//= to rows*cols
    pub rows: u32,
    pub cols: u32,
}
macro_rules! size_of {
    ($t:ty) => (mem::size_of::<$t>())
}
macro_rules! align_of {
    ($t:ty) => (mem::align_of::<$t>())
}
impl<T: Copy> ops::Deref for Grid<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        unsafe {::std::slice::from_raw_parts(*self.mem, self.size)}
    }
}
impl<T: Copy> ops::DerefMut for Grid<T> {
    fn deref_mut(&mut self) -> &mut [T] {
        unsafe {::std::slice::from_raw_parts_mut(*self.mem, self.size)}
    }
}
impl<T: Copy> ops::Index<usize> for Grid<T> {
    type Output = T;
    fn index<'a>(&'a self, idx: usize) -> &'a T {
        let tmp: &[T] = self;
        &tmp[idx]
    }
}
impl<T: Copy> ops::IndexMut<usize> for Grid<T> {
    fn index_mut<'a>(&'a mut self, idx: usize) -> &'a mut T {
        let tmp: &'a mut [T] = self;
        &mut tmp[idx]
    }
}
impl<T: Copy> Grid<T> {
    pub fn new(rows: u32, cols: u32) -> Self {
        let size = rows as usize * cols as usize;//total size might not fit in a u32
        unsafe {
            //align to the size of a pointer
            let ptr = heap::allocate(size * size_of!(T), align_of!(T));
            if ptr.is_null() {
                std::process::exit(-1111);
            }
            ptr::write_bytes(ptr, 0, size*size_of!(T));//zero memory
            Grid {mem: Unique::new(ptr as *mut T),
                  size: size, rows: rows, cols: cols}
        }
    }
    pub fn from_slice(slice: &[T], rows: u32, cols: u32) -> Self {
        //will probably unnecessarily zero the grid
        let grid = Grid::new(rows, cols);
        let tmp = slice.as_ptr();
        unsafe {
            ptr::copy_nonoverlapping(tmp, *grid.mem as *mut T, grid.size);
        }
        grid
    }
    /// Sets all elements of the grid to 0
    pub fn zero(&self) {
        unsafe {ptr::write_bytes(*self.mem as *mut T, 0, self.size * size_of!(T))}
    }
    /// Copies the values in src to self,
    /// panics if the grids don't have the same dimensions
    pub fn copy(&self, src: &Grid<T>) {
        assert!(self.size == src.size && 
                self.rows == src.rows && self.cols == src.cols);
        unsafe {ptr::copy_nonoverlapping(*src.mem, *self.mem, self.size)}
    }
    /// Swaps the values of src and self,
    /// panics if the grids don't have the same dimensions.
    /// This only swaps a pointer, not actual memory.
    pub fn swap(&self, src: &Grid<T>) {
        assert!(self.size == src.size && 
                self.rows == src.rows && self.cols == src.cols);
        unsafe { ptr::swap(*self.mem, *src.mem) }
    }
    /// Returns the element at row x, column y. Panics if this is out of bounds
    pub fn read(&self, x:u32, y:u32) -> T {
        let idx = (x * self.cols) as isize + y as isize;
        assert!(idx < self.size as isize);
        unsafe {*self.mem.offset(idx)}
    }
    /// Returns the element at row x, column y, or None if out of bounds
    pub fn get(&self, x:u32, y:u32) -> Option<T> {
        let idx = (x * self.cols) as isize + y as isize;
        if idx < self.size as isize {
            unsafe {Some(*self.mem.offset(idx))}
        } else {
            None
        }
    }
    pub fn write(&self, val: T, x:u32, y:u32) {
        let idx = (x * self.cols) as isize + y as isize;
        if idx < self.size as isize {
            unsafe {*self.mem.offset(idx) = val}
        }
    }
    pub fn set(&self, val: T, x:u32, y:u32) {
        self.write(val, x, y);
    }
    pub fn get_coords(&self, idx: isize) -> (u32,u32) {
        ((idx / self.cols as isize) as u32,
         (idx % self.cols as isize) as u32)
    }
    pub fn get_index(&self, x:u32, y:u32) -> isize {
        (self.cols*y) as isize + x as isize
    }
    pub fn index_unsafe(&self, x:usize) -> T {
        assert!(x < self.size);
        unsafe {*self.mem.offset(x as isize)}
    }
    pub fn index_safe(&self, x:usize) -> Option<T> {
        if x < self.size {
            unsafe {Some(*self.mem.offset(x as isize))}
        } else {
            None
        }
    }       
}
#[macro_export]
macro_rules! index {
    ($grid : expr, $idx : expr) => {{
        assert!(($idx as usize) < $grid.size);
        unsafe {*$grid.mem.offset($idx as usize)}
    }};
    ($grid : expr, $x : expr, $y : expr) => {{
        assert!(($y as usize * $grid.cols as usize) + $x as usize < $grid.size);
        unsafe {*$grid.mem.offset(($y as isize * $grid.cols as isize) + 
                                  $x as isize)}
    }}
}
#[macro_export]
macro_rules! index_opt {
    ($grid : expr, $idx : expr) => {{
        if(($idx as usize) < $grid.size){
            None
        } else {
            unsafe {Some(*$grid.mem.offset($idx as isize))}
        }
    }};
    ($grid : expr, $x : expr, $y : expr) => {{
        let grid = $grid;
        if((($y as u64) * ($grid.cols as u64)) + ($x as u64) < ($grid.size as u64)){
            None
        } else {
        unsafe {Some(*grid.mem.offset((($y as isize) * ($grid.cols as isize)) + 
                                      ($x as isize)))}
        }
    }}
}
    
impl<T: num::One + Copy> Grid<T> {
    /// Randomly sets each element of the grid to 0 or 1
    ///
    /// Most elements are set to 0, there is about a 1/10 chance for an
    /// element to be set to 1.
    pub fn randomize(&self) {
        self.zero();
        for i in 0..self.rows {
            for j in 0..self.cols {
                //gives about a 1 in 10 chance
                if(rand::random::<u8>() <= 50){
                    self.write(T::one(), i, j);
                }
            }
        }
    }
}
impl<T: Copy + Display> fmt::Display for Grid<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), Error>{
        for i in 0..self.rows {
            for j in 0..self.cols {
                try!(f.write_str(&self.read(i,j).to_string()));
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
