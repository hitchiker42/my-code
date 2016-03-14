extern crate alloc;
extern crate rand;
use std::ptr::{Unique, self};
use std::{mem, ops, num, self};
use self::alloc::heap;
pub struct Grid<T> {
    mem: Unique<T>,
    size: usize,//= to rows*cols
    pub rows: u32,
    pub cols: u32,
}
macro_rules! size_of {
    ($t:ty) => (mem::size_of::<$t>())
}
macro_rules! align_of {
    ($t:ty) => (mem::align_of::<$t>())
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
    pub fn zero(&self) {
        unsafe {ptr::write_bytes(*self.mem as *mut T, 0, self.size * size_of!(T))}
    }    
                
    pub fn read(&self, x:u32, y:u32) -> T {
        let idx = (x * self.cols) as isize + y as isize;
        assert!(idx < self.size as isize);
        unsafe {*self.mem.offset(idx)}
    }
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
}
impl<T: num::One + Copy> Grid<T> {
    pub fn randomize(&self) {
        self.zero();
        for i in 0..self.rows {
            for j in 0..self.cols {
                //gives about a 1 in 10 chance
                if(rand::random::<u8>() <= 25){
                    self.write(T::one(), i, j);
                }
            }
        }
    }
}
impl<T> ops::Deref for Grid<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        unsafe {::std::slice::from_raw_parts(*self.mem, self.size)}
    }
}
impl<T> ops::DerefMut for Grid<T> {
    fn deref_mut(&mut self) -> &mut [T] {
        unsafe {::std::slice::from_raw_parts_mut(*self.mem, self.size)}
    }
}
impl<T> ops::Index<usize> for Grid<T> {
    type Output = T;
    fn index<'a>(&'a self, idx: usize) -> &'a T {
        let tmp: &[T] = self;
        &tmp[idx]
    }
}
impl<T> ops::IndexMut<usize> for Grid<T> {
    fn index_mut<'a>(&'a mut self, idx: usize) -> &'a mut T {
        let tmp: &'a mut [T] = self;
        &mut tmp[idx]
    }
}
