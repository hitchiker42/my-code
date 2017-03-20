#![crate_type = "lib"]
#![crate_name = "util"]
#![allow(dead_code, unused_parens, unused_imports, non_camel_case_types)]
extern crate libc;
extern crate num;
//use std;
//use libc;
pub mod time;
pub mod c;
pub mod rand;
pub mod debug;
pub mod macros;
pub mod mem;
pub mod hashtable;
///A simple 2 dimensional numeric array
pub mod grid;
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
