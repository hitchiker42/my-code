#[crate_type = "lib"]
#[crate_name = "util"]
#![allow(dead_code, unused_parens, non_camel_case_types)]
extern crate libc;
//use std;
//use libc;
pub mod time;
pub mod c;
pub mod rand;
pub mod debug;
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
