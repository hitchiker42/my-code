extern crate libc;
use std::{time, self};
//use libc;
mod libm {
    extern {
        pub fn modf(val: f64, iptr:* mut f64) -> f64;
    }
}
const nano_scale_float: f64 = 1e9;
const nano_scale: i64 = 1000000000;
pub fn modf(val: f64) -> (f64,f64) {
    let mut ipart = 0.0;
    let mut fpart = 0.0;
    unsafe {
        fpart = libm::modf(val, &mut ipart);
    }
    (ipart, fpart)
}
// pub fn float_to_duration(secs: f64) -> time::Duration {
//     let ipart = 

// pub fn float_time() -> f64 {
// }
// pub fn float_sleep(secs: f64) {
// }
