//use libc::*;
//
use std::mem;
mod sem {
    pub struct Semaphore;
    //Return true on success, false if limit reached
    pub fn sem_post(sem: &mut sem) -> bool;
    //return true on success, false if interupted by a signal
    pub fn sem_wait(sem: &mut sem) -> bool;
    //return None if sem is locked, same as sem_wait otherwise
    pub fn sem_try_wait(sem: &mut sem) -> Option<bool>;
    //same as above, except None means a timeout 
    pub fn sem_timed_wait(sem: &mut sem, time: f64) -> Option<bool>;
}
    
mod libm {
    extern {
        pub fn modf(val: f64, iptr:* mut f64) -> f64;
    }
}
pub mod libc {
    pub use libc::*;
}
/// Seperates a floating point number into integer and fractional components
///
/// Note that this calls the C function of the same name, this could also be
/// implemented in pure rust as |x: f64| -> (trunc(x), fract(x))
///
/// #Examples
/// ```
/// let foo = 1.12345678;
/// let (bar, baz) = modf(foo);
/// assert!(bar == 1.0 && baz == 0.12345678)
/// ```
pub fn modf(val: f64) -> (f64,f64) {
    let mut ipart: f64;
    let fpart: f64;
    unsafe {
        ipart = mem::uninitialized();
        fpart = libm::modf(val, &mut ipart);
    }
    (ipart, fpart)
}
//osx doesn't implement posix semaphores
#[cfg(all(any(unix), not(target_os = "macos")))]
mod sem {
    use libc::{c_int, c_uint, timespec};
    pub enum sem_t {}
    extern {
        pub fn sem_init(sem: *mut sem_t, shared: c_int, count: c_uint) -> c_int;
        pub fn sem_wait(sem: *mut sem_t) -> c_int;
        pub fn sem_trywait(sem: *mut sem_t) -> c_int;
        pub fn sem_timedwait(sem: *mut sem_t, timeout: *const timespec) -> c_int;
        pub fn sem_post(sem: *mut sem_t) -> c_int;
    }
struct Semaphore {
    sem: sem::sem_t,
}
impl Semaphore {
}
}
//this is just taken from the depreciated standard library module
#[cfg(any(not(unix)), target_os = "macos")]
mod sem {
struct Semaphore {
    lock: Mutex<isize>,
    cvar: Condvar,
};
    impl Semaphore {
    }
};
