use std::time::Duration;
use std::{mem, thread, self};
use c;
use libc::{timespec, clock_gettime, CLOCK_REALTIME};
const NANO_SCALE_FLOAT: f64 = 1e9;
const NANO_SCALE: u64 = 1000000000;
pub fn get_current_time() -> timespec {
    let mut tp: timespec;
    unsafe {
        tp = mem::uninitialized();
        //The only reasonable error would be if the system didn't support
        //CLOCK_REALTIME, which any posix compliant system needs to, though
        //I'm pretty sure OSX doesn't, but we're ignoring that for now;
        let err = clock_gettime(CLOCK_REALTIME, &mut tp);
    }
    tp
}
pub fn get_time_duration() -> Duration {
    let tp = get_current_time();
    Duration::new(tp.tv_sec as u64, tp.tv_nsec as u32)
}
pub fn float_time() -> f64 {
    let tp = get_current_time();
    (tp.tv_sec as f64 + (tp.tv_nsec as f64 / NANO_SCALE_FLOAT))
}
pub fn float_to_duration(secs: f64) -> Duration {
    let (ipart, fpart) = c::modf(secs);
    Duration::new(ipart as u64, (fpart * NANO_SCALE_FLOAT) as u32)
}
pub fn nsec_to_duration(nsecs: u64) -> Duration {
    Duration::new(nsecs / NANO_SCALE,
                 (nsecs % NANO_SCALE) as u32)
}
pub fn duration_to_float(dur: Duration) -> f64 {
    (dur.as_secs() as f64 + (dur.subsec_nanos() as f64 / NANO_SCALE_FLOAT))
}
pub fn float_sleep(secs: f64) {
    thread::sleep(float_to_duration(secs))
}
pub fn nanosleep(nsecs: u64){
    thread::sleep(nsec_to_duration(nsecs))
}
