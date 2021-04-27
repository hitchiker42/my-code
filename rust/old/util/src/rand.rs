use time;
pub struct rand_state {
    state: [u64; 2],
}
static mut internal_rand_state: rand_state = rand_state {state: [0,0]};
fn rand(state_opt: Option<&mut rand_state>) -> u64 {
    let ref mut state = match state_opt {
        Some(x) => x,
        None => unsafe {&mut internal_rand_state}
    };
    let mut x = state.state[0];
    let y = state.state[1];
    state.state[0] = y;
    x ^= x << 23;
    state.state[1] = (x ^ y) ^ (x >> 17) ^ (y >> 26);
    state.state[1] + y
}
macro_rules! rand {
    ($x:expr) => (rand(Some($x)));
        () => (rand(None))
}   
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
