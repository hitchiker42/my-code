#[no_mangle]
pub fn fact(n: u64) -> u64 {
    if n <= 1 {
        1
    } else {
        n*fact(n-1)
    }
}
#[no_mangle]
pub fn fact_tail(n: u64) -> u64 {
    fn f(n: u64, acc:u64) -> u64 { if n<=1 {acc} else {f(n-1,n*acc)}}
    f(n, 1)
}
