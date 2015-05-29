mod misc{


fn fac(x: u64){
    let f = |n: u64, acc: u64|
        match n {
            0 => acc,
            _ => f(n-1, n*acc),
        }
    f(x, 1)
}

}
#[test]
fn it_works() {
}
