use std::io::{self,Write};
fn main(){
    let mut input = String::new();
    try!(io::stdin().read_line(&mut input));
    try!(io::stdout().write(b"Hello, World.\n"));
    try!(io::stdout().write(input.as_bytes()));
}
// fn main(){
//     println!("Hello, World!");
// }
