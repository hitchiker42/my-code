macro_rules! narg_expr {
    () => {0};
    ($e: expr $(, $rest: expr)*) => { 1 + narg!($($rest),*) }
}
// macro_rules! gen_accessor {
//     ($slot:ident: $ty:ty, get) => {
//         pub fn get_$slot(&self) -> $ty {
//             self.$slot
//         }
//     }
//     ($slot:ident: $ty:ty, set) => {
//         pub fn set_$slot(&mut self, val:$ty) {
//             self.$slot = val;
//         }
//     }
// }
//#[macro_export]
//Define a struct and create accessor methods
//macro_rules! gen_accessors {
    
    
