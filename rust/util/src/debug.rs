use std::io::{self, Write};
#[macro_export]
macro_rules! here {
    //I'm not sure how to get the current function, if you even can
    () => {
        #[cfg(debug_assertions)]
        write!(io::stderr(), "file: {}, line {}\n", file!(), line!())
    }
    ($str:expr) => {
        #[cfg(debug_assertions)]
        write!(io::stderr(), "file: {}, line {}\n{}", file!(), line!(), $str)
    }
    ($fmt:expr, $($arg : tt)*) => {
        #[cfg(debug_assertions)]
        write!(io::stderr(), "file: {}, line {}\n{}", file!(), line!(), 
               format!($($arg)*))
    }
}
