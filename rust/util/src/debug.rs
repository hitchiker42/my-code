use std::io::{self, stderr, Write};
/*
  We need to do 'use std::io' in each macro, since apperently rust doesn't 
  use the environment where the macro is defined to get packages.
*/
#[macro_export]
macro_rules! here {
    //I'm not sure how to get the current function, if you even can
    () => {{
        use std::io::{Write, self};
        //#[cfg(debug_assertions)]
        debug_assert!(
            {io::stderr().write_fmt(format_args!("file: {}, line {}\n",
                                                 file!(), line!())).unwrap();true})
    }};
    ($str:expr) => {{
        use std::io::{Write, self};
        debug_assert!(
            {io::stderr().write_fmt(format_args!("file: {}, line {}\n{}",
                                                file!(), line!(), $str)).unwrap();true})
    }};
    ($fmt:expr, $($arg : tt)*) => {{
        use std::io::{Write, self};
        debug_assert!(
            {io::stderr().write_fmt(format_args!("file: {}, line {}\n{}",
                                                file!(), line!(), 
                                                 format!($($arg)*))).unwrap();true})
    }}
}
#[macro_export]
macro_rules! debug_fmt {
    ($fmt : expr, $($args : tt)*) => {{
        use std::io::{Write, self};
        debug_assert!(
            {io::stderr().write_fmt(format_args!($fmt, $($args)*)).unwrap(); true})
    }}
}
