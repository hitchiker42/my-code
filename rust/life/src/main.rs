#![feature(pattern)]
extern crate sdl2;
extern crate ncurses;
extern crate life;
//use std::{self};
use std::str::pattern::Pattern;
fn main() {
    let mut argv = std::env::args();
    let _ = argv.next();//ignore program name
    let mut sdl = false;
    let mut ncurses = false;
    let mut sdl_opengl = false;
    loop {
        match argv.next() {
            Some(ref x) if x.is_prefix_of("--sdl") => (sdl = true),
            Some(ref x) if x.is_prefix_of("--ncurses") => (ncurses = true),
            Some(ref x) if x.is_prefix_of("--opengl") ||
                       x.is_prefix_of("--sdl-opengl") => (sdl_opengl = true),
            Some(ref x) => println!("Invalid option {}", x),
            None => break,
        }
    }
            
    println!("Hello, world!");
}
