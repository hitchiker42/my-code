extern crate ncurses as ncurses_lib;
use self::ncurses_lib::*;
use game::*;
use util::time;
use util::c::libc;
use std::mem;
//mod game;
pub fn init() {
    initscr();//start curses mode
    cbreak();//disable line buffering, pass ^C,^Z,etc to terminal
    //raw();//disable line buffering, don't pass ctrl chars to terminal
    noecho();//don't echo keyboard input;
}
pub fn draw(game: &LifeGame, win: WINDOW){
    for i in 0..game.rows() {
        for j in 0..game.cols() {
            wmove(win, i as i32, j as i32);
            if(i == 0 || j == 0 || 
               i == game.rows()-1 || j == game.cols()-1){
                waddch(win, '*' as chtype);
            }
            waddch(win, if game.grid[i][j] == 1 {'#'} else {' '} as chtype);
        }
    }
    wrefresh(win);
}
pub fn run_life(game: &mut LifeGame, win_opt: Option<WINDOW>) -> ! {
    let win = win_opt.unwrap_or(stdscr);
    nodelay(win, true);
    curs_set(CURSOR_VISIBILITY::CURSOR_INVISIBLE);
    loop {
        time::float_sleep(0.2);
        draw(game, win);
        game.step_world();
    }
}
pub fn main() -> ! {
    init();
    let (mut term_rows, mut term_cols) = (24i32,80i32);
    getmaxyx(stdscr, &mut term_rows, &mut term_cols);
    let mut game = LifeGame::new(term_rows as usize, term_cols as usize);
    game.randomize();
//    unsafe { libc::atexit(mem::transmute(ncurses_lib::ll::endwin)) };
    run_life(&mut game, None);
}
    
