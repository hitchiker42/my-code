#![allow(dead_code, unused_parens, unused_imports)]
#![crate_type = "lib"]
#![crate_name = "life"]
#[macro_use]
extern crate util;
extern crate rand;
#[macro_use]
pub mod ncurses;
//mod sdl;
pub mod game {
use std;
use rand;
use util::time;
use util::debug::*;
use util::macros::*;
use util::grid::Grid;
#[repr(C)]
pub enum EdgeRules {
    edge_dead,
    edge_alive,
    edge_wrap,
    edge_grow,
}
pub struct LifeRules {
    life_min: i32,
    life_max: i32,
    birth_min: i32,
    birth_max: i32,
    edge_rule: EdgeRules,
}
pub struct LifeGame {
    pub grid: Grid<u8>,
    pub grid_step: Grid<u8>,
    pub rules: LifeRules,
}
impl LifeRules {
    pub fn new(lmin: i32, lmax: i32,
               bmin: i32, bmax: i32, rule: EdgeRules ) -> Self {
        LifeRules {life_min: lmin, life_max: lmax,
                   birth_min: bmin, birth_max: bmax, edge_rule: rule}
    }
}
impl LifeGame {
    pub fn new(rows: usize, cols: usize) -> Self {
        LifeGame {grid: Grid::new(rows, cols), grid_step: Grid::new(rows, cols),
                  rules: LifeRules::new(2, 3, 3, 3, EdgeRules::edge_wrap)}
    }
    pub fn rows(&self) -> usize {
        self.grid.rows()
    }
    pub fn cols(&self) -> usize {
        self.grid.cols()
    }
    pub fn randomize(&mut self) {
        for i in 0..self.rows() {
            for j in 0..self.cols() {
                //gives about a 1 in 10 chance
                if(rand::random::<u8>() <= 50){
                    self.grid[i][j] = 1;
                }
            }
        }
    }
    fn swap_grids(&mut self){
        std::mem::swap(&mut self.grid, &mut self.grid_step)
    }
    //Currently this just assumes all cells on the edge are dead
    fn count_neighbors(&self, x: usize, y: usize) -> usize {
        let cols = self.cols();
        let indices = [((y-1)*cols + x-1), ((y-1)*cols + x), ((y-1)*cols + x+1),
                       (y*cols +  x-1),     (y*cols + x+1),
                       ((y+1)*cols + x-1), ((y+1)*cols + x), ((y+1)*cols + x+1)];
        return indices.into_iter()
                      .filter_map(|x| self.grid.idx_opt(*x))
                      .fold(0, |acc, x| acc + *x as usize);
    }
    pub fn step_world(&mut self) {
        let (mut i,mut j): (usize,usize);
        i = 1;
//        debug_fmt!("Previous world:\n{}\n", self.grid_step);
//        debug_fmt!("Current world:\n{}\n", self.grid);
        while(i < (self.rows()-1)) {
            j = 1;
            while(j < (self.cols()-1)) {
                let neighbors = self.count_neighbors(i,j) as i32;
//                debug_fmt!("neighbors = {}\n", neighbors);
                let val = self.grid[i][j];
                if(val == 1 &&
                   neighbors < self.rules.life_min ||
                   neighbors > self.rules.life_max){
                    self.grid_step[i][j] = 0;
                } else if(val ==  0 &&
                          neighbors >= self.rules.birth_min &&
                          neighbors <= self.rules.birth_max){
//                    here!();
                    self.grid_step[i][j] = 1;
                } else {
                    self.grid_step[i][j] = val;
                }
                j += 1;
            }
            i += 1;
        }
//        debug_fmt!("Next world:\n{}\n", self.grid_step);
        self.swap_grids();
    }
}

/// Runs the game without displaying the grid
/// prints information useful for debugging
pub fn debug() -> ! {
    let mut game = LifeGame::new(20,20);
    game.randomize();
    debug_fmt!("Rows = {}, Cols = {}\n", game.rows(), game.cols());
    loop {
        game.step_world();
        time::float_sleep(0.2);
    }
}
}
