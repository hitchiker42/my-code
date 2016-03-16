#![allow(dead_code, unused_parens)]
#![feature(unique, alloc, heap_api, zero_one)]
extern crate util;//will move to its own crate eventually
mod grid;
pub mod ncurses;
//mod sdl;
pub mod game {
use grid::Grid;
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
    pub fn new(rows: u32, cols: u32) -> Self {
        LifeGame {grid: Grid::new(rows, cols), grid_step: Grid::new(rows, cols),
                  rules: LifeRules::new(2, 3, 3, 3, EdgeRules::edge_wrap)}
    }
    pub fn rows(&self) -> u32 {
        self.grid.rows
    }
    pub fn cols(&self) -> u32 {
        self.grid.cols
    }
    //Currently this just assumes all cells on the edge are dead
    fn count_neighbors(&self, x: u32, y: u32) -> i32 {
        let cols = self.grid.cols;
        let indices = [((y-1)*cols, x-1), ((y-1)*cols, x), ((y-1)*cols, x+1),
                       (y*cols, x-1),     (y*cols, x+1),
                       ((y+1)*cols, x-1), ((y+1)*cols, x), ((y+1)*cols, x+1)];
        return indices.into_iter()
                      .map(|x| self.grid.get(x.0, x.1))
                      .fold(0, |acc, x| acc + x.unwrap_or(0) as i32);
    }
    pub fn step_world(&self) {
        let (mut i,mut j) = (1u32,1u32);
        while(i < (self.grid.rows-1)) {
            while(j < (self.grid.cols-1)) {
                let neighbors = self.count_neighbors(i,j);
                let val = self.grid.read(i,j);
                if(val == 1 &&
                   neighbors < self.rules.life_min || 
                   neighbors > self.rules.life_max){
                    self.grid_step.write(0, i, j);
                } else if(val ==  0 &&
                          neighbors >= self.rules.birth_min &&
                          neighbors <= self.rules.birth_max){
                    self.grid_step.write(1, i, j);
                } else {
                    self.grid_step.write(val, i, j);
                }
                j += 1;
            }
            i += 1;
        }
        self.grid.swap(&self.grid_step);
    }
}
/// Runs the game without displaying the grid
/// prints information useful for debugging
pub fn debug() -> ! {
}
