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
    pub grid: Vec<u8>,
    pub grid_step: Vec<u8>,
    pub rules: LifeRules,
    pub rows: u32,
    pub cols: u32,
    pub size: usize,//== rows * cols
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
        let size = rows as usize * cols as usize;
        LifeGame {grid: Vec::with_capacity(size), 
                  grid_step: Vec::with_capacity(size),
                  rows: rows, cols: cols, size: size,
                  rules: LifeRules::new(2, 3, 3, 3, EdgeRules::edge_wrap)}
    }
