use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;
use std::{mem, i32, u32, u8};
struct Arc {
    target: u32,
    weight: u32,
}
pub struct Node {
    num: u32,
    index: u32,
    x: i32,
    y: i32,
    arcs: Box<[Arc]>,
    parent: Option<Box<Node>>,
    dist: f64,
}
macro_rules! read_int {
    ($reader:expr, $ty: ident) => ({
        let mut buf: [u8; 4];
        let sz = $reader.read(&mut buf).unwrap();
        assert!(sz == 4);
        unsafe {
            $ty::from_be(mem::transmute::<[u8; 4], $ty>(buf))
        }
    });
}
macro_rules! read_byte {
    ($reader:expr, $ty: ident) => ({
        let mut buf: [u8; 1];
        let sz = $reader.read(&mut buf).unwrap();
        assert!(sz == 1);
        unsafe {
            $ty::from_be(mem::transmute::<[u8; 1], $ty>(buf))
        }
    });
}
pub fn read_roadmap(filename: &str) -> Vec<Node> {
    let mut f = File::open(filename).unwrap();
    let mut buf = BufReader::new(f);

    let ver = read_byte!(buf, u8);
    let num_nodes = read_int!(buf, u32);
    if ver > 1 {
        let _ = read_int!(buf, u32);//number of arcs, we're ignoring this
        //because I can't figure out how to get pointers to offsets
        //in a slice/vec without just using raw pointers
    }
    let nodes:Vec<Node> = Vec::with_capacity(num_nodes as usize);
    for i in 0..num_nodes {
        let node_num = read_int!(buf, u32);
        let (x,y) = (read_int!(buf, i32), read_int!(buf, i32));
        let num_arcs = read_int!(buf, u32);
        let arcs: Vec<Arc> = Vec::with_capacity(num_arcs as usize);
        for _ in 0..num_arcs {
            let target = read_int!(buf, u32);
            let weight = read_int!(buf, u32);
                    arcs.push(Arc {target: target, weight: weight});
        }
        nodes.push(Node {num: node_num,
                         index: i,
                         x: x, y: y,
                         arcs: arcs.into_boxed_slice(),
                         parent: None,
                         dist: std::f64::INFINITY});
    }
    return nodes;
}
impl Ord for Node {
    fn cmp(&self, other: &Node) -> Ordering {
        // Notice that the we flip the ordering here
        other.dist.cmp(&self.dist)
    }
}

// `PartialOrd` needs to be implemented as well.
impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Node) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
pub fn djikstra<T: Write>(out: T, nodes: mut Vec<Nodes>,
                          source: u32, target; u32) -> Option<u32> {
    let start = nodes[source];
    let last = nodes[last];

    let heap = BinaryHeap::new();
