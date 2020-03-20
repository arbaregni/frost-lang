use crate::mir::{Mir, VarblId, InstrIndex};
use crate::symbols::SymbolTable;
use crate::util;
use std::collections::HashMap;
use std::cmp;
use petgraph::dot::{Dot, Config};
use petgraph::{Undirected};
use petgraph::graph::NodeIndex;
use petgraph::graphmap::GraphMap;
use petgraph::data::Build;

const MIPS_NUM_FEASIBLE_REGISTERS: usize = 16;
const MIPS_FEASIBLE_REGISTERS: [&'static str; MIPS_NUM_FEASIBLE_REGISTERS]
    = ["$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7",
       "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7"];

type Color = u8;
type Coloring = HashMap<VarblId, Color>;
type ConflictGraph = GraphMap<VarblId, (), Undirected>;

pub struct RegisterAllocation {
    colors: Coloring
}
impl RegisterAllocation {
    pub fn get(&self, varbl: &VarblId) -> &'static str {
        let color = *self.colors.get(varbl)
            .expect(&f!("unassigned memory location for variable {varbl}"));
        MIPS_FEASIBLE_REGISTERS[color as usize]
    }
}

#[derive(Debug)]
/// The Live Range for a variable
struct Interval {
    begin: InstrIndex,
    end: InstrIndex,
}
impl Interval {
    fn new(begin: InstrIndex, end: InstrIndex) -> Interval {
        Interval { begin, end }
    }
    fn expand(&mut self, idx: InstrIndex) {
        if idx < self.begin {
            self.begin = idx;
        }
        if idx > self.end {
            self.end = idx;
        }
    }
    fn intersects(&self, other: &Interval) -> bool {
        let left = cmp::max(self.begin, other.begin);
        let right = cmp::min(self.end, other.end);
        left <= right
    }
}
/// construct the set of intervals across the entire program
fn make_intervals(mir: &Mir) -> HashMap<VarblId, Interval> {
    let mut intervals = HashMap::new();
    for (instr, idx) in mir.iter() {
        instr.visit_variables(&mut |varbl, _| {
            let interval = intervals.entry(*varbl).or_insert(Interval::new(idx, idx));
            interval.expand(idx);
        })
    }
    intervals
}



/// construct the interference graph from the set of intervals
fn make_conflict_graph(intervals: &HashMap<VarblId, Interval>) -> ConflictGraph {
    //TODO make constructing the graph faster
    let nodes = intervals.len();
    let edges = util::estimate_edge_count(nodes);
    let mut graph = ConflictGraph::with_capacity(nodes, edges);
    for (varbl_a, interval_b) in intervals {
        for (varbl_b, interval_b) in intervals {
            if *varbl_a == *varbl_b { continue; }
            graph.add_edge(*varbl_a, *varbl_b, ());
        }
    }
    graph
}



/// removes a node with a degree strictly less than `n` from `conflict_graph`,
/// returning a tuple containing the node and it's neighbors
fn remove_node_with_lesser_degree(conflict_graph: &mut ConflictGraph, n: usize) -> Option<(VarblId, Vec<VarblId>)> {
    // scan for the first node that with degree < n
    for node in conflict_graph.nodes() {
        let degree = conflict_graph.neighbors(node).count();
        if degree >= n { continue; }
        // the degree is less than n, so we remove it and pop it onto the stack
        let neighbors = conflict_graph.neighbors(node).collect::<Vec<_>>();
        conflict_graph.remove_node(node);
        return Some( (node, neighbors) );
    }
    None
}

/// add `node` to the graph, and connect it to everything in `neighbors`, creating them as needed
fn recreate_node(conflict_graph: &mut ConflictGraph, node: VarblId, neighbors: Vec<VarblId>) {
    for neighbor in neighbors.into_iter() {
        conflict_graph.add_edge(node, neighbor, ());
    }
}


fn spill_cost(varbl: VarblId, live_range: Interval, conflict_graph: &ConflictGraph, mir: &Mir) -> usize {
    unimplemented!()
}

/// Assign a color to `node_idx` by assigning it the smallest color possible
fn assign_color(colors: &mut Coloring, conflict_graph: &ConflictGraph, node: VarblId) {
    // find all of our neighbors colors (if they have one)
    let mut neighbor_colors = conflict_graph.neighbors(node)
        .filter_map(|n| colors.get(&n))
        .map(|c| *c)
        .collect::<Vec<_>>();
    neighbor_colors.sort_unstable();

    // pick as our color the smallest color that does not conflict
    let mut color = 0;
    for c in neighbor_colors.into_iter() {
        if color == c {
            color += 1;
        }
    }
    colors.insert(node, color);
}

pub fn allocate_registers(mir: &Mir, symbols: &SymbolTable) -> RegisterAllocation {
    let intervals = make_intervals(mir);
    let mut conflict_graph = make_conflict_graph(&intervals);
    println!("conflict graph: {:?}", Dot::with_config(
        &conflict_graph, &[Config::EdgeNoLabel]
    ));
    let n = MIPS_NUM_FEASIBLE_REGISTERS;
    let mut colorable = Vec::new(); // for the nodes with degree less than n
    while let Some(value) = remove_node_with_lesser_degree(&mut conflict_graph, n) {
        colorable.push(value);
    }
    println!("after trimming: {:?}", Dot::with_config(
        &conflict_graph, &[Config::EdgeNoLabel]
    ));
    // now we figure out if we have to spill any nodes
    // if so, we may have to redo the coloring

    assert_eq!(conflict_graph.node_count(), 0);

    let mut colors = Coloring::new();
    while let Some((node, neighbors)) = colorable.pop() {
        recreate_node(&mut conflict_graph, node, neighbors);
        assign_color(&mut colors, &conflict_graph, node);
    }

    RegisterAllocation { colors }
}
