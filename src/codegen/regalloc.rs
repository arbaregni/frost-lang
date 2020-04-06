use crate::mir::{Mir, VarblId, InstrIndex};
use crate::symbols::SymbolTable;
use crate::util;
use std::collections::HashMap;
use std::cmp;
use petgraph::dot::{Dot, Config};
use petgraph::{Undirected};
use petgraph::graphmap::GraphMap;
use std::fmt::{Formatter, Error};

// there are 24 registers that we care about allocating
const MIPS_NUM_REGISTERS: usize = 24;
// the first 16 are feasible
const MIPS_NUM_FEASIBLE_REGISTERS: usize = 16;
const MIPS_REGISTERS: [&'static str; MIPS_NUM_REGISTERS]
    = [
       "$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7",  // <━━┓ free to color however we like
       "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7",  // <━━┛

       "$a0", "$a1", "$a2", "$a3", "$v0", "$v1", "$ra", "$sp"]; // <--------------- the last 8 have special semantics
//                                                  ^
//                                                  |
//                                                index 22

pub type Color = u8;
pub type Coloring = HashMap<VarblId, Color>;
type ConflictGraph = GraphMap<VarblId, (), Undirected>;

// the index into MIPS_REGISTERS keeps register "$ra" at index 22
pub const RETURN_ADDR_COLOR: Color = 22;

pub struct RegisterAllocation {
    colors: Coloring
}
impl RegisterAllocation {
    pub fn get(&self, varbl: &VarblId) -> &'static str {
        let color = *self.colors.get(varbl)
            .expect(&f!("unassigned memory location for variable {varbl}"));
        MIPS_REGISTERS[color as usize]
    }
}
impl std::fmt::Display for RegisterAllocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        for (varbl, color) in self.colors.iter() {
            writeln!(f, "{} <-> {}", varbl, MIPS_REGISTERS[*color as usize])?;
        }
        Ok(())
    }
}
/// The Live Range for a variable
/// Note: the current representation
///       relies on an ordering of
///       instructions which is already
///       established.
#[derive(Debug)]
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
    for (&varbl_a, interval_a) in intervals {
        graph.add_node(varbl_a);
        for (&varbl_b, interval_b) in intervals {
            if varbl_a != varbl_b && interval_a.intersects(interval_b) {
                graph.add_edge(varbl_a, varbl_b, ());
            }
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

/// The heuristic for the cost it would take to spill the given variable
/// Uses the cost of spilling / degree of node
fn spill_cost(varbl: VarblId, _live_range: &Interval, conflict_graph: &ConflictGraph, _mir: &Mir) -> f32 {
    let degree = conflict_graph.neighbors(varbl).count() as f32;
    // TODO: use nesting depth heuristic
    10000. / degree
}

/// Assign a color to `node_idx` by assigning it the smallest color possible
fn assign_color(colors: &mut Coloring, conflict_graph: &ConflictGraph, node: VarblId) {
    if colors.contains_key(&node) {
        // we have already colored this node.
        // no reason to give it a new color
        return
    }
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

/// allocate registers for based on our internal representation
/// the algorithm is basically Chaitin's algorithm
pub fn allocate_registers(mir: &Mir, _symbols: &SymbolTable) -> RegisterAllocation {
    // generate live ranges and the conflict graph
    let intervals = make_intervals(mir);
    let mut conflict_graph = make_conflict_graph(&intervals);
    println!("conflict graph: {:?}", Dot::with_config(
        &conflict_graph, &[Config::EdgeNoLabel]
    ));

    // start to color the conflict graph
    let mut colors = mir.precoloring.clone();
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

    let mut spill_var = None;

    for varbl in conflict_graph.nodes() {

        let cost = spill_cost(varbl, &intervals[&varbl], &conflict_graph, mir);

        match spill_var {
            None => {
                spill_var = Some((varbl, cost));
            }
            Some((_, prev_min_cost)) if cost < prev_min_cost => {
                spill_var = Some((varbl, cost));
            }
            _ => { /* nothing to do */ }
        }

    }
    assert!(spill_var.is_none()); // for now: no deciding to spill in order to split a live range


    // reconstruct the graph and apply colors
    while let Some((node, neighbors)) = colorable.pop() {
        recreate_node(&mut conflict_graph, node, neighbors);
        assign_color(&mut colors, &conflict_graph, node);
    }

    RegisterAllocation { colors }
}
