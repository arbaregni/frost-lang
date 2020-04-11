use crate::mir::{Mir, VarblId, InstrIndex};
use crate::symbols::SymbolTable;
use crate::util;
use std::collections::HashMap;
use std::cmp;
use petgraph::dot::{Dot, Config};
use petgraph::{Undirected};
use petgraph::graphmap::GraphMap;
use std::fmt::{Formatter, Error};
use std::cmp::Ordering;

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

type InstrSpan = (InstrIndex, InstrIndex);
type ConflictGraph = GraphMap<VarblId, InstrSpan, Undirected>;

type LiveRangeMap = HashMap<VarblId, LiveRange>;

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
struct LiveRange {
    // an ordered vector of instructions
    // tuples are indices where they are found and a bool indicating if the variable is defined (or else, it's just accessed)
    uses: Vec<(InstrIndex, bool)>,
    // indices of where defines are in the uses vector
    // intuitively, this starts a def-use chain
    chain_starts: Vec<usize>,
}
impl LiveRange {
    /// create an empty live range
    fn new() -> LiveRange {
        LiveRange { uses: Vec::new(), chain_starts: Vec::new() }
    }
    /// add a new instruction to the live range,
    /// asserts that this function is called in the correct order
    fn push(&mut self, idx: InstrIndex, is_def: bool) {
        // a check to make sure that the new index is after the previous last instruction in the chain
        assert!(self.uses.last().map(|(tail, _)| *tail <= idx).unwrap_or(true));
        if is_def {
            self.chain_starts.push(self.uses.len());
        }
        self.uses.push((idx, is_def));
    }
    /// returns the intersection (if one exists) of self and other
    fn intersection(&self, other: &LiveRange) -> Option<(InstrIndex, InstrIndex)> {
        // empty ranges do not intersect
        if self.is_empty() || other.is_empty() { return None; }

        // we have several cases for where self and other could be relative to each other:
        // 1)                               2)
        //    ┏━━self━━┓                                     ┏━━self━━┓
        //             ^  ┗━━━other━━┛         ┗━━━other━━┛  ^
        //             |  ^                               ^  |
        //    right ---+  |                               |  +---- left
        //                +--- left             right ----+
        //
        // 3)                               4)
        //    ┏━━━━self━━━━┓                             ┏━━━━self━━━━┓
        //           ┗━━━━━other━━━━┛          ┗━━━━━other━━━━┛
        //           ^     ^                             ^    ^
        //           |     |                             |    |
        //   left ---+     +--- right            left ---+    +--- right
        //
        // 5)                               6)
        //     ┏━━━━━━self━━━━━━━┓                      ┏━self━┓
        //          ┗━other━┛                       ┗━━━━━other━━━━┛
        //          ^       ^                           ^      ^
        //          |       |                           |      |
        //  left ---+       +--- right          left ---+      +--- right
        //
        // 7)                               8)
        //    ┏━━━self━━━┓                                ┏━━━self━━━┓
        //               ┗━━━━other━━━┛      ┗━━━━other━━━┛
        //               ^                                ^
        //               |                                |
        //       left ---+--- right               left ---+--- right
        //
        // notice that if they intersect, left is less than right
        // if there is no intersection, left is greater than right

        let left = cmp::max(self.begin(), other.begin());
        let right = cmp::min(self.end(), other.end());
        match left.cmp(right) {
            Ordering::Less => Some((*left, *right)), // an intersection exists
            Ordering::Equal => {
                //    notice that LiveRange::begin() must be a definition that does not depend on previous values
                // (because there are no previous values to depend on)
                // by definition, anything after LiveRange::end() does not access its value
                //   therefore, we have if self.begin() == other.end() or other.begin() == self.end()
                // there is NO intersection.
                None
            },
            Ordering::Greater => None, // no intersection exists
        }
    }
    fn is_empty(&self) -> bool { self.uses.is_empty() }
    fn begin(&self) -> &InstrIndex { self.uses.first().map(|(idx, _)| idx).unwrap() }
    fn end(&self) -> &InstrIndex { self.uses.last().map(|(idx, _)| idx).unwrap() }
}
/// construct the set of live ranges across the entire program
fn make_live_ranges(mir: &Mir) -> LiveRangeMap {
    let mut intervals = HashMap::new();
    for (instr, idx) in mir.iter() {
        instr.visit_variables(&mut |varbl, is_def| {
            let interval = intervals.entry(*varbl).or_insert(LiveRange::new());
            interval.push(idx, is_def);
        })
    }
    intervals
}



/// construct the interference graph from the set of intervals
fn make_conflict_graph(ranges: &HashMap<VarblId, LiveRange>) -> ConflictGraph {
    //TODO make constructing the graph faster
    let nodes = ranges.len();
    let edges = util::estimate_edge_count(nodes);
    let mut graph = ConflictGraph::with_capacity(nodes, edges);
    for (&varbl_a, range_a) in ranges {
        graph.add_node(varbl_a);
        for (&varbl_b, range_b) in ranges {
            if varbl_a == varbl_b { continue; }
            if let Some(intersect) = range_a.intersection(range_b) {
                graph.add_edge(varbl_a, varbl_b, intersect);
            }
        }
    }
    graph
}


struct RemovedNode {
    node: VarblId,
    neighbors: Vec<VarblId>,
}

/// removes a node with a degree strictly less than `n` from `conflict_graph`,
/// returning a tuple containing the node and it's neighbors
fn remove_node_with_lesser_degree(conflict_graph: &mut ConflictGraph, n: usize) -> Option<RemovedNode> {
    // scan for the first node that with degree < n
    for node in conflict_graph.nodes() {
        let degree = conflict_graph.neighbors(node).count();
        if degree >= n { continue; }
        // the degree is less than n, so we remove it and pop it onto the stack
        let neighbors = conflict_graph.neighbors(node).collect::<Vec<_>>();
        conflict_graph.remove_node(node);
        return Some(RemovedNode{node, neighbors});
    }
    None
}

/// recreate the removed node in the conflict graph,
/// adding the edges (where it still needs them) to its previous neighbors
/// we assume that no new edges were created
fn recreate_node(conflict_graph: &mut ConflictGraph, ranges: &LiveRangeMap, removed_node: RemovedNode) {
    let RemovedNode{node, neighbors} = removed_node;
    for neighbor in neighbors.into_iter() {
        if let Some(intersect) = ranges[&node].intersection(&ranges[&neighbor]) {
            conflict_graph.add_edge(node, neighbor, intersect);
        } else {
            // work has been done and these variables no longer overlap
        }
    }
}

/// The heuristic for the cost it would take to spill the given variable
/// Uses the cost of spilling / degree of node
fn spill_cost(varbl: VarblId, _live_range: &LiveRange, conflict_graph: &ConflictGraph, _mir: &Mir) -> f32 {
    let degree = conflict_graph.neighbors(varbl).count();

    let cost = 0;



    cost as f32 / degree as f32
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
    let ranges = make_live_ranges(mir);
    let mut conflict_graph = make_conflict_graph(&ranges);
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

        let cost = spill_cost(varbl, &ranges[&varbl], &conflict_graph, mir);

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
    while let Some(removed_node) = colorable.pop() {
        let node = removed_node.node;
        // first, we put the node back into the graph
        recreate_node(&mut conflict_graph, &ranges, removed_node);
        // now that we have it back in the graph, we can give it a color
        // using a greedy coloring (i.e, next non conflicting color)
        assign_color(&mut colors, &conflict_graph, node);
    }

    RegisterAllocation { colors }
}
