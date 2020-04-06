use petgraph::graph::{Graph, NodeIndex};
use petgraph::{Directed};
use std::fmt::{Formatter, Error};
use std::collections::HashSet;
use std::cmp::Ordering;
use once_cell::unsync::OnceCell;
use crate::codegen::{LabelMaker, Coloring};

/// the id to keep track of variables in mir code
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone, Ord, PartialOrd)]
pub struct VarblId(pub u32);

impl std::fmt::Display for VarblId {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "V{}", self.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Val {
    Varbl(VarblId), // a variable value with it's ID
    Const(i32), // a constant
    Nothing, // no value
}
impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match *self {
            Val::Varbl(id) => write!(f, "{}", id),
            Val::Const(val) => write!(f, "{}", val),
            Val::Nothing => write!(f, "()"),
        }
    }
}
impl Val {
    /// Call `closure` if we are a variable value, with the second parameter being `false`
    fn visit_variables<F>(&self, closure: &mut F)
        where F: FnMut(&VarblId, bool)
    {
        if let Val::Varbl(ref varbl) = self {
            closure(varbl, false)
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instr {
    Add     {dest: VarblId, a: Val, b: Val},      // store a + b in dest
    Sub     {dest: VarblId, a: Val, b: Val},      // store a - b in dest
    Equals  {dest: VarblId, a: Val, b: Val},      // store a == b in dest
    Set     {dest: VarblId, expr: Val},           // store expr in dest
    Print   (Val),                                // prints out the value
    Save    {varbl: VarblId },                    // saves the value in memory. spill_id is shared by corresponding saves/restores
    Restore {varbl: VarblId },                    // restores the value from memory
}
impl Instr {
    /// Call `closure` on every variable within this instruction.
    /// The second parameter is true if and only if the variable is set (written to) by this instruction.
    pub fn visit_variables<F>(&self, closure: &mut F)
        where F: FnMut(&VarblId, bool)
    {
        match self {
            Instr::Add { dest, a, b } | Instr::Sub { dest, a, b } | Instr::Equals { dest, a, b } => {
                closure(dest, true); // dest is set to a + b
                a.visit_variables(closure);
                b.visit_variables(closure);
            },
            Instr::Set { dest, expr } => {
                closure(dest, true); // dest is set to expr
                expr.visit_variables(closure);
            },
            Instr::Print(val) => {
                val.visit_variables(closure);
            },
            Instr::Save    { varbl } => closure(varbl, false), // the value is read into memory
            Instr::Restore { varbl } => closure(varbl, true), // the value is overwritten from memory
        }
    }
}
impl std::fmt::Display for Instr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Instr::Add     { dest, a, b } => write!(f, "{} = {} + {}", dest, a, b),
            Instr::Sub     { dest, a, b } => write!(f, "{} = {} - {}", dest, a, b),
            Instr::Equals  { dest, a, b } => write!(f, "{} = ({} == {})", dest, a, b),
            Instr::Set     { dest, expr } => write!(f, "{} = {}", dest, expr),
            Instr::Print(expr) => write!(f, "print {}", expr),
            Instr::Save    { varbl } => write!(f, "save {}", varbl),
            Instr::Restore { varbl } => write!(f, "restore {}", varbl),
        }
    }
}

#[derive(Debug, Clone)]
pub struct EdgeInfo;
impl EdgeInfo {
    pub fn new() -> EdgeInfo {
        EdgeInfo{}
    }
}


impl std::fmt::Display for EdgeInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "()")
    }
}

pub type MirGraph = Graph<MirBlock, EdgeInfo, Directed>;

#[derive(Debug, Clone)]
pub enum ExitStrategy {
    /// we don't know what will happen when we reach the end of the block
    Undefined,
    /// we will return from the subroutine at the end of the block
    Ret,
    /// we are guaranteed to go to this block
    AlwaysGoto(NodeIndex),
    /// we are entering a subroutine: jump there, then return to the next block after the call returns
    Call{subrtn: NodeIndex, after_call: NodeIndex},
    /// branch based on some value
    Branch{ condition: Val, on_zero: NodeIndex, on_nonzero: NodeIndex}
}

#[derive(Debug, Clone)]
pub struct MirBlock {
    maybe_label: OnceCell<String>,
    depth: usize, // the nexting depth of this block
    instrs: Vec<Instr>,
    exit_strategy: ExitStrategy,
}
impl MirBlock {
    pub fn with_depth(depth: usize) -> MirBlock {
        MirBlock {
            maybe_label: OnceCell::new(),
            depth,
            instrs: vec![],
            exit_strategy: ExitStrategy::Undefined
        }
    }
    pub fn create_block(mir_graph: &mut MirGraph, depth: usize) -> NodeIndex {
        let block = MirBlock::with_depth(depth);
        mir_graph.add_node(block)
    }
    pub fn create_block_at_same_depth(mir_graph: &mut MirGraph, curr_block: &NodeIndex) -> NodeIndex {
        let depth = mir_graph[*curr_block].depth;
        let block = MirBlock::with_depth(depth);
        let idx = mir_graph.add_node(block);
        //mir_graph.add_edge(*curr_block, idx, EdgeInfo::new());
        idx
    }
    pub fn entry_point() -> MirBlock {
        let this = MirBlock::with_depth(0);
        this.maybe_label.set(String::from("main")).unwrap();
        this
    }
    /// get our label, possibly using the label maker
    pub fn label(&self, label_maker: &mut LabelMaker) -> &str {
        if self.maybe_label.get().is_none() {
            // we need to set it
            self.maybe_label.set(label_maker.make_label()).unwrap();
        }
        self.maybe_label.get().unwrap().as_str()
    }
    /// increment the nesting depth of this block
    pub fn increment_depth(&mut self) {
        self.depth += 1;
    }
    pub fn set_exit_strategy(&mut self, strategy: ExitStrategy) {
        self.exit_strategy = strategy;
    }
    pub fn exit_strategy(&self) -> ExitStrategy { self.exit_strategy.clone() }
    pub fn push(&mut self, instr: Instr) { self.instrs.push(instr); }
    pub fn iter(&self) -> impl Iterator<Item = &Instr> { self.instrs.iter() }
    pub fn get(&self, idx: usize) -> Option<&Instr> { self.instrs.get(idx) }
    pub fn len(&self) -> usize { self.instrs.len() }
}

impl std::fmt::Display for MirBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        for instr in self.instrs.iter() {
            writeln!(f, "{}", instr)?;
        }
        Ok(())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct InstrIndex {
    outer: usize, // indexes into the ordering to get the node index
    inner: usize, // indexes into the mir block
}

impl InstrIndex {
    pub fn new(outer: usize, inner: usize) -> InstrIndex {
        InstrIndex { outer, inner }
    }
}
impl std::cmp::Ord for InstrIndex {
    fn cmp(&self, other: &InstrIndex) -> Ordering {
        match self.outer.cmp(&other.outer) {
            Ordering::Equal => self.inner.cmp(&other.inner),
            ord => ord
        }
    }
}
impl std::cmp::PartialOrd for InstrIndex {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
pub struct Mir {
    pub graph: MirGraph,         // the call flow graph
    pub precoloring: Coloring,    // values whose registers are already decided
    pub entry_block: NodeIndex, // the entry block of the program
    pub exit_block: NodeIndex, // the exit block of the program
    pub ordering: Vec<NodeIndex>,
}
impl Mir {
    /// Construct a Mir object from the graph
    /// Performs some analysis on the graph, such as (TODO: reducing) and sorting
    pub fn from(graph: MirGraph, precoloring: Coloring, entry_block: NodeIndex, exit_block: NodeIndex) -> Mir {
        let mut this = Mir {
            graph, precoloring, entry_block, exit_block, ordering: Vec::new(),
        };
        //TODO: simplify the graph
        this.sort_blocks();
        this
    }
    fn sort_blocks(&mut self) {
        // TODO getting better results out of this by using dominators
        // for now, we'll just pick an arbitrary depth-first order
        self.ordering = Vec::with_capacity(self.graph.node_count());
        let mut todo = vec![self.entry_block];
        let mut visited = HashSet::new();
        while let Some(node) = todo.pop() {
            self.ordering.push(node);
            visited.insert(node);
            for neighbor in self.graph.neighbors(node) {
                if visited.contains(&neighbor) { continue; }
                todo.push(neighbor);
            }
        }
    }
    pub fn get(&self, idx: InstrIndex) -> Option<&Instr> {
        let node_idx = *self.ordering.get(idx.outer)?;
        self.graph[node_idx].get(idx.inner)
    }
    pub fn blocks(&self) -> MirBlocks {
        MirBlocks { mir: self, idx: 0 }
    }
    pub fn iter(&self) -> MirIterator {
        MirIterator { mir: self, idx: InstrIndex::new(0, 0) }
    }
}

pub struct MirBlocks<'a> {
    mir: &'a Mir,
    idx: usize,
}
impl <'a> std::iter::Iterator for MirBlocks<'a> {
    type Item = &'a MirBlock;
    fn next(&mut self) -> Option<Self::Item> {
        let node_idx = self.mir.ordering.get(self.idx)?;
        self.idx += 1;
        Some( &self.mir.graph[*node_idx] )
    }
}

pub struct MirIterator<'a> {
    mir: &'a Mir,
    idx: InstrIndex,
}
impl <'a> std::iter::Iterator for MirIterator<'a> {
    type Item = (&'a Instr, InstrIndex);

    fn next(&mut self) -> Option<Self::Item> {
        let instr = self.mir.get(self.idx)?;
        let old_idx = self.idx;
        // advance the iterator to the next instruction
        self.idx.inner += 1;
        let mut node_idx = self.mir.ordering[self.idx.outer];
        while self.idx.inner >= self.mir.graph[node_idx].len() {
            self.idx.inner = 0;
            self.idx.outer += 1;
            if self.idx.outer >= self.mir.ordering.len() { break; }
            node_idx = self.mir.ordering[self.idx.outer];
        }
        Some( (instr, old_idx) )
    }
}