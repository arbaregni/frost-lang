use petgraph::graph::{Graph, NodeIndex};
use petgraph::{Directed, Direction};
use std::fmt::{Formatter, Error};
use std::collections::{HashSet, HashMap};
use once_cell::unsync::OnceCell;
use crate::codegen::LabelMaker;
use petgraph::visit::EdgeRef;

/// the id to keep track of variables in mir code
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone, Ord, PartialOrd)]
pub struct VarblId(pub u32);

pub const RETURN_ADDR_VARBL: VarblId = VarblId(0);

impl std::fmt::Display for VarblId {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        if *self == RETURN_ADDR_VARBL {
            write!(f, "return addr")
        } else {
            write!(f, "V{}", self.0)
        }
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
    Push    (VarblId),                            // pushes the varbl onto the stack
    Pop     (VarblId),                            // pops the stack, writing to the given varbl
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
            Instr::Push(varbl) => closure(varbl, false), // the value is read into memory
            Instr::Pop(varbl) => closure(varbl, true), // the value is overwritten from memory
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
            Instr::Push(varbl) => write!(f, "push {}", varbl),
            Instr::Pop(varbl) => write!(f, "pop {}", varbl),
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
impl ExitStrategy {
    pub fn visit_variables<F>(&self, closure: &mut F)
    where F: FnMut(&VarblId, bool)
    {
        match self {
            ExitStrategy::Undefined | ExitStrategy::AlwaysGoto(_) => { /* doesn't use or define any variables */ },

            ExitStrategy::Ret => closure(&RETURN_ADDR_VARBL, false), // returning from a function accesses the return address
            ExitStrategy::Call { .. } => closure(&RETURN_ADDR_VARBL, true), // calling a function sets the return address

            ExitStrategy::Branch { condition, .. } => condition.visit_variables(closure),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MirBlock {
    debug_tag: String, // useful information on where this block comes from
    maybe_label: OnceCell<String>,
    depth: u32, // the nesting depth of this block
    instrs: Vec<Instr>,
    exit_strategy: ExitStrategy,
}
impl MirBlock {
    pub fn with_depth(depth: u32) -> MirBlock {
        MirBlock {
            debug_tag: String::new(),
            maybe_label: OnceCell::new(),
            depth,
            instrs: vec![],
            exit_strategy: ExitStrategy::Undefined
        }
    }
    pub fn create_block(mir_graph: &mut MirGraph, depth: u32) -> NodeIndex {
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

    pub fn add_tag(&mut self, tag: &str) {
        if !self.debug_tag.is_empty() {
            self.debug_tag.push_str(", ");
        }
        self.debug_tag.push_str(tag);
    }
    pub fn tag(&self) -> Option<&str> {
        if self.debug_tag.is_empty() {
            return None;
        }
        Some(self.debug_tag.as_str())
    }

    /// our exit strategy (default is ExitStrategy::Undefined)
    pub fn set_exit_strategy(&mut self, strategy: ExitStrategy) {
        self.exit_strategy = strategy;
    }
    /// return a clone of our exit strategy
    pub fn exit_strategy(&self) -> ExitStrategy { self.exit_strategy.clone() }
    pub fn depth(&self) -> u32 { self.depth }
    pub fn push(&mut self, instr: Instr) { self.instrs.push(instr); }
    pub fn iter(&self) -> impl Iterator<Item = &Instr> { self.instrs.iter() }
    pub fn get(&self, idx: usize) -> Option<&Instr> { self.instrs.get(idx) }
    pub fn len(&self) -> usize { self.instrs.len() }
}

impl std::fmt::Display for MirBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        writeln!(f, "(label = {:?}, depth = {})", self.maybe_label, self.depth)?;
        for instr in self.instrs.iter() {
            writeln!(f, "{}", instr)?;
        }
        writeln!(f, "{:?}", self.exit_strategy)?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct Mir {
    pub graph: MirGraph,         // the call flow graph
    pub entry_block: NodeIndex, // the entry block of the program
    pub exit_block: NodeIndex, // the exit block of the program
    varbl_total: u32,
    pub ordering: Vec<NodeIndex>,
}
impl Mir {
    /// Construct a Mir object from the graph
    /// Performs some analysis on the graph, such as (TODO: reducing) and sorting
    pub fn from(graph: MirGraph, entry_block: NodeIndex, exit_block: NodeIndex, varbl_total: u32) -> Mir {
        let mut this = Mir {
            graph, entry_block, exit_block, varbl_total, ordering: Vec::new(),
        };
        //TODO: simplify the graph
        this.set_nesting_depths();
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
    /// set each of the node's nesting depths
    /// this is the number of cycles its a part of
    pub fn set_nesting_depths(&mut self) {
        // mir: the mir struct we are using
        // path: a vector of node indices in the order that we have visited them
        // visited: maps node indices --> path indices
        fn helper(mir: &mut Mir, curr_block: NodeIndex, path: &mut Vec<NodeIndex>, visited: &mut HashMap<NodeIndex, usize>) {
            // do cycle detection before visiting curr_block
            if let Some(&cycle_start) = visited.get(&curr_block) {
                // we already visited curr_block
                // so this is a cycle: increment the depths of everything in it
                for path_idx in cycle_start..path.len() {
                    mir.graph[path[path_idx]].increment_depth();
                }
                // nothing else to do here
                return;
            }

            // now we can visit this node
            visited.insert(curr_block, path.len());
            path.push(curr_block);

            // no we visit each of our children
            let next_blocks = mir.graph.edges_directed(curr_block, Direction::Outgoing)
                .map(|edge| edge.target())
                .collect::<Vec<_>>();

            for block in next_blocks {
                helper(mir, block, path, visited);
            }

            // we are done visiting this node
            visited.remove(&curr_block).expect("could not remove current block from visited");
            path.pop().expect("could not remove current block from path");
        }

        let mut path = vec![];
        let mut visited = HashMap::new();
        helper(self, self.entry_block, &mut path, &mut visited);
    }
    pub fn blocks(&self) -> MirBlocks {
        MirBlocks { mir: self, idx: 0 }
    }
    pub fn varbl_total(&self) -> u32 { self.varbl_total }
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