use crate::symbols::SymbolId;
use petgraph::graph::{Graph, NodeIndex};
use petgraph::Directed;
use std::fmt::{Formatter, Error};

#[derive(Debug, Copy, Clone)]
pub enum Val {
    Varbl(usize), // a variable value and it's ID
    Const(i32), // a constant
    Nothing, // no value
}
impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match *self {
            Val::Varbl(id) => write!(f, "V{}", id),
            Val::Const(val) => write!(f, "{}", val),
            Val::Nothing => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instr {
    Add     {dest: Val, a: Val, b: Val},      // store a + b in dest
    Sub     {dest: Val, a: Val, b: Val},      // store a - b in dest
    Equals  {dest: Val, a: Val, b: Val},      // store a == b in dest
    Set     {dest: Val, expr: Val},           // store expr in dest
    CallRtn (SymbolId),                       // call the function at the specified symbol id
    Print   (Val),                            // prints out the value
}

impl std::fmt::Display for Instr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Instr::Add     { dest, a, b } => write!(f, "{} = {} + {}", dest, a, b),
            Instr::Sub     { dest, a, b } => write!(f, "{} = {} - {}", dest, a, b),
            Instr::Equals  { dest, a, b } => write!(f, "{} = ({} == {})", dest, a, b),
            Instr::Set     { dest, expr } => write!(f, "{} = {}", dest, expr),
            Instr::CallRtn (symbol_id) => write!(f, "call rtn {}", symbol_id),
            Instr::Print(expr) => write!(f, "print {}", expr),
        }
    }
}

#[derive(Debug, Clone)]
pub enum EdgeInfo {
    Unit
}
impl EdgeInfo {
    pub fn new() -> EdgeInfo {
        EdgeInfo::Unit
    }
}
impl std::fmt::Display for EdgeInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "()")
    }
}

pub type MirGraph = Graph<MirBlock, EdgeInfo, Directed>;

#[derive(Debug, Clone)]
pub struct MirBlock {
    instrs: Vec<Instr>,
}
impl MirBlock {
    pub fn new() -> MirBlock {
        MirBlock { instrs: Vec::new() }
    }
    pub fn push(&mut self, instr: Instr) {
        self.instrs.push(instr);
    }
    pub fn iter(&self) -> impl Iterator<Item = &Instr> {
        self.instrs.iter()
    }
}
impl std::fmt::Display for MirBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        for instr in self.instrs.iter() {
            writeln!(f, "{}", instr)?;
        }
        Ok(())
    }
}

pub struct Mir {
    pub graph: MirGraph,         // the call flow graph
    pub entry_block: NodeIndex, // the entry block of the program
    pub exit_block: NodeIndex, // the exit block of the program
}