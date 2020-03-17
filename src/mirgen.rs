use crate::ast::{Ast, AstKind, AstBlock};
use crate::mir::{Instr, Val, MirGraph, MirBlock, EdgeInfo};
use crate::symbols::{SymbolTable, SymbolId};
use petgraph::graph::NodeIndex;
use crate::scope::ScopeId;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use crate::functions::SubroutineInfo;

struct AlphaRenamer {
    counter: usize,
    table: HashMap<SymbolId, Val>, // maps identifiers to their corresponding mir values
}
impl AlphaRenamer {
    pub fn new() -> AlphaRenamer {
        AlphaRenamer {
            counter: 0,
            table: HashMap::new(),
        }
    }
    /// get the mir value associated with an identifier in the specified scope
    pub fn rename(&mut self, name: &str, scope_id: ScopeId, symbols: &SymbolTable) -> Val {
        let id = symbols.scope_table.get_id(name, scope_id).expect(&f!("unbound symbol {name} in scope {scope_id}")).clone();
        match self.table.entry(id) {
            Entry::Occupied(ref entry) => entry.get().clone(),
            Entry::Vacant(entry) => {
                let val = Val::Varbl(self.counter);
                entry.insert(val.clone());
                self.counter += 1;
                val
            }
        }
    }
    /// make an intermediate value
    pub fn make_intermediate(&mut self) -> Val {
        let val = Val::Varbl(self.counter);
        self.counter += 1;
        val
    }
}

impl Ast {
    /// Recursively create the IR for the given Ast node.
    ///  mir_graph: the directed call flow graph
    ///  curr_block: a reference to the current block to work on.
    ///             When the function returns, it is updated to be the last block
    ///  renamer: an alpha renamer to translate values into their mir equivalents
    ///  symbols: the symbol table
    fn generate_instr(&self, mir_graph: &mut MirGraph, curr_block: &mut NodeIndex, renamer: &mut AlphaRenamer, symbols: &SymbolTable) -> Val {
        match self.kind {
            AstKind::Int(v) => Val::Const(v),
            AstKind::Real(_v) => unimplemented!(),
            AstKind::String(_) => unimplemented!(),
            AstKind::Boole(v) => Val::Const(if v { 1 } else { 0 }),
            AstKind::Ident(ref name) => renamer.rename(name, self.scope_id, symbols),
            AstKind::TypeExpr(_) => unimplemented!(),
            AstKind::Block(ref block) => block.generate_instr(mir_graph, curr_block, renamer, symbols),
            AstKind::IfStmnt{ref test, ref if_branch, ref else_branch} => {
                // calculate the condition first
                let condition = test.generate_instr(mir_graph, curr_block, renamer, symbols);
                // create a place for the result when the branches join back
                let dest = renamer.make_intermediate();

                // create the starting blocks of the branches
                let mut if_block = mir_graph.add_node(MirBlock::new());
                let mut else_block = mir_graph.add_node(MirBlock::new());
                mir_graph.add_edge(*curr_block, if_block, EdgeInfo::new());
                mir_graph.add_edge(*curr_block, else_block, EdgeInfo::new());

                // create the branches
                let if_val = if_branch.generate_instr(mir_graph, &mut if_block, renamer, symbols);
                mir_graph[if_block].push(Instr::Set{dest, expr: if_val}); // note: if_block now contains the index of the last block in the if branch
                let else_val = else_branch.generate_instr(mir_graph, &mut else_block, renamer, symbols);
                mir_graph[else_block].push(Instr::Set{dest, expr: else_val}); // note: else_block now contains the index of the last block in the else branch

                // join the branches together into a new block
                *curr_block = mir_graph.add_node(MirBlock::new());
                mir_graph.add_edge(if_block, *curr_block, EdgeInfo::new());
                mir_graph.add_edge(else_block, *curr_block, EdgeInfo::new());

                dest
            }
            AstKind::FunCall { ref func, ref args } => {
                let dest = renamer.make_intermediate();
                let arg_vals: Vec<Val> = args.iter().map(|arg| arg.generate_instr(mir_graph, curr_block, renamer, symbols)).collect();
                // hard-code intrinsics for now
                match func.as_str() {
                    "add" => {
                        mir_graph[*curr_block].push(Instr::Add{dest, a: arg_vals[0], b: arg_vals[1]});
                        return dest;
                    }
                    "sub" => {
                        mir_graph[*curr_block].push(Instr::Sub{dest, a: arg_vals[0], b: arg_vals[1]});
                        return dest;
                    }
                    "equals" => {
                        mir_graph[*curr_block].push(Instr::Equals{dest, a: arg_vals[0], b: arg_vals[1]});
                        return dest;
                    }
                    "print" => {
                        mir_graph[*curr_block].push(Instr::Print(arg_vals[0]));
                        return dest;
                    }
                    _ => { /* not an intrinsic */ }
                }
                let symbol_id = *symbols.scope_table.get_id(func, self.scope_id).expect(&f!("unbound function `{func}`"));
                let subrtn = find_or_create_subroutine(mir_graph, &symbol_id, renamer, symbols);

                // jump to the subroutine
                // set each of the arguments in the subroutine to what values we want to call the subroutine with
                for (subrtn_arg, arg_val) in subrtn.args.iter().zip(arg_vals.into_iter()) {
                    mir_graph[*curr_block].push(Instr::Set { dest: *subrtn_arg, expr: arg_val });
                }
                mir_graph[*curr_block].push(Instr::CallRtn(symbol_id));
                mir_graph.add_edge(*curr_block, subrtn.start, EdgeInfo::new());

                // jump back to a new block
                *curr_block = mir_graph.add_node(MirBlock::new());
                mir_graph.add_edge(subrtn.end, *curr_block, EdgeInfo::new());
                // put the result of the subroutine into our value
                mir_graph[*curr_block].push(Instr::Set{dest, expr: subrtn.return_val });

                dest
            }
            AstKind::Assign{ ref ident, opt_type: _, ref rhs } => {
                let dest = renamer.rename(ident, rhs.scope_id, symbols);
                let expr = rhs.generate_instr(mir_graph, curr_block, renamer, symbols);
                mir_graph[*curr_block].push(Instr::Set{dest, expr});
                Val::Nothing
            }
            AstKind::FunDec{ .. } => Val::Nothing, // the function declaration itself has no runtime behavior
            AstKind::StructDec {..} => Val::Nothing, //  declarations have no runtime behavior
        }
    }
}

impl AstBlock {
    fn generate_instr(&self, mir_graph: &mut MirGraph, curr_block: &mut NodeIndex, renamer: &mut AlphaRenamer, symbols: &SymbolTable) -> Val {
        let mut readable = Val::Nothing;
        for statement in self.0.iter() {
            readable = statement.generate_instr(mir_graph, curr_block, renamer, symbols);
        }
        readable
    }
}

///    Returns a tuple of the (starting block index, finishing block index, return value)
/// for the subroutine with the given symbol id,
/// creating the blocks if necessary
fn find_or_create_subroutine<'a>(mir_graph: &mut MirGraph, symbol_id: &SymbolId, renamer: &mut AlphaRenamer, symbols: &'a SymbolTable) -> &'a SubroutineInfo {
    // lookup our function declaration
    let fun_dec = symbols.get_fun_by_id(symbol_id).expect(&f!("unbound function with symbol id {symbol_id}"));
    if let Some(subrtn_info) = fun_dec.maybe_mir() {
        return subrtn_info;
    }
    // creating the starting block for this subroutine
    let start = mir_graph.add_node(MirBlock::new());
    // pass in the ending block index by reference
    // it will hold the index of the last block created
    let mut end = start;
    // translate the body of the function
    let returns = fun_dec.body.borrow().generate_instr(mir_graph, &mut end, renamer, symbols);
    //TODO: insert an Instr::Ret here?

    // extract the mir values for our parameters
    let args = fun_dec.params.iter().map(|param| {
        renamer.rename(param, fun_dec.scope_id(), symbols)
    }).collect::<Vec<Val>>();
    fun_dec.set_mir(SubroutineInfo{ start, end, args, return_val: returns });
    fun_dec.maybe_mir().unwrap()
}

pub fn create_mir_instrs(ast_nodes: &[Ast], symbols: &mut SymbolTable) -> MirGraph {
    let mut mir_graph = MirGraph::new();
    let mut renamer = AlphaRenamer::new();
    let mut entry_block = mir_graph.add_node(MirBlock::new());
    for node in ast_nodes {
        node.generate_instr(&mut mir_graph, &mut entry_block, &mut renamer, symbols);
    }
    mir_graph
}
