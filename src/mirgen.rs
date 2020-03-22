use crate::ast::{Ast, AstKind, AstBlock};
use crate::mir::{Instr, Val, MirGraph, MirBlock, EdgeInfo, Mir, VarblId, ExitStrategy};
use crate::symbols::{SymbolTable, SymbolId};
use petgraph::graph::NodeIndex;
use crate::scope::ScopeId;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use crate::functions::SubroutineInfo;

struct AlphaRenamer {
    counter: u32,
    table: HashMap<SymbolId, VarblId>, // maps identifiers to their corresponding mir values
}
impl AlphaRenamer {
    pub fn new() -> AlphaRenamer {
        AlphaRenamer {
            counter: 0,
            table: HashMap::new(),
        }
    }
    /// get the mir value associated with an identifier in the specified scope
    pub fn rename(&mut self, name: &str, scope_id: ScopeId, symbols: &SymbolTable) -> VarblId {
        let id = symbols.scope_table.get_id(name, scope_id).expect(&f!("unbound symbol {name} in scope {scope_id}")).clone();
        match self.table.entry(id) {
            Entry::Occupied(ref entry) => entry.get().clone(),
            Entry::Vacant(entry) => {
                let varbl = VarblId(self.counter);
                entry.insert(varbl.clone());
                self.counter += 1;
                varbl
            }
        }
    }
    /// make an intermediate value
    pub fn make_intermediate(&mut self) -> VarblId {
        let varbl = VarblId(self.counter);
        self.counter += 1;
        varbl
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
            AstKind::Real(_) => unimplemented!(),
            AstKind::String(_) => unimplemented!(),
            AstKind::Boole(v) => Val::Const(if v { 1 } else { 0 }),
            AstKind::Ident(ref name) => Val::Varbl(renamer.rename(name, self.scope_id, symbols)),
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
                // the current block goes to if_block if condition is nonzero and the else_block if it is zero
                mir_graph[*curr_block].set_exit_strategy(ExitStrategy::Branch{
                    condition, on_zero: else_block, on_nonzero: if_block
                });

                // create the branches
                let if_val = if_branch.generate_instr(mir_graph, &mut if_block, renamer, symbols);
                // if_block now contains the index of the last block in the if branch
                mir_graph[if_block].push(Instr::Set{dest, expr: if_val});
                let else_val = else_branch.generate_instr(mir_graph, &mut else_block, renamer, symbols);
                // else_block now contains the index of the last block in the else branch
                mir_graph[else_block].push(Instr::Set{dest, expr: else_val});

                // join the branches together into a new block
                *curr_block = mir_graph.add_node(MirBlock::new());
                mir_graph.add_edge(if_block, *curr_block, EdgeInfo::new());
                mir_graph.add_edge(else_block, *curr_block, EdgeInfo::new());
                // both branches will always feed into the new block
                mir_graph[if_block  ].set_exit_strategy(ExitStrategy::AlwaysGoto(*curr_block));
                mir_graph[else_block].set_exit_strategy(ExitStrategy::AlwaysGoto(*curr_block));

                Val::Varbl(dest)
            }
            AstKind::FunCall { ref func, ref args } => {
                let dest = renamer.make_intermediate();
                let arg_vals: Vec<Val> = args.iter().map(|arg| arg.generate_instr(mir_graph, curr_block, renamer, symbols)).collect();
                // hard-code intrinsics for now
                let maybe_intrinsic = match func.as_str() {
                    "add" => Some( Instr::Add{dest, a: arg_vals[0], b: arg_vals[1]} ),
                    "sub" => Some( Instr::Sub{dest, a: arg_vals[0], b: arg_vals[1]} ),
                    "equals" => Some( Instr::Equals{dest, a: arg_vals[0], b: arg_vals[1]} ),
                    "print" => Some( Instr::Print(arg_vals[0]) ),
                    _ => None,
                };
                if let Some(instr) = maybe_intrinsic {
                    mir_graph[*curr_block].push(instr);
                    return Val::Varbl(dest);
                }
                let symbol_id = *symbols.scope_table.get_id(func, self.scope_id).expect(&f!("unbound function `{func}`"));
                let subrtn = find_or_create_subroutine(mir_graph, &symbol_id, renamer, symbols);

                // jump to the subroutine
                // set each of the arguments in the subroutine to what values we want to call the subroutine with
                for (subrtn_arg, arg_val) in subrtn.args.iter().zip(arg_vals.into_iter()) {
                    mir_graph[*curr_block].push(Instr::Set { dest: *subrtn_arg, expr: arg_val });
                }
                mir_graph.add_edge(*curr_block, subrtn.start, EdgeInfo::new());
                // we are calling a specific function here
                mir_graph[*curr_block].set_exit_strategy(ExitStrategy::Call(subrtn.start));

                // jump back to a new block
                *curr_block = mir_graph.add_node(MirBlock::new());
                mir_graph.add_edge(subrtn.end, *curr_block, EdgeInfo::new());
                // find_or_create_subroutine should have set the exit strategy for subrtn.end
                subrtn.return_val
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
    // create the starting block for this subroutine
    let start = mir_graph.add_node(MirBlock::new());
    // create the dummy ending block for this routine
    let end = mir_graph.add_node(MirBlock::new());
    // extract the mir values for our parameters
    let args = fun_dec.params.iter().map(|param| {
        renamer.rename(param, fun_dec.scope_id(), symbols)
    }).collect::<Vec<VarblId>>();
    // create our return value
    let dest = renamer.make_intermediate();
    // set the mir now. this is so self referential (i.e. recursive) functions can use this information
    // and prevent find_or_create_subroutine from recursing forever
    fun_dec.set_mir(SubroutineInfo{ start, end, args, return_val: Val::Varbl(dest) });

    // we can find the actual ending block by passing it into generate_instr
    let mut actual_end = start;
    // translate the body of the function
    let actual_return = fun_dec.body.borrow().generate_instr(mir_graph, &mut actual_end, renamer, symbols);
    // hook the actual ending block into our "official" end
    mir_graph.add_edge(actual_end, end, EdgeInfo::new());
    mir_graph[actual_end].set_exit_strategy(ExitStrategy::AlwaysGoto(end));
    // now set our "official" return value
    mir_graph[end].push(Instr::Set{ dest, expr: actual_return });
    // and the function ends by returning to the caller
    mir_graph[end].set_exit_strategy(ExitStrategy::Ret);
    fun_dec.maybe_mir().unwrap()
}

pub fn create_mir(ast_nodes: &[Ast], symbols: &SymbolTable) -> Mir {
    let mut graph = MirGraph::new();
    let mut renamer = AlphaRenamer::new();
    let entry_block = graph.add_node(MirBlock::entry_point());
    let mut exit_block = entry_block; // this will be the exit block after all the blocks are translated
    for node in ast_nodes {
        node.generate_instr(&mut graph, &mut exit_block, &mut renamer, symbols);
    }
    // the program returns to it's caller at the end of the main function
    graph[exit_block].set_exit_strategy(ExitStrategy::Ret);
    Mir::from(graph, entry_block, exit_block)
}
