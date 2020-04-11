use crate::ast::{Ast, AstKind, AstBlock};
use crate::mir::{Instr, Val, MirGraph, MirBlock, EdgeInfo, Mir, VarblId, ExitStrategy};
use crate::symbols::{SymbolTable, SymbolId};
use petgraph::graph::NodeIndex;
use crate::scope::ScopeId;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use crate::functions::SubroutineInfo;
use crate::codegen::{Coloring, Color};
use crate::{codegen, mir};

struct Context {
    varbl_counter: u32, // a counter for generating variable id's
    varbl_table: HashMap<SymbolId, VarblId>, // maps identifiers to their corresponding varbl id's
    precoloring: Coloring,   // some values need to be in certain registers
}
impl Context {
    pub fn new() -> Context {
        let mut this = Context {
            varbl_counter: 1, // return address is V0
            varbl_table: HashMap::new(),
            precoloring: Coloring::new(),
        };
        this.precolor(mir::RETURN_ADDR_VARBL, codegen::RETURN_ADDR_COLOR);
        this
    }
    /// get the mir value associated with an identifier in the specified scope
    pub fn rename(&mut self, name: &str, scope_id: ScopeId, symbols: &SymbolTable) -> VarblId {
        let id = symbols.scope_table.get_id(name, scope_id).expect(&f!("unbound symbol {name} in scope {scope_id}")).clone();
        match self.varbl_table.entry(id) {
            Entry::Occupied(ref entry) => entry.get().clone(),
            Entry::Vacant(entry) => {
                let varbl = VarblId(self.varbl_counter);
                entry.insert(varbl.clone());
                self.varbl_counter += 1;
                varbl
            }
        }
    }
    /// make an intermediate value
    pub fn make_intermediate(&mut self) -> VarblId {
        let varbl = VarblId(self.varbl_counter);
        self.varbl_counter += 1;
        varbl
    }
    /// color a value to a specific register
    pub fn precolor(&mut self, varbl: VarblId, color: Color) {
        self.precoloring.insert(varbl, color);
    }
}

impl Ast {
    /// Recursively create the IR for the given Ast node.
    ///  mir_graph: the directed call flow graph
    ///  curr_block: a reference to the current block to work on.
    ///             When the function returns, it is updated to be the last block
    ///  ctx: used to translate identifiers into mir values, and to keep track of pairs of save/reloads
    ///  symbols: the symbol table
    fn generate_instr(&self, mir_graph: &mut MirGraph, curr_block: &mut NodeIndex, ctx: &mut Context, symbols: &SymbolTable) -> Val {
        match self.kind {
            AstKind::Int(v) => Val::Const(v),
            AstKind::Real(_) => unimplemented!(),
            AstKind::String(_) => unimplemented!(),
            AstKind::Boole(v) => Val::Const(if v { 1 } else { 0 }),
            AstKind::Ident(ref name) => Val::Varbl(ctx.rename(name, self.scope_id, symbols)),
            AstKind::TypeExpr(_) => unimplemented!(),
            AstKind::Block(ref block) => block.generate_instr(mir_graph, curr_block, ctx, symbols),
            AstKind::IfStmnt{ref test, ref if_branch, ref else_branch} => {
                // calculate the condition first
                let condition = test.generate_instr(mir_graph, curr_block, ctx, symbols);

                // create the starting blocks of the branches
                let mut if_block = MirBlock::create_block_at_same_depth(mir_graph, curr_block);
                mir_graph[if_block].add_tag("if branch");
                let mut else_block = MirBlock::create_block_at_same_depth(mir_graph, curr_block);
                mir_graph[else_block].add_tag("else branch");
                mir_graph.add_edge(*curr_block, if_block, EdgeInfo::new());
                mir_graph.add_edge(*curr_block, else_block, EdgeInfo::new());
                // the current block goes to if_block if condition is nonzero and the else_block if it is zero
                mir_graph[*curr_block].set_exit_strategy(ExitStrategy::Branch{
                    condition, on_zero: else_block, on_nonzero: if_block
                });

                // create the branches
                let if_val = if_branch.generate_instr(mir_graph, &mut if_block, ctx, symbols);
                let else_val = else_branch.generate_instr(mir_graph, &mut else_block, ctx, symbols);

                // create a place for the result when the branches join back
                let dest = ctx.make_intermediate();

                // if_block now contains the index of the last block in the if branch
                mir_graph[if_block].push(Instr::Set{dest, expr: if_val});
                // else_block now contains the index of the last block in the else branch
                mir_graph[else_block].push(Instr::Set{dest, expr: else_val});

                // join the branches together into a new block
                *curr_block = MirBlock::create_block_at_same_depth(mir_graph, curr_block);
                mir_graph.add_edge(if_block, *curr_block, EdgeInfo::new());
                mir_graph.add_edge(else_block, *curr_block, EdgeInfo::new());
                // both branches will always feed into the new block
                mir_graph[if_block  ].set_exit_strategy(ExitStrategy::AlwaysGoto(*curr_block));
                mir_graph[else_block].set_exit_strategy(ExitStrategy::AlwaysGoto(*curr_block));

                Val::Varbl(dest)
            }
            AstKind::FunCall { ref func, ref args } => {
                let arg_vals: Vec<Val> = args.iter().map(|arg| arg.generate_instr(mir_graph, curr_block, ctx, symbols)).collect();
                // hard-code intrinsics for now
                match func.as_str() {
                    "add" => {
                        let dest = ctx.make_intermediate();
                        mir_graph[*curr_block].push(Instr::Add{dest, a: arg_vals[0], b: arg_vals[1]});
                        return Val::Varbl(dest);
                    }
                    "sub" => {
                        let dest = ctx.make_intermediate();
                        mir_graph[*curr_block].push(Instr::Sub{dest, a: arg_vals[0], b: arg_vals[1]});
                        return Val::Varbl(dest);
                    }
                    "equals" => {
                        let dest = ctx.make_intermediate();
                        mir_graph[*curr_block].push(Instr::Equals{dest, a: arg_vals[0], b: arg_vals[1]});
                        return Val::Varbl(dest);
                    }
                    "print" => {
                        mir_graph[*curr_block].push(Instr::Print(arg_vals[0]));
                        return Val::Nothing;
                    }
                    _ => { /* do nothing */ }
                }
                let symbol_id = *symbols.scope_table.get_id(func, self.scope_id).expect(&f!("unbound function `{func}`"));
                let subrtn = find_or_create_subroutine(mir_graph, &symbol_id, ctx, symbols);


                // we always spill the return address
                mir_graph[*curr_block].push(Instr::Push(mir::RETURN_ADDR_VARBL));
                // load up the the arguments to the subroutine
                // we spill everything to avoid clobbering previous parameters
                // NOTE: graph coloring may later undo some of this spilling,
                //       so we are not too worried about performance here
                for (subrtn_param, arg_val) in subrtn.params.iter().zip(arg_vals.into_iter()) {
                    // save the previous value of the argument
                    mir_graph[*curr_block].push(Instr::Push(*subrtn_param));
                    // overwrite the value with the argument
                    mir_graph[*curr_block].push(Instr::Set { dest: *subrtn_param, expr: arg_val });
                }
                // jump back to a new block after the subroutine finishes
                let after_call = MirBlock::create_block_at_same_depth(mir_graph, curr_block);

                // jump to the body of the subroutine
                mir_graph.add_edge(*curr_block, subrtn.start, EdgeInfo::new());
                mir_graph[*curr_block].set_exit_strategy(ExitStrategy::Call{subrtn: subrtn.start, after_call});
                // the subroutine needs an edge from its end to where we resume control.
                // the exit strategy should already be set
                mir_graph.add_edge(subrtn.end, after_call, EdgeInfo::new());

                // now that we're done with the call, the current block should move
                *curr_block = after_call;


                // now we reload all of the subroutine's parameters
                for subrtn_param in subrtn.params.iter().rev() {
                    mir_graph[*curr_block].push(Instr::Pop(*subrtn_param));
                }
                // reload the return address
                mir_graph[*curr_block].push(Instr::Pop(mir::RETURN_ADDR_VARBL));

                // we can simply return the resultant value of the subroutine call
                subrtn.return_val
            }
            AstKind::Assign{ ref ident, opt_type: _, ref rhs } => {
                let dest = ctx.rename(ident, rhs.scope_id, symbols);
                let expr = rhs.generate_instr(mir_graph, curr_block, ctx, symbols);
                mir_graph[*curr_block].push(Instr::Set{dest, expr});
                Val::Nothing
            }
            AstKind::FunDec{ .. } => Val::Nothing, // the function declaration itself has no runtime behavior
            AstKind::StructDec {..} => Val::Nothing, //  declarations have no runtime behavior
        }
    }
}

impl AstBlock {
    fn generate_instr(&self, mir_graph: &mut MirGraph, curr_block: &mut NodeIndex, ctx: &mut Context, symbols: &SymbolTable) -> Val {
        let mut readable = Val::Nothing;
        for statement in self.0.iter() {
            readable = statement.generate_instr(mir_graph, curr_block, ctx, symbols);
        }
        readable
    }
}
/// Returns the info we need to call the specified subroutine
/// creating the blocks if necessary
fn find_or_create_subroutine<'a>(mir_graph: &mut MirGraph, symbol_id: &SymbolId, ctx: &mut Context, symbols: &'a SymbolTable) -> &'a SubroutineInfo {
    // lookup our function declaration
    let fun_dec = symbols.get_fun_by_id(symbol_id).expect(&f!("unbound function with symbol id {symbol_id}"));
    if let Some(subrtn_info) = fun_dec.maybe_mir() {
        return subrtn_info;
    }
    // create the starting block for this subroutine
    let start = MirBlock::create_block(mir_graph, 0);
    // add some debugging information to the mir block
    mir_graph[start].add_tag(&format!("subroutine {:?}", symbol_id));

    // create the dummy ending block for this routine
    let end = MirBlock::create_block_at_same_depth(mir_graph, &start);
    // extract the mir values for our parameters
    let args = fun_dec.params.iter().map(|param| {
        ctx.rename(param, fun_dec.scope_id(), symbols)
    }).collect::<Vec<VarblId>>();
    // create our return value
    let dest = ctx.make_intermediate();
    // set the mir now. this is so self referential (i.e. recursive) functions can use this information
    // and prevent find_or_create_subroutine from recursing forever
    fun_dec.set_mir(SubroutineInfo{ start, end, params: args, return_val: Val::Varbl(dest) });

    // we can find the actual ending block by passing it into generate_instr
    let mut actual_end = start;
    // translate the body of the function
    let actual_return = fun_dec.body.borrow().generate_instr(mir_graph, &mut actual_end, ctx, symbols);
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
    let mut ctx = Context::new();
    let entry_block = graph.add_node(MirBlock::entry_point());
    let mut exit_block = entry_block; // this will be the exit block after all the blocks are translated
    for node in ast_nodes {
        node.generate_instr(&mut graph, &mut exit_block, &mut ctx, symbols);
    }
    // the program returns to it's caller at the end of the main function
    graph[exit_block].set_exit_strategy(ExitStrategy::Ret);
    Mir::from(graph, ctx.precoloring, entry_block, exit_block)
}
