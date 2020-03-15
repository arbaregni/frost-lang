use crate::ast::{Ast, AstKind, AstBlock};
use crate::mir::{Instr, Val};
use crate::symbols::SymbolTable;
use crate::functions::MirSubroutine;

impl Ast {
    pub fn generate_instr(&self, instrs: &mut Vec<Instr>, symbols: &mut SymbolTable) -> Val {
        match self.kind {
            AstKind::Int(v) => Val::Const(v),
            AstKind::Real(_v) => unimplemented!(),
            AstKind::String(_) => unimplemented!(),
            AstKind::Boole(v) => Val::Const(if v { 1 } else { 0 }),
            AstKind::Ident(ref name) => symbols.alpha_rename(name, self.scope_id), 
            AstKind::TypeExpr(_) => unimplemented!(),
            AstKind::Block(ref block) => block.generate_instr(instrs, symbols),
            AstKind::IfStmnt{ref test, ref if_branch, ref else_branch} => {
                let dest = symbols.make_intermediate();
                let condition = test.generate_instr(instrs, symbols);
                let mut body = vec![];
                let if_val = if_branch.generate_instr(&mut body, symbols);
                body.push(Instr::Set{dest, expr: if_val});
                instrs.push(Instr::Cond{test: condition, invert: false, body});
                if let Some(branch) = else_branch {
                    let mut body = vec![];
                    let else_val = branch.generate_instr(&mut body, symbols);
                    body.push(Instr::Set{dest, expr: else_val});
                    instrs.push(Instr::Cond{test: condition, invert: true, body});
                }
                dest
            }
            AstKind::FunCall { ref func, ref args } => {
                let dest = symbols.make_intermediate();

                let arg_vals: Vec<Val> = args.iter().map(|arg| arg.generate_instr(instrs, symbols)).collect();
                // hard-code intrinsics for now
                match func.as_str() {
                    "add" => {
                        instrs.push(Instr::Add{dest, a: arg_vals[0], b: arg_vals[1]});
                        return dest;
                    }
                    "sub" => {
                        instrs.push(Instr::Sub{dest, a: arg_vals[0], b: arg_vals[1]});
                        return dest;
                    }
                    "equals" => {
                        instrs.push(Instr::Equals{dest, a: arg_vals[0], b: arg_vals[1]});
                        return dest;
                    }
                    "print" => {
                        instrs.push(Instr::Print(arg_vals[0]));
                        return dest;
                    }
                    _ => { /* not an intrinsic */ }
                }
                let symbol_id = *symbols.scope_table.get_id(func, self.scope_id).expect(&f!("unbound function `{func}`"));
                instrs.push(Instr::CallFun{dest, symbol_id, args: arg_vals});
                dest
            }
            AstKind::Assign{ ref ident, opt_type: _, ref rhs } => {
                println!("generating mir for assign...");
                let dest = symbols.alpha_rename(ident, rhs.scope_id);
                let expr = rhs.generate_instr(instrs, symbols);
                instrs.push(Instr::Set{dest, expr});
                Val::Nothing
            }
            AstKind::FunDec{ ref ident, ref fun_dec} => {
                // find our symbol id
                let symbol_id = *symbols.scope_table.get_id(ident, self.scope_id).expect(&f!("unbound function `{ident}`"));
                // create the mir for this subroutine
                let mut instrs = vec![];
                let returns = fun_dec.body.borrow_mut().generate_instr(&mut instrs, symbols);
                fun_dec.set_mir(instrs, returns);

                Val::Nothing // the function declaration itself has no runtime behavior
            }
            AstKind::StructDec {..} => Val::Nothing, //  declarations have no runtime behavior
        }
    }
}

impl AstBlock {
    fn generate_instr(&self, instrs: &mut Vec<Instr>, symbols: &mut SymbolTable) -> Val {
        let mut readable = Val::Nothing;
        for statement in self.0.iter() {
            readable = statement.generate_instr(instrs, symbols);
        }
        readable
    }
}

pub fn create_mir_instrs(ast_nodes: &[Ast], symbols: &mut SymbolTable) -> Vec<Instr> {
    let mut instrs = vec![];
    for node in ast_nodes {
        node.generate_instr(&mut instrs, symbols);
    }
    instrs
}
