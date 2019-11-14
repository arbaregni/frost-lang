use crate::ast::Ast;
use crate::mir::{MirInstr, MirReadable};
use crate::symbols::SymbolTable;

impl Ast {
    pub fn generate_instr(&self, instrs: &mut Vec<MirInstr>, symbols: &mut SymbolTable) -> MirReadable {
        match *self {
            Ast::Int(v) => MirReadable::Val(f!("{v}")),
            Ast::Real(v) => MirReadable::Val(f!("{v}")),
            Ast::String(ref v) => MirReadable::Val(f!("{v}")),
            Ast::Ident(ref key) => MirReadable::At(symbols.get_loc(key)),
            Ast::TypeExpr(_) => unimplemented!(),

            Ast::FnCall { ref func, ref args } => {
                let mut mir_exprs = vec![];
                for arg in args.iter() {
                    mir_exprs.push(arg.generate_instr(instrs, symbols));
                }
                // deal with builtins
                match func.0.as_str() {
                    "add" => {
                        MirReadable::Op {
                            infix: "+".to_string(),
                            lhs: Box::new(mir_exprs[0].clone()),
                            rhs: Box::new(mir_exprs[1].clone())
                        }
                    }
                    "print" => {
                        instrs.push(MirInstr::Output{expr: mir_exprs[0].clone()});
                        MirReadable::Void
                    }
                    _ => unimplemented!(),
                }
            }
            Ast::Assign{ ref ident, opt_type: _, ref rhs } => {
                let address = symbols.get_loc(ident);
                let expr = rhs.generate_instr(instrs, symbols);
                instrs.push(MirInstr::WriteTo{ address, expr });
                MirReadable::Void
            }
            Ast::StructDec(_, _) => unimplemented!(),
        }
    }
}

pub fn create_mir_instrs(ast_nodes: &[Ast], symbols: &mut SymbolTable) -> Vec<MirInstr> {
    let mut instrs = vec![];
    for node in ast_nodes {
        node.generate_instr(&mut instrs, symbols);
    }
    instrs
}