mod regalloc;
pub use regalloc::*;
#[macro_use] mod codegen_macros;

use crate::mir::{Val, Mir, Instr, MirBlock, ExitStrategy};
use crate::symbols::{SymbolTable};
use regalloc::{RegisterAllocation};

const WORD_SIZE: i32 = 4;

/// a struct to generate labels as we need them
pub struct LabelMaker {
    counter: u32,
}
impl LabelMaker {
    pub fn new() -> LabelMaker {
        LabelMaker { counter: 0 }
    }
    pub fn make_label(&mut self) -> String {
        let num = self.counter;
        self.counter += 1;
        f!("L{num}")
    }
}

pub struct MipsProgram {
    text: String,
    reg_alloc: RegisterAllocation,
    label_maker: LabelMaker,
}

impl MipsProgram {
    fn new(reg_alloc: RegisterAllocation) -> Self {
        Self {
            text: String::new(),
            reg_alloc,
            label_maker: LabelMaker::new(),
        }
    }
    fn pre_process(&mut self, _mir: &Mir, _symbols: &SymbolTable) {
        // prepare for the main function
        self.text.push_str("    .text\n    .globl main\n");
    }
    fn compile_instr(&mut self, instr: &Instr) {
        use Instr::*;
        use Val::*;
        match instr {
            Add{dest, a, b} => {
                let rd = get_reg!(self, dest);
                match (a, b) {
                    (Varbl(a), Varbl(b)) => {
                        let rs = get_reg!(self, a);
                        let rt = get_reg!(self, b);
                        make_instr!(self, "add", rd, rs, rt);
                    }
                    (val, Const(0)) | (Const(0), val) => {
                        set_reg!(self, rd, val);
                    }
                    (Varbl(a), Const(n)) | (Const(n), Varbl(a)) => {
                        let rs = get_reg!(self, a);
                        make_instr!(self, "addi", rd, rs, n);
                    }
                    (Const(n), Const(m)) => {
                        make_instr!(self, "li", rd, n + m);
                    }
                    _ => unreachable!("invalid inputs to Add: a = {:?}, b = {:?}", a, b)
                }
            }
            Sub{dest, a, b} => {
                let rd = get_reg!(self, dest);
                match (a, b) {
                    (Varbl(a), Varbl(b)) => {
                        let rs = get_reg!(self, a);
                        let rt = get_reg!(self, b);
                        make_instr!(self, "sub", rd, rs, rt);
                    }
                    (val, Const(0)) => {
                        set_reg!(self, rd, val);
                    }
                    (Varbl(a), Const(n)) if *n != 0 => {
                        let rs = get_reg!(self, a);
                        make_instr!(self, "addi", rd, rs, -n);
                    }
                    (Const(n), Varbl(a)) => {
                        // load the constant
                        make_instr!(self, "li", rd, n);
                        // subtract off the second parameter
                        let rt = get_reg!(self, a);
                        make_instr!(self, "sub", rd, rd, rt);
                    }
                    (Const(n), Const(m)) => {
                        make_instr!(self, "li", rd, n - m);
                    }
                    _ => unreachable!("invalid inputs to Sub: a = {:?}, b = {:?}", a, b)
                }
            }
            Equals{dest, a, b} => {
                let rd = get_reg!(self, dest);
                match (a, b) {
                    (Varbl(a), Varbl(b)) => {
                        let rs = get_reg!(self, a);
                        let rt = get_reg!(self, b);
                        make_instr!(self, "seq", rd, rs, rt)
                    },
                    (Varbl(v), Const(imm)) | (Const(imm), Varbl(v)) => {
                        let rs = get_reg!(self, v);
                        make_instr!(self, "seq", rd, rs, imm)
                    },
                    (Const(n), Const(m)) => {
                        let imm = if n == m { 1 } else { 0 };
                        make_instr!(self, "li", rd, imm);
                    }
                    _ => unreachable!("invalid inputs to Equals: a = {:?}, b = {:?}", a, b)
                }
            }
            Set{dest, expr} => {
                let rd = get_reg!(self, dest);
                set_reg!(self, rd, expr)
            }
            Print(val) => {
                // assume that val is an integer for now
                make_instr!(self, "li", "$v0", 1 ; "prepare for syscall 1");
                set_reg!(self, "$a0", val        ; "load argument");
                make_instr!(self, "syscall");
            }
            Push(varbl) => {
                let r = get_reg!(self, varbl);
                make_instr!(self, "addi", "$sp", -WORD_SIZE  ; format!("push {} onto the stack", varbl) );
                make_instr!(self, "sw", r, 0, ( "$sp" ));
            }
            Pop(varbl) => {
                let r = get_reg!(self, varbl);
                make_instr!(self, "lw", r, 0, ( "$sp" )     ; format!("pop {} off the stack", varbl));
                make_instr!(self, "addi", "$sp", WORD_SIZE);
            }
        }
    }
    fn exit_block(&mut self, mir: &Mir, block: &MirBlock) {
        let next_block;
        match block.exit_strategy() {
            ExitStrategy::Undefined => {
                // nothing to do: we just hope that the Mir::Instr's handle it, or that the fall through is correct
                return;
            },
            ExitStrategy::Ret => {
                // return to caller
                make_instr!(self, "jr", "$ra" ; "exit function");
                return;
            },
            ExitStrategy::AlwaysGoto(idx) => {
                next_block = idx;
            },
            ExitStrategy::Call{ subrtn, after_call } => {
                let label = mir.graph[subrtn].label(&mut self.label_maker);
                make_instr!(self, "jal", label);
                next_block = after_call;
            },
            ExitStrategy::Branch { condition, on_zero, on_nonzero  } => {
                match condition {
                    Val::Varbl(v) => {
                        // v can take on anything: we must branch
                        let zero_label = mir.graph[on_zero].label(&mut self.label_maker);
                        let loc = self.reg_alloc.get(&v);
                        make_instr!(self, "beq", loc, "$zero", zero_label);
                        // at this point, we are on the non_zero path (if it was zero, we would have transferred control)
                        next_block = on_nonzero;
                    },
                    Val::Const(0) => {
                        // we are going down the on_zero path
                        next_block = on_zero;
                    },
                    Val::Const(_) => {
                        // we are going down the on_nonzero path
                        next_block = on_nonzero;
                    },
                    Val::Nothing => unreachable!(),
                }
            },
        }
        // figure out if we need to jump to get to the next block,
        // or if we can just carry on sequentially
        if true { // TODO: find an actual way to test this
            let label = mir.graph[next_block].label(&mut self.label_maker);
            make_instr!(self, "j", label);
        } else {

        }

    }
}



pub fn generate_mips(mir: &Mir, symbols: &SymbolTable) -> String
{
    let reg_alloc = regalloc::allocate_registers(mir, symbols);
    println!("{}", reg_alloc);
    let mut prgm = MipsProgram::new(reg_alloc);
    prgm.pre_process(mir, symbols);
    for block in mir.blocks() {
        // print the label
        let lbl = block.label(&mut prgm.label_maker);
        if let Some(comment) = block.tag() {
            write_label!(prgm, lbl ; comment);
        } else {
            write_label!(prgm, lbl);
        }
        // compile every instruction in this block
        for instr in block.iter() {
            prgm.compile_instr(instr);
        }

        prgm.exit_block(mir, block);
    }
    prgm.text
}