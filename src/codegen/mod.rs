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
                match (a, b) {
                    (Varbl(a), Varbl(b)) => {
                        make_instr!(self, "add", self.reg_alloc.get(dest), self.reg_alloc.get(a), self.reg_alloc.get(b));
                    }
                    (Varbl(a), Const(n)) | (Const(n), Varbl(a)) => {
                        make_instr!(self, "addi", self.reg_alloc.get(dest), self.reg_alloc.get(a), n);
                    }
                    (Const(n), Const(m)) => {
                        make_instr!(self, "li", self.reg_alloc.get(dest), n + m);
                    }
                    (val, Nothing) | (Nothing, val) => {
                        set_reg!(self, self.reg_alloc.get(dest), val);
                    }

                    #[allow(unreachable_patterns)]
                    (_, Varbl(_)) | (_, Const(_)) => { unreachable!("these should be covered by previous cases.\
                                                                     intelliJ bug: the match *is* exhaustive.")}
                }
            }
            Sub{dest, a, b} => {
                match (a, b) {
                    (Varbl(a), Varbl(b)) => {
                        make_instr!(self, "sub", self.reg_alloc.get(dest), self.reg_alloc.get(a), self.reg_alloc.get(b));
                    }
                    (Varbl(a), Const(n)) if *n != 0 => {
                        make_instr!(self, "addi", self.reg_alloc.get(dest), self.reg_alloc.get(a), -n);
                    }
                    (val, Const(0)) => {
                        set_reg!(self, self.reg_alloc.get(dest), val);
                    }
                    (Const(n), Varbl(a)) => {
                        // get c - v
                        make_instr!(self, "addi", self.reg_alloc.get(dest), self.reg_alloc.get(a), -n);
                        // find the negative
                        make_instr!(self, "sub", self.reg_alloc.get(dest), "$zero", self.reg_alloc.get(dest));
                    }
                    (Const(n), Const(m)) => {
                        make_instr!(self, "li", self.reg_alloc.get(dest), n - m);
                    }
                    (_, Nothing) => { /* nothing to do */ }
                    (_, Varbl(_)) | (_, Const(_)) => { unreachable!("these should be covered by previous cases.\
                                                                     intellIJ bug: the match *is* exhaustive.")}
                }
            }
            Equals{dest, a, b} => {
                let loc = self.reg_alloc.get(dest);
                // first, take the xor of a and b, then we will negate it
                match (a, b) {
                    (Varbl(a), Varbl(b)) => {
                        make_instr!(self, "seq", loc, self.reg_alloc.get(a), self.reg_alloc.get(b))
                    },
                    (Varbl(v), Const(c)) | (Const(c), Varbl(v)) => {
                        make_instr!(self, "seq", loc, self.reg_alloc.get(v), c)
                    },
                    (Const(n), Const(m)) => {
                        make_instr!(self, "li", loc, if n == m { 1 } else { 0 });
                    }
                    (Nothing, _) | (_, Nothing) => {}
                }
            }
            Set{dest, expr} => {
                set_reg!(self, self.reg_alloc.get(dest), expr)
            }
            Print(val) => {
                // assume that val is an integer for now
                make_instr!(self, "li", "$v0", "1"); // prepare for syscall 1 (print int)
                set_reg!(self, "$a0", val);
                make_instr!(self, "syscall");
            }
            Push(varbl) => {
                let loc = self.reg_alloc.get(varbl);
                make_instr!(self, "addi", "$sp", -WORD_SIZE);
                make_instr!(self, "sw", loc, 0, ( "$sp" ));
            }
            Pop(varbl) => {
                let loc = self.reg_alloc.get(varbl);
                make_instr!(self, "lw", loc, 0, ( "$sp" ));
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
                make_instr!(self, "jr", "$ra");
                return;
            },
            ExitStrategy::AlwaysGoto(idx) => {
                next_block = idx;
            },
            ExitStrategy::Call{ subrtn, after_call } => {
                make_instr!(self, "jal",   mir.graph[subrtn].label(&mut self.label_maker));
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
        write_label!(prgm, lbl);
        // compile every instruction in this block
        for instr in block.iter() {
            prgm.compile_instr(instr);
        }

        prgm.exit_block(mir, block);
    }
    prgm.text
}