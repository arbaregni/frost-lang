use crate::{regalloc};
use crate::mir::{Val, Mir, Instr, VarblId};
use crate::symbols::{SymbolTable};
use crate::regalloc::{RegisterAllocation};

pub struct MipsProgram {
    text: String,
    reg_alloc: RegisterAllocation,
    label_counter: u32,
}

macro_rules! write_label {
    ($prgm:expr, $label:expr) => {
        $prgm.text.push_str(&format!("{}:\n", $label));
    }
}
macro_rules! make_instr {
    ($prgm:expr, $prefix:expr, $dest:expr, $offset:expr, ( $reg:expr )) => {
        $prgm.text.push_str(&format!("    {} {},{}({})\n", $prefix, $dest, $offset, $reg))
    };
    ($prgm:expr, $prefix:expr) => {
        $prgm.text.push_str(&format!("    {}\n", $prefix))
    };
    ($prgm:expr, $prefix:expr, $reg:expr) => {
        $prgm.text.push_str(&format!("    {} {}\n", $prefix, $reg))
    };
    ($prgm:expr, $prefix:expr, $dest:expr, $reg:expr) => {
        $prgm.text.push_str(&format!("    {} {},{}\n", $prefix, $dest,$reg))
    };
    ($prgm:expr, $prefix:expr, $dest:expr, $reg0:expr, $reg1:expr) => {
        $prgm.text.push_str(&format!("    {} {},{},{}\n", $prefix, $dest, $reg0, $reg1))
    };
}

macro_rules! set_reg {
    ($prgm:expr, $reg:expr, $val:expr) => {
        match $val {
            Val::Varbl(varbl) => make_instr!($prgm, "move", $reg, $prgm.reg_alloc.get(varbl)),
            Val::Const(n) => make_instr!($prgm, "li", $reg, *n),
            Val::Nothing => { /* no op */ },
        }
    }
}

impl MipsProgram {
    fn new(reg_alloc: RegisterAllocation) -> Self {
        Self {
            text: String::new(),
            reg_alloc,
            label_counter: 0,
        }
    }
    fn pre_process(&mut self, mir: &Mir, _symbols: &SymbolTable) {
        // prepare for the main function
        self.text.push_str("    .text\n    .globl main\n");
        write_label!(self, "main");
    }
    fn compile_instr(&mut self, instr: &Instr) {
        use Instr::*;
        use Val::*;
        match instr {
            Add{dest, a, b} => {
                match (a, b) {
                    (Varbl(a), Varbl(b)) => {
                        make_instr!(self, "add", self.reg_alloc.get(a), self.reg_alloc.get(b), self.reg_alloc.get(b));
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
                }
            }
            Sub{dest, a, b} => {
                match (a, b) {
                    (Varbl(a), Varbl(b)) => {
                        make_instr!(self, "sub", self.reg_alloc.get(dest), self.reg_alloc.get(a), self.reg_alloc.get(b));
                    }
                    (Varbl(a), Const(n)) => {
                        make_instr!(self, "addi", self.reg_alloc.get(dest), self.reg_alloc.get(a), -n);
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
                    (_, Nothing) | (Nothing, _) => unreachable!(),
                }
            }
            Equals{dest, a, b} => {
                // first, take the difference of a and b, then we will negate it
                self.compile_instr(&Instr::Sub{dest: *dest, a: *a, b: *b});
                let loc = self.reg_alloc.get(dest);
                make_instr!(self, "nor", loc, loc, loc);
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
            _ => {
                println!("warning! unrecognized instr {:?}", instr);
            }
        }
    }
    fn make_label(&mut self) -> String {
        let lbl = format!("L{}", self.label_counter);
        self.label_counter += 1;
        lbl
    }
    fn post_process(&mut self) {
        make_instr!(self, "jr", "$ra"); // return to caller
    }
    fn to_string(&self) -> String {
        self.text.clone()
    }
}


pub fn generate_mips(mir: &Mir, symbols: &SymbolTable) -> String
{
    let reg_alloc = regalloc::allocate_registers(mir, symbols);
    let mut prgm = MipsProgram::new(reg_alloc);
    prgm.pre_process(mir, symbols);
    for (instr, _idx) in mir.iter() {
        prgm.compile_instr(instr);
    }
    prgm.post_process();
    prgm.to_string()
}

