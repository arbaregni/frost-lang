use crate::mir;
use crate::mir::Val;
use crate::symbols::{SymbolId, SymbolTable};
use std::collections::HashMap;
use crate::functions::FunDec;
use std::ops::Deref;

pub fn compile<Target>(symbol_table: &mut SymbolTable, mut mir_instrs: Vec<mir::Instr>) -> String
  where Target: Program
{
    let mut prgm = Target::new();
    prgm.pre_process(symbol_table, &mut mir_instrs);
    for instr in &mir_instrs {
        prgm.compile_instr(instr);
    }
    prgm.post_process();
    prgm.to_string()
}

pub trait Program {
    fn new() -> Self;
    fn pre_process(&mut self, symbol_table: &mut SymbolTable, instrs: &mut [mir::Instr]);
    fn compile_instr(&mut self, instr: &mir::Instr);
    fn post_process(&mut self);
    fn to_string(&self) -> String;
}

pub struct MipsProgram {
    label_counter: usize,
    labels_by_symbol: HashMap<SymbolId, String>,
    active_save_regs: Vec<String>,
    active_temp_regs: Vec<String>,
    text: String
}

macro_rules! write_label {
    ($mips_program:expr, $label:expr) => {
        $mips_program.text.push_str(&format!("{}:\n", $label));
    }
}
macro_rules! make_instr {
    ($mips_program:expr, $prefix:expr, $dest:expr, $offset:expr, ( $reg:expr )) => {
        $mips_program.text.push_str(&format!("    {} {},{}({})\n", $prefix, $dest, $offset, $reg))
    };
    ($mips_program:expr, $prefix:expr) => {
        $mips_program.text.push_str(&format!("    {}\n", $prefix))
    };
    ($mips_program:expr, $prefix:expr, $reg:expr) => {
        $mips_program.text.push_str(&format!("    {} {}\n", $prefix, $reg))
    };
    ($mips_program:expr, $prefix:expr, $dest:expr, $reg:expr) => {
        $mips_program.text.push_str(&format!("    {} {},{}\n", $prefix, $dest, $reg))
    };
    ($mips_program:expr, $prefix:expr, $dest:expr, $reg0:expr, $reg1:expr) => {
        $mips_program.text.push_str(&format!("    {} {},{},{}\n", $prefix, $dest, $reg0, $reg1))
    };

}

impl Program for MipsProgram {
    fn new() -> Self {
        Self {
            label_counter: 0,
            labels_by_symbol: HashMap::new(),
            active_save_regs: Vec::new(),
            active_temp_regs: Vec::new(),
            text: String::new(),
        }
    }
    fn pre_process(&mut self, symbol_table: &mut SymbolTable, _instrs: &mut [mir::Instr]) {
        // compile all of the subroutines
        for (symbol_id, fun_dec) in symbol_table.subroutine_iter() {
            // create the label for this subroutine
            let lbl = self.make_label();
            // bind the label in the table so we can find it later on
            self.labels_by_symbol.insert(symbol_id, lbl.clone());
            // compile its mir instructions
            self.compile_subroutine(symbol_id, fun_dec.deref());
        }

        // prepare for the main function
        self.text.push_str("    .text\n    .globl main\n");
        write_label!(self, "main");
    }
    fn compile_instr(&mut self, instr: &mir::Instr) {
        use mir::Val::*;
        use mir::Instr::*;
        match instr {
            Add{dest, a, b} => {
                match (a, b) {
                    (v1 @ Varbl(_), v2 @ Varbl(_)) => {
                        make_instr!(self, "add", &self.val_str(&dest), &self.val_str(&v1), &self.val_str(&v2));
                    }
                    (v @ Varbl(_), c @ Const(_)) | (c @ Const(_), v  @ Varbl(_)) => {
                        make_instr!(self, "addi",&self.val_str(&dest),&self.val_str(&v),&self.val_str(&c));
                    }
                    (Const(n), Const(m)) => {
                        make_instr!(self, "li",&self.val_str(&dest),&format!("{}", n+m));
                    }
                    (val, Nothing) | (Nothing, val) => {
                        self.set_reg(&self.val_str(dest), val);
                    }
                }
            }
            Sub{dest, a, b} => {
                match (a, b) {
                    (v1 @ Varbl(_), v2 @ Varbl(_)) => {
                        make_instr!(self, "sub",&self.val_str(&dest),&self.val_str(&v1),&self.val_str(&v2));
                    }
                    (v @ Varbl(_), Const(c)) => {
                        make_instr!(self, "addi",&self.val_str(&dest),&self.val_str(&v),&format!("{}", -c));
                    }
                    (Const(c), v  @ Varbl(_)) => {
                        // get c - v
                        make_instr!(self, "addi",&self.val_str(&dest),&self.val_str(&v),&format!("{}", -c));
                        // find the negative
                        make_instr!(self, "sub", &self.val_str(&dest), "$zero", &self.val_str(&dest));
                    }
                    (Const(n), Const(m)) => {
                        make_instr!(self, "li",&self.val_str(&dest),&format!("{}", n-m));
                    }
                    (_, Nothing) | (Nothing, _) => unreachable!(),
                }
            }
            Equals{dest, a, b} => {
                // first, take the difference of a and b, then we will negate it
                self.compile_instr(&mir::Instr::Sub{dest: *dest, a: *a, b: *b});
                let valstr = self.val_str(&dest);
                make_instr!(self, "nor",&valstr, &valstr, &valstr);
            }
            Set{dest, expr} => {
                self.set_reg(&self.val_str(&dest), &expr);
            }
            Print(val) => {
                // assume that val is an integer for now
                make_instr!(self, "li", "$v0", "1"); // prepare for syscall 1 (print int)
                self.set_reg("$a0", &val);
                make_instr!(self, "syscall");
            }
            Cond{test, invert, body} => {
                match test {
                    Varbl(_) => {
                        // request a label to jump to on false
                        let lbl = self.make_label();
                        // a regular condition jumps over its body if it finds zero
                        // an inverted condition jumps over its body on non-zero values
                        let prefix = if *invert { "bne" } else { "beq" };
                        // create an instruction of the form:
                        // beq $t0,$zero,Label
                        make_instr!(self, prefix, "$zero", &self.val_str(&test), &lbl);
                        // write the body of the if statement
                        for instr in body {
                            self.compile_instr(instr);
                        }
                        write_label!(self, lbl);
                    }
                    Const(c) if (*c != 0) ^ invert => {
                        // non-zero constant (or zero constant and we inverted): must execute
                        for inst in body {
                            self.compile_instr(inst);
                        }
                    }
                    Const(_) | Nothing => { /* no - op */ }
                }
            }
            CallFun{dest, symbol_id, args} => {
                let mut idx = 0;
                // save the things that need to be saved by the caller
                for temp_reg in self.active_temp_regs.iter() {
                    make_instr!(self, "sw", temp_reg, idx, ( "$sp" ));
                }
                // caller must always save the return address
                make_instr!(self, "sw", "$ra", idx, ( "$sp" ));
                idx += 1;

                // move the stack pointer down
                make_instr!(self, "addi", "$sp", "$sp", -idx);

                // jump and link to the subroutine
                let lbl = &self.labels_by_symbol[symbol_id];
                make_instr!(self, "jal", lbl);

                // move the stack pointer back up
                make_instr!(self, "addi", "$sp", "$sp", idx);

                // recover return address
                idx -= 1;
                make_instr!(self, "lw", "$ra", idx, ( "$sp" ));
            }
            _ => {
                println!("warning! unrecognized instr {:?}", instr);
            }
        }
    }
    fn post_process(&mut self) {
        make_instr!(self, "jr", "$ra"); // return to caller
    }
    fn to_string(&self) -> String {
        self.text.clone()
    }
}

impl MipsProgram {
    fn make_label(&mut self) -> String {
        let lbl = format!("L{}", self.label_counter);
        self.label_counter += 1;
        lbl
    }
    fn val_str(&self, val: &mir::Val) -> String {
        match val {
            mir::Val::Varbl(ref idx) => format!("$t{}", *idx),
            mir::Val::Const(ref num) => format!("{}", *num),
            mir::Val::Nothing => String::new(), // no-op
        }
    }
    fn set_reg(&mut self, dest: &str, value: &Val) {
        let prefix = match value {
            Val::Varbl(_) => "move",
            Val::Const(_) => "li",
            Val::Nothing  => return, // no - op
        };
        make_instr!(self, prefix, &dest, &self.val_str(&value));
    }
    fn compile_subroutine(&mut self, symbol_id: SymbolId, fun_dec: &FunDec) {
        // compile the actual body of the subroutine, using whatever registers we want to use
        let mut subroutine = MipsProgram::new();
        // lookup all the arguments for this subroutine
        for param in fun_dec.params.iter() {
            let symbol_id = 0;
            dbg!( param );
        }
        for instr in fun_dec.mir().instrs.iter() {
            subroutine.compile_instr(instr);
        }
        // find our label and begin the block of the subroutine in the main file
        let lbl = &self.labels_by_symbol[&symbol_id];
        write_label!(self, lbl);

        // of the registers that we used, save the ones that are supposed to be saved
        let mut idx = 0;
        for saved_reg in subroutine.active_save_regs.iter() {
            make_instr!(self, "sw", saved_reg, idx, ( "$sp" ));
            idx += 1;
        }
        if idx != 0 {
            // move the stack pointer down
            make_instr!(self, "$sp", "$sp", -idx);
        }

        // paste in the body of the subroutine
        self.text.push_str(&subroutine.text);

        println!("returning: {:?}", self.val_str(&fun_dec.mir().returns));

        if idx != 0 {
            // move the stack pointer back up
            make_instr!(self, "addi", "$sp", "$sp", idx);
        }

        // return to the caller
        make_instr!(self, "jr", "$ra");
        self.text.push('\n');
    }
}

