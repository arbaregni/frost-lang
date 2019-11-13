use crate::mir::{MirInstr, Loc, MirReadable};
use std::fmt::{Formatter, Error};

const STACK_PTR: &'static str = "theta";
const STACK_NAME: &'static str = "lSTACK";
const EOL: char = '\n';

pub struct Program {
    lines: Vec<String>,
}

impl Program {
    pub fn empty() -> Program {
        Program {
            lines: Vec::new()
        }
    }
    pub fn generate(instrs: &[MirInstr]) -> Program {
        let mut prgm = Program::empty();
        for instr in instrs {
            instr.generate_code(&mut prgm);
        }
        prgm
    }
    pub fn new_line(&mut self, mut line: String) {
        self.lines.push(line);
    }
    pub fn get_code(&self) -> String {
        let mut code = String::new();
        for line in self.lines.iter() {
            code.push_str(line);
            code.push(EOL);
        }
        code
    }
}
impl std::fmt::Display for Loc {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match *self {
            Loc::Register(ref reg) => write!(f, "{}", reg),
            Loc::StackAbsolute(n) => write!(f, "{}({})", STACK_NAME, n),
            Loc::StackRelative(n) => write!(f, "{}({}-{})", STACK_NAME, STACK_PTR, n),
        }
    }
}
impl std::fmt::Display for MirReadable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match *self {
            MirReadable::Void => write!(f, ""),
            MirReadable::At(ref loc) => write!(f, "{}", loc),
            MirReadable::Val(ref val) => write!(f, "{}", val),
            MirReadable::Op {ref infix, ref lhs, ref rhs} => {
                write!(f, "({} {} {})", lhs, infix, rhs)
            },
        }
    }
}

impl MirInstr {
    pub fn generate_code(&self, prgm: &mut Program) {
        match *self {
            MirInstr::WriteTo{ref address, ref expr} => {
                prgm.new_line(f!("{address} <- {expr}"));
            }
            MirInstr::PushStack{ref expr} => {
                prgm.new_line(f!("{STACK_NAME}({STACK_PTR}) <- {expr}"));
                prgm.new_line(f!("{STACK_PTR} <- {STACK_PTR} + 1"));
            }
            MirInstr::ReduceStack{ref amount} => {
                prgm.new_line(f!("{STACK_PTR} <- {STACK_PTR} - {amount}"));
            }
            MirInstr::Output{ref expr} => {
                prgm.new_line(f!("Disp {expr}"))
            }
        }
    }
}