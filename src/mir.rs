#[derive(Debug, Clone)]
pub enum Loc {
    /// the value at a specific register
    Register(String),
    /// the value at a specific stack location
    StackAbsolute(usize),
    /// the value some number of slots below the top of the stack
    StackRelative(usize),
}

#[derive(Debug, Clone)]
pub enum MirReadable {
    // read from a location
    At(Loc),
    // read a constant
    Val(String),
    // read from an evaluated expression
    Op{infix: String, lhs: Box<MirReadable>, rhs: Box<MirReadable>},
    // not a value to be read
    Void,
}


/// Medium Internal Representation Instruction
#[derive(Debug)]
pub enum MirInstr {
    WriteTo{address: Loc, expr: MirReadable},
    PushStack{expr: MirReadable},
    ReduceStack{amount: MirReadable},
    Output{expr: MirReadable},
}