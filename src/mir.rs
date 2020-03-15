use crate::symbols::SymbolId;

#[derive(Debug, Copy, Clone)]
pub enum Val {
    Varbl(usize), // a variable value and it's ID
    Const(i32), // a constant
    Nothing, // no value
}

#[derive(Debug, Clone)]
pub enum Instr {
    Add     {dest: Val, a: Val, b: Val},      // store a + b in dest
    Sub     {dest: Val, a: Val, b: Val},      // store a - b in dest
    Equals  {dest: Val, a: Val, b: Val},      // store a == b in dest
    Set     {dest: Val, expr: Val},           // store expr in dest
    CallFun {dest: Val, symbol_id: SymbolId, args: Vec<Val>}, // call the function at the specified symbol id
    Cond    {test: Val, invert: bool, body: Vec<Instr>},    // executes the body only if the value `test` is non-zero
    Loop    {test: Val, body: Vec<Instr>},    // loops over the body while the value `test` is non-zero
    Print   (Val),                            // prints out the value
}
