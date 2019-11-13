#[macro_use] extern crate pest;
#[macro_use] extern crate pest_derive;
#[macro_use] extern crate fstrings;

use crate::mir::{MirInstr, Loc, MirReadable};
use crate::symbols::SymbolTable;
use std::fs;
use std::error::Error;
use crate::codegen::{Program};
use crate::mirgen::create_mir_instrs;


mod util;
mod error;
mod parse;
mod ast;
mod symbols;
mod mirgen;
mod mir;
mod codegen;

/// # Example
/// struct Point {
///     x: Num;
///     y: Num;
/// }
/// t = Point { x = 1; y = 2 };
/// --snip--
/// product = t.x * t.y;
/// # Symbol Table
/// Name      Type                  Destination
/// Point     TypeExpr              -----
/// t         StructType{Num,Num}   reg A
/// product   Num                   reg B
/// # Generated Code
/// A <- theta              ; alloc 2
/// theta <- theta + 2      ;
/// STACK(A) <- 1.0           ; struct init
/// STACK(A+1) <- 2.0         ;
/// B <- STACK(A) * STACK(B)    ; assignment evaluation

fn main() {
    let path = r"C:\Users\james\Projects\basic-transpilation-prime\test.txt";
    let source = fs::read_to_string(path).expect("failed to read file");
    let ast_nodes = match parse::parse(&source) {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("{}", err);
            return
        }
    };

    println!("{:#?}", ast_nodes);

    let mut symbol_table = match SymbolTable::scan_symbols(&ast_nodes) {
        Ok(symbol_table) => symbol_table,
        Err(err) => {
            eprintln!("{}", err);
            return
        }
    };



    let _ = symbol_table.assign_locations();

    println!("{:#?}", symbol_table);

    let instrs = create_mir_instrs(&ast_nodes, &mut symbol_table);

    println!("{:#?}", instrs);

    let mut prgm = Program::generate(&instrs);

    println!("{}", prgm.get_code());
}