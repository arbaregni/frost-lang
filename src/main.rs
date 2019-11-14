extern crate pest;
#[macro_use] extern crate pest_derive;
#[macro_use] extern crate fstrings;

use crate::symbols::SymbolTable;
use std::fs;
use crate::codegen::{Program};
use crate::mirgen::create_mir_instrs;
use crate::type_inference::{type_check, Type};
use crate::ast::Ident;
use crate::functions::FnType;


mod util;
mod error;
mod functions;
mod parse;
mod ast;
mod type_inference;
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

    let mut symbol_table = SymbolTable::scan_symbols(&ast_nodes);
    let add = Ident("add".to_string());
    symbol_table.bind(&add);
    symbol_table.get_type_entry(&add)
        .or_insert(Type::Fn(FnType{
            in_types: vec![Type::Int, Type::Int],
            out_type: Box::new(Type::Int)
        }));
    let print = Ident("print".to_string());
    symbol_table.bind(&print);
    symbol_table.get_type_entry(&print)
        .or_insert(Type::Fn(FnType{
            in_types: vec![Type::Int],
            out_type: Box::new(Type::Void)
        }));

    println!("{:#?}", symbol_table);

    match type_check(&ast_nodes, &mut symbol_table) {
        Ok(_) => {}, // no error
        Err(err) => {
            eprintln!("{}", err);
            return;
        }
    }





    let _ = symbol_table.assign_locations();

    println!("{:#?}", symbol_table);

    let instrs = create_mir_instrs(&ast_nodes, &mut symbol_table);

    println!("{:#?}", instrs);

    let mut prgm = Program::generate(&instrs);

    println!("{}", prgm.get_code());
}