extern crate pest;
#[macro_use] extern crate pest_derive;
#[macro_use] extern crate fstrings;

#[macro_use] mod util;
mod error;
mod functions;
mod parse;
mod ast;
mod type_inference;
mod symbols;
mod scope;
mod mirgen;
mod mir;
mod codegen;

use std::fs;
use crate::symbols::SymbolTable;
use crate::mirgen::create_mir;
use crate::type_inference::{type_check, Type};
use crate::error::Error;
use crate::scope::ScopeId;
use petgraph::dot::{Dot, Config};

/// # Example
/// struct Point {
///     x: Num;
///     y: Num;
/// }
/// t = Point { x = 1; y = 2 };

fn compile(source: &str) -> Result<String, Error> {
    let mut symbol_table = SymbolTable::new();

    let mut ast_nodes= parse::parse(&source, &mut symbol_table)?;

    println!("scanning symbols...");

    let mut symbol_table = SymbolTable::scan_symbols(&mut ast_nodes);

    // ============BINDING INTRINSICS==============
    let add_fun = functions::FunDec::new_intrinsic(
        vec!["a".to_string(), "b".to_string()],
        functions::FunType{
            in_types: vec![Type::Int, Type::Int],
            out_type: Box::new(Type::Int),
        },
    );
    symbol_table.bind_fun("add", ScopeId::default(), add_fun);

    let sub_fun = functions::FunDec::new_intrinsic(
        vec!["a".to_string(), "b".to_string()],
        functions::FunType{
            in_types: vec![Type::Int, Type::Int],
            out_type: Box::new(Type::Int),
        },
    );
    symbol_table.bind_fun("sub", ScopeId::default(), sub_fun);

    let equals_fun = functions::FunDec::new_intrinsic(
        vec!["a".to_string(), "b".to_string()],
        functions::FunType {
            in_types: vec![Type::Int, Type::Int],
            out_type: Box::new(Type::Boole),
        },
    );
    symbol_table.bind_fun("equals", ScopeId::default(), equals_fun);

    let print_fun = functions::FunDec::new_intrinsic(
        vec!["a".to_string()],
        functions::FunType{
            in_types: vec![Type::Int],
            out_type: Box::new(Type::Void),
        },
    );
    symbol_table.bind_fun("print", ScopeId::default(), print_fun);

//    println!("symbols: {:#?}", symbol_table);
//    println!("ast: {:#?}", ast_nodes);

    println!("typechecking...");

    type_check(&ast_nodes, &mut symbol_table)?;

    println!("lowering to mir....");

    let mir = create_mir(&ast_nodes, &mut symbol_table);

    println!("{}", Dot::with_config(
        &mir.graph,
        &[Config::EdgeNoLabel, ]
    ));



    let compiled = panic!("TODO: compile mir"); // codegen::compile::<codegen::MipsProgram>(&mut symbol_table, mir_instrs);

    Ok(compiled)
}

fn main() {
    let path = r"C:\Users\james\Projects\basic-transpilation-prime\test.txt";
    let source = fs::read_to_string(path).expect("failed to read file");

    let compiled = match compile(&source) {
        Ok(compiled) => compiled,
        Err(mut err) => {
            err.make_underlined(&source);
            eprintln!("{}", err);
            return;
        }
    };

    println!("{}", compiled);
}
