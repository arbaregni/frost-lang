use std::collections::HashMap;
use crate::ast::{Ident, TypeExpr, Ast};
use ::fstrings;
use crate::mir::Loc;
use crate::error::Error;
use std::ops::Deref;
use std::fmt::Formatter;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct SymbolId(pub usize);
impl std::fmt::Display for SymbolId {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "id{}", self.0)
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    counter: usize,
    idents: HashMap<Ident, SymbolId>,
    types: HashMap<SymbolId, TypeExpr>,
    locations: HashMap<SymbolId, Loc>,
}
impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            counter: 0,
            idents: HashMap::new(),
            types: HashMap::new(),
            locations: HashMap::new(),
        }
    }
    pub fn scan_symbols(ast_nodes: &[Ast]) -> Result<SymbolTable, Error> {
        let mut symbol_table = SymbolTable::new();
        for ast_node in ast_nodes.iter() {
            ast_node.scan_symbols(&mut symbol_table);
        }
        Ok(symbol_table)
    }
    fn next_id(&mut self) -> SymbolId {
        self.counter += 1;
        SymbolId(self.counter - 1)
    }
    fn get_id(&self, key: &Ident) -> &SymbolId {
        self.idents
            .get(key)
            .expect(&f!("Key {key} not found"))
    }
    pub fn bind(&mut self, key: &Ident) {
        let id = self.next_id();
        self.idents.insert(key.clone(), id);
    }

    pub fn get_loc(&self, key: &Ident) -> Loc {
        self.locations
            .get(self.get_id(key))
            .expect(&f!("No location for key {key}"))
            .clone()
    }
    pub fn assign_locations(&mut self) -> usize {
        let mut iter = (b'A'..=b'Z')
            .map(char::from);
        for i in 0..self.counter {
            let loc = Loc::Register(iter.next().expect("ran out of registers").to_string());
            self.locations.insert(SymbolId(i), loc);
        }
        0
    }
}

impl Ast {
    fn scan_symbols(&self, symbols: &mut SymbolTable) {
        match *self {
            Ast::Int(_)
              | Ast::Real(_) | Ast::String(_)
              | Ast::Infix(_, _, _) | Ast::TypeExpr { .. }
              | Ast::Ident(_) | Ast::FnCall { .. } => {},
            Ast::Assign { ref lhs, ref opt_type, ref rhs } => {
                symbols.bind(lhs);
                rhs.scan_symbols(symbols);
            },
            Ast::StructDec(_, _) => unimplemented!(),
        }
    }
}