use std::fmt::Formatter;
use std::collections::HashMap;
use crate::error::Error;
use crate::ast::{Ast, Ident};
use crate::mir::Loc;
use crate::functions::FnType;
use crate::type_inference::Type;
use std::collections::hash_map::Entry;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SymbolId(usize);
impl std::fmt::Display for SymbolId {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "id{}", self.0)
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    counter: usize,
    symbols: HashMap<Ident, SymbolId>,
    types: HashMap<SymbolId, Type>,
    locations: HashMap<SymbolId, Loc>,

    fn_decl: HashMap<Ident, FnType>,
}
impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            counter: 0,
            symbols: HashMap::new(),
            types: HashMap::new(),
            locations: HashMap::new(),
            fn_decl: HashMap::new(),
        }
    }
    pub fn scan_symbols(ast_nodes: &[Ast]) -> SymbolTable {
        let mut symbol_table = SymbolTable::new();
        for ast_node in ast_nodes.iter() {
            ast_node.scan_symbols(&mut symbol_table);
        }
        symbol_table
    }
    fn next_id(&mut self) -> SymbolId {
        self.counter += 1;
        SymbolId(self.counter - 1)
    }
    fn get_id(&self, ident: &Ident) -> &SymbolId {
        self.symbols
            .get(ident)
            .expect(&f!("Identifier {ident} not found"))
    }
    pub fn bind(&mut self, key: &Ident) {
        let id = self.next_id();
        self.symbols.insert(key.clone(), id);
    }

    pub fn get_loc(&self, ident: &Ident) -> Loc {
        self.locations
            .get(self.get_id(ident))
            .expect(&f!("No location for key {ident}"))
            .clone()
    }
    pub fn get_type(&'_ self, name: &Ident) -> Option<&'_ Type> {
        self.types
            .get(self.get_id(name))
    }
    pub fn get_type_entry(&'_ mut self, ident: &Ident) -> Entry<'_, SymbolId, Type> {
        self.types.entry(self.get_id(ident).clone())
    }

    pub fn find_function(&self, ident: &Ident) -> FnType {
        FnType{ in_types: vec![Type::Int], out_type: Box::new(Type::Void) }
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
              Ast::Int(_) | Ast::Real(_) | Ast::String(_)
            | Ast::TypeExpr { .. } | Ast::Ident(_) | Ast::FnCall { .. } => {},
            Ast::Assign { ref ident, ref opt_type, ref rhs } => {
                // ideally, we would bind in the subscope
                symbols.bind(ident);
                rhs.scan_symbols(symbols);
            },
            Ast::StructDec(_, _) => unimplemented!(),
        }
    }
}