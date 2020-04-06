use crate::ast::Ast;
use crate::type_inference::{Type, Quantified};
use crate::scope::{ScopeId, ScopeTable};
use crate::functions::{FunDec};

use std::fmt::Formatter;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::rc::Rc;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SymbolId(usize);
impl std::fmt::Display for SymbolId {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "id{}", self.0)
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    id_counter: usize,

    pub scope_table: ScopeTable,

    type_table: HashMap<SymbolId, Type>,

    // ---- DECLARATIONS ----
    struct_table: HashMap<SymbolId, ()>,
    fun_table: HashMap<SymbolId, Rc<FunDec>>,
    quantified: HashMap<SymbolId, Quantified>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            id_counter: 0,
            scope_table: ScopeTable::new(),
            type_table: HashMap::new(),
            
            struct_table: HashMap::new(),
            fun_table: HashMap::new(),

            quantified: HashMap::new(),
        }
    }
    pub fn scan_symbols(ast_nodes: &mut [Ast]) -> SymbolTable {
        let mut symbol_table = SymbolTable::new();
        let globl = symbol_table.scope_table.global_scope();
        let mut scope = globl;
        for ast_node in ast_nodes.iter_mut() {
            ast_node.scan_symbols(&mut scope, globl, &mut symbol_table);
        }
        symbol_table
    }
    fn next_id(&mut self) -> SymbolId {
        self.id_counter += 1;
        SymbolId(self.id_counter - 1)
    }

    pub fn bind_var(&mut self, ident: &str, scope_id: ScopeId) -> SymbolId {
        let symbol_id = self.next_id();
        self.scope_table.bind(ident, scope_id, symbol_id);
        symbol_id
    }
    pub fn bind_struct(&mut self, ident: &str, scope_id: ScopeId) {
        println!("binding struct! {}#{}", ident, scope_id);
    }
    pub fn bind_fun(&'_ mut self, ident: &str, scope_id: ScopeId, fun_dec: FunDec) {
        let id = self.bind_var(ident, scope_id);
        self.fun_table.insert(id, Rc::new(fun_dec));
    }
    pub fn bind_quant(&mut self, ident: &str, scope_id: ScopeId, quant: Quantified) {
        let symbol_id = self.bind_var(ident, scope_id);
        self.quantified.insert(symbol_id, quant);
    }
    pub fn get_type(&'_ self, ident: &str, scope_id: ScopeId) -> Option<&'_ Type> {
        let id = self.scope_table.get_id(ident, scope_id)?;
        self.type_table.get(id)
    }
    pub fn type_entry(&'_ mut self, ident: &str, scope_id: ScopeId) -> Entry<'_, SymbolId, Type> {
        let id = self.scope_table.get_id(ident, scope_id).expect(&f!("unbound symbol {ident} in scope {scope_id}"));
        self.type_table.entry(id.clone())
    }
    /// Returns the SymbolId corresponding to the given function declaration
    pub fn introduce_fun_dec(&'_ mut self, fun_dec: Rc<FunDec>) -> SymbolId {
        let symbol_id = self.next_id();
        self.fun_table.insert(symbol_id, fun_dec);
        symbol_id
    }
    pub fn get_fun(&'_ self, name: &str, scope_id: ScopeId) -> Option<&'_ FunDec> {
        let symbol_id = self.scope_table.get_id(name, scope_id)?;
        self.get_fun_by_id(symbol_id)
    }
    pub fn get_fun_by_id(&'_ self, symbol_id: &SymbolId) -> Option<&'_ FunDec> {
        let rc = self.fun_table.get(symbol_id)?;
        Some(rc.as_ref())
    }
    pub fn quant_entry(&'_ mut self, ident: &str, scope_id: ScopeId) -> Entry<'_, SymbolId, Quantified> {
        let id = self.scope_table.get_id(ident, scope_id).expect(&f!("unbound symbol {ident} in scope {scope_id}"));
        self.quantified.entry(id.clone())
    }


}
