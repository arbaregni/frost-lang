use crate::ast::{Ast, AstKind, AstBlock};
use crate::symbols::{SymbolTable, SymbolId};
use std::collections::HashMap;
use std::fmt::{Formatter, Error};
use std::rc::Rc;

#[derive(Debug, Copy, Clone, Default)]
pub struct ScopeId(usize);
impl std::fmt::Display for ScopeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "scope{}", self.0)
    }
}
#[derive(Debug)]
pub struct Scope {
    parent_id: Option<ScopeId>, // the index of our parent inside the symbol table
    bound_symbols: HashMap<String, SymbolId>, // names of variables -> symbol ids
}
impl Scope {
    pub fn new_global() -> Self {
        Scope { parent_id: None, bound_symbols: HashMap::new() }
    }
    pub fn new_child(parent: ScopeId) -> Self {
        Scope { parent_id: Some(parent), bound_symbols: HashMap::new() }
    }
    /// return Ok(ident's symbol id) or Err(our parent_id)
    pub fn lookup(&'_ self, ident: &str) -> Result<&'_ SymbolId, Option<ScopeId>> {
        self.bound_symbols
            .get(ident)
            .ok_or(self.parent_id)
    }
    pub fn bind(&mut self, ident: &str, id: SymbolId) {
        // todo return error if symbol is already bound
        self.bound_symbols.insert(ident.to_string(), id);
    }
}

#[derive(Debug)]
pub struct ScopeTable {
    scopes: Vec<Scope>
}
impl ScopeTable {
    /// Create a new scope table
    pub fn new() -> Self {
        ScopeTable {
            scopes: vec![Scope::new_global()]
        }
    }
    /// Returns the id of the global scope
    pub fn global_scope(&self) -> ScopeId {
        ScopeId(0)
    }
    /// Returns the symbol id of the identifier in that scope
    pub fn get_id(&'_ self, ident: &str, scope_id: ScopeId) -> Option<&'_ SymbolId> {
        let mut curr_scope = scope_id;
        loop {
            match self.scopes[curr_scope.0].lookup(ident) {
                Ok(ref symbol_id) => {
                    return Some(symbol_id);
                }
                Err(Some(parent_id)) => {
                    curr_scope = parent_id;
                }
                Err(None) => {
                    return None; // could not find symbol
                }
            }
        }
    }
    /// returns scope id of child which is created to the parent
    pub fn create_subscope(&'_ mut self, parent_id: ScopeId) -> ScopeId {
        let id = ScopeId(self.scopes.len());
        let scope = Scope::new_child(parent_id);
        self.scopes.push(scope);
        id
    }
    /// Binds the identifier in the given scope
    pub fn bind(&mut self, ident: &str, scope_id: ScopeId, symbol_id: SymbolId){
        self.scopes[scope_id.0].bind(ident, symbol_id);
    }
    /// Searches all scopes for the name of this symbol id
    #[allow(dead_code)]
    pub fn recover_ident(&self, symbol_id: SymbolId) -> Option<&str> {
        for scope in self.scopes.iter() {
            for (ident, id) in scope.bound_symbols.iter() {
                if *id == symbol_id {
                    return Some(ident.as_str());
                }
            }
        }
        None
    }
}
impl Ast {
    // scope_id: identifies the scope that this Ast node belongs in
    //            (increments when assignments want to put postceding statements in a subscope)
    // top_level_id: identifies the scope where declarations should be hoisted to
    pub fn scan_symbols(&mut self, scope_id: &mut ScopeId, top_level_id: ScopeId, symbols: &mut SymbolTable) {
        // correct the scope id
        self.scope_id = *scope_id;
        match self.kind {
              AstKind::Int(_) | AstKind::Real(_) | AstKind::Boole(_) | AstKind::String(_)
            | AstKind::TypeExpr { .. } | AstKind::Ident(_) => {},
            AstKind::Block(ref mut block) => block.scan_symbols(scope_id, top_level_id, symbols),
            AstKind::IfStmnt { ref mut test, ref mut if_branch, ref mut else_branch } => {
                test.scan_symbols(scope_id, top_level_id, symbols);
                if_branch.scan_symbols(scope_id, top_level_id, symbols);
                else_branch.scan_symbols(scope_id, top_level_id, symbols);
            }
            AstKind::Assign { ref ident, opt_type: _, ref mut rhs } => {
                let subscope_id = symbols.scope_table.create_subscope(*scope_id);
                // advance the scope_id
                *scope_id = subscope_id;
                // an example of why this is necessary
                // ```
                //   x = 4;    <--------  creates a subscope
                //   print(x);  -------+
                //   print("hello");   |  these statements need to be in that subscope
                //                     v
                // ```
                symbols.bind_var(ident, *scope_id);
                rhs.scan_symbols(scope_id, top_level_id, symbols);
            },
            AstKind::FunCall { func:_, ref mut args } => {
                for arg in args {
                    arg.scan_symbols(scope_id, top_level_id, symbols);
                }
            }
            AstKind::StructDec{ ref ident, .. } => {
                symbols.bind_struct(ident, *scope_id);
            }
            AstKind::FunDec{ref ident, ref mut fun_dec } => {
                let fun_id = symbols.introduce_fun_dec(Rc::clone(fun_dec));
                symbols.scope_table.bind(ident, top_level_id, fun_id);
                // TODO: functions can inherit from parent scopes
                let parent_id = symbols.scope_table.global_scope();
                let subscope_id = symbols.scope_table.create_subscope(parent_id);
                fun_dec.set_scope_id(subscope_id);
                symbols.scope_table.bind(ident, subscope_id, fun_id);
                for param in fun_dec.params.iter() {
                    symbols.bind_var(param.as_str(), subscope_id);
                }

                // both the top level scope and the scope start as the function's sub-scope
                let mut subscope_id_copy = subscope_id;
                fun_dec.body.borrow_mut().scan_symbols(&mut subscope_id_copy, subscope_id, symbols);
            },
        }
    }
}

impl AstBlock {
    fn scan_symbols(&mut self, scope_id: &mut ScopeId, top_level_id: ScopeId, symbols: &mut SymbolTable) {
        for statement in self.0.iter_mut() {
            statement.scan_symbols(scope_id, top_level_id, symbols);
        }
    }
}