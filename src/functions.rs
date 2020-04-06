use crate::type_inference::Type;
use crate::ast::{AstBlock};
use crate::mir;
use crate::scope::ScopeId;
use std::cell::RefCell;
use once_cell::unsync::OnceCell;
use petgraph::graph::NodeIndex;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunType {
    pub in_types: Vec<Type>,
    pub out_type: Box<Type>,
}

impl std::convert::Into<Type> for FunType {
    fn into(self) -> Type {
        Type::Fn(self)
    }
}

#[derive(Debug, Clone)]
pub struct FunDec {
    pub params: Vec<String>,
    pub body: RefCell<AstBlock>,
    pub fun_type: FunType,
    maybe_scope_id: OnceCell<ScopeId>,
    /// The internal representation of this function declaration:
    /// The index of the starting block, the ending block, and the return value
    maybe_mir: OnceCell<SubroutineInfo>,
}

impl FunDec {
    pub fn new(params: Vec<String>, body: AstBlock, fun_type: FunType) -> FunDec {
        FunDec {
            params,
            body: RefCell::new(body),
            fun_type,
            maybe_scope_id: OnceCell::new(),
            maybe_mir: OnceCell::new(),
        }
    }
    pub fn new_intrinsic(params: Vec<String>, fun_type: FunType) -> FunDec {
        FunDec::new(params, AstBlock::empty(), fun_type)
    }
    pub fn set_scope_id(&self, scope_id: ScopeId) {
        self.maybe_scope_id.set(scope_id).expect("function scope id already set");
    }
    pub fn scope_id(&self) -> ScopeId {
        *self.maybe_scope_id.get().expect("function missing its scope id")
    }
    pub fn maybe_mir(&self) -> Option<&SubroutineInfo> {
        self.maybe_mir.get()
    }
    pub fn set_mir(&self, subroutine_info: SubroutineInfo) {
        self.maybe_mir.set(subroutine_info).expect("mir subroutine info is already set");
    }
    pub fn has_mir(&self) -> bool {
        self.maybe_mir.get().is_some()
    }

}

#[derive(Debug, Clone)]
pub struct SubroutineInfo {
    pub start: NodeIndex,
    pub end: NodeIndex,
    pub params: Vec<mir::VarblId>,
    pub return_val: mir::Val,
}