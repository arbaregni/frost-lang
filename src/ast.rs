use pest;
use crate::parse;
use crate::type_inference::Type;
use crate::functions::FunDec;
use crate::scope::ScopeId;
use std::rc::Rc;

type Pair<'a> = pest::iterators::Pair<'a, parse::Rule>;

#[derive(Debug, Clone)]
pub struct AstBlock(pub Vec<Ast>);

impl AstBlock {
    pub fn empty() -> AstBlock { AstBlock(Vec::new()) }
}
impl std::convert::From<Vec<Ast>> for AstBlock {
    fn from(nodes: Vec<Ast>) -> AstBlock {
        AstBlock(nodes)
    }
}
impl std::convert::Into<AstKind> for AstBlock {
    fn into(self) -> AstKind {
        AstKind::Block(self)
    }
}

#[derive(Debug, Clone)]
pub enum AstKind {
    Int(i32),
    Real(f32),
    Boole(bool),
    String(String),
    Ident(String),
    TypeExpr(Type),
    Block(AstBlock),
    IfStmnt {test: Box<Ast>, if_branch: Box<Ast>, else_branch: Box<Ast>},
    FunCall {func: String, args: Vec<Ast> },
    Assign {ident: String, opt_type: Option<Type>, rhs: Box<Ast>},
    StructDec {ident: String, fields: Vec<(String, Type)>},
    FunDec{ident: String, fun_dec: Rc<FunDec> },
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub kind: AstKind,
    pub span: parse::Span,
    pub scope_id: ScopeId,
}

impl Ast {
    pub fn new_nil() -> Ast {
        Ast {
            kind: AstKind::Block(AstBlock::empty()),
            span: parse::Span::new(0, 0),
            scope_id: Default::default()
        }
    }
    pub fn into_boxed(self) -> Box<Ast> {
        Box::new(self)
    }

}

impl AstKind {
    pub fn at_pair(self, pair: &Pair<'_>) -> Ast {
        Ast {
            kind: self,
            scope_id: ScopeId::default(),
            span: parse::Span::new(pair.as_span().start(), pair.as_span().end()),
        }
    }
    pub fn parse_int(pair: Pair<'_>) -> AstKind {
        let data = pair.as_str().parse::<i32>().expect("Ast parse_int failed");
        AstKind::Int(data)
    }
    pub fn parse_real(pair: Pair<'_>) -> AstKind {
        let data = pair.as_str().parse::<f32>().expect("Ast parse_real failed");
        AstKind::Real(data)
    }
    pub fn parse_boole(pair: Pair<'_>) -> AstKind {
        let data = pair.as_str().parse::<bool>().expect("Ast parse_boole failed");
        AstKind::Boole(data)
    }
    pub fn parse_string(pair: Pair<'_>) -> AstKind {
        let data = pair.as_str().to_string();
        AstKind::String(data)
    }
    pub fn parse_ident(pair: Pair<'_>) -> AstKind {
        AstKind::Ident(pair.as_str().to_string())
    }
}
