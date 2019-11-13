use pest;
use crate::parse;
use std::fmt::{Formatter, Error};

type Pair<'a> = pest::iterators::Pair<'a, parse::Rule>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub String);
impl From<Pair<'_>> for Ident {
    fn from(pair: Pair<'_>) -> Self {
        assert_eq!(parse::Rule::ident, pair.as_rule());
        Ident(pair.as_str().to_string())
    }
}
impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "ident({})", self.0)
    }
}

#[derive(Debug)]
pub struct TypeExpr {
    name: Ident,
    generics: Vec<TypeExpr>
}

impl From<Pair<'_>> for TypeExpr {
    fn from(pair: Pair<'_>) -> Self {
        assert_eq!(parse::Rule::type_expr, parse::Rule::ident);
        let mut pairs = pair.into_inner();
        let name = Ident::from(pairs.next().expect("type expr missing name"));
        let mut generics = vec![];
        TypeExpr { name, generics }
    }
}
#[derive(Debug)]
pub enum Ast {
    Int(i32),
    Real(f32),
    String(String),
    Ident(Ident),
    Infix(String, Box<Ast>, Box<Ast>),
    FnCall{func: Ident, args: Vec<Ast>, kwargs: Vec<(Ident, Ast)>},
    TypeExpr(TypeExpr),
    Assign{lhs: Ident, opt_type: Option<TypeExpr>, rhs: Box<Ast>},
    StructDec(Ident, Vec<(Ident, TypeExpr)>)
}

impl Ast {
    pub fn parse_int(pair: Pair<'_>) -> Ast {
        Ast::Int(pair.as_str().parse::<i32>().expect("Ast parse_int failed"))
    }
    pub fn parse_real(pair: Pair<'_>) -> Ast {
        Ast::Real(pair.as_str().parse::<f32>().expect("Ast parse_real failed"))
    }
    pub fn into_boxed(self) -> Box<Ast> {
        Box::new(self)
    }
}