use pest;
use crate::parse;
use std::fmt::{Formatter, Error};
use crate::type_inference::Type;

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
pub enum Ast {
    Int(i32),
    Real(f32),
    String(String),
    Ident(Ident),
    FnCall{func: Ident, args: Vec<Ast> },
    TypeExpr(Type),
    Assign{ident: Ident, opt_type: Option<Type>, rhs: Box<Ast>},
    StructDec(Ident, Vec<(Ident, Type)>)
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