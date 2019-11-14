
use pest::Parser;
use pest::prec_climber::{PrecClimber, Operator, Assoc};
use pest::iterators::Pair;
use crate::ast::{Ast, Ident};
use pest::error::Error;
use crate::util::Tuples;
use crate::type_inference::Type;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct Grammar;


pub fn parse(source: &str) -> Result<Vec<Ast>, Error<Rule>> {
    let mut ast_nodes = vec![];
    let pairs = Grammar::parse(Rule::source, source)?;
    for pair in pairs {
        if pair.as_rule() == Rule::EOI { continue; }
        let ast = parse_pair(pair);
        ast_nodes.push(ast);
    }
    Ok(ast_nodes)
}

fn parse_type(pair: Pair<Rule>) -> Type {
    assert_eq!(pair.as_rule(), Rule::type_expr);
    let mut pairs = pair.into_inner();
    let name = Ident::from(pairs.next().expect("type expr missing name"));
    match name.0.as_str() {
        "Void" => Type::Void,
        "Int" => Type::Int,
        "Real" => Type::Real,
        _ => unimplemented!()
    }
}

fn parse_pair(pair: Pair<Rule>) -> Ast {
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::add, Assoc::Left) | Operator::new(Rule::sub, Assoc::Left),
        Operator::new(Rule::mul, Assoc::Left) | Operator::new(Rule::div, Assoc::Right),
        Operator::new(Rule::dot, Assoc::Left)
    ]);

    match pair.as_rule() {
        Rule::line | Rule::statement => parse_pair(pair.into_inner().nth(0).expect("no token inside line or statement")),
        Rule::int =>    Ast::parse_int(pair),
        Rule::real =>    Ast::parse_real(pair),
        Rule::ident =>  Ast::Ident(Ident::from(pair)),
        Rule::string => Ast::String(pair.as_str().to_string()),
       // Rule::type_expr => Ast::TypeExpr,
        Rule::infix_expr => {
            climber.climb(pair.into_inner(), parse_pair, |lhs, op, rhs| {
                let builtin = match op.as_str() {
                    "+" => Ident(format!("add")),
                    _ => panic!("Invalid operator {}", op),
                };
                Ast::FnCall { func: builtin, args: vec![lhs, rhs] }
            })
        }
        Rule::fn_call => {
            let mut args = vec![];
            let mut kwargs = vec![];
            let mut pairs = pair.into_inner();
            let func = Ident::from(pairs.next().expect("fn_call missing fn name"));
            for pair in pairs {
                match pair.as_rule() {
                    Rule::arg => args.push(parse_pair(pair
                                                 .into_inner()
                                                 .next()
                                                 .expect("fn_call arg missing inner"))),
                    Rule::kwarg => {
                        for (key, arg) in Tuples::new(pair.into_inner()) {
                            kwargs.push((Ident::from(key), parse_pair(arg)));
                        }
                    },
                    _ => unreachable!()
                }
            }
            Ast::FnCall{ func, args }
        }
        Rule::assignment_inferred => {
            let mut pairs = pair.into_inner();
            Ast::Assign{
                ident: Ident::from(pairs.next().expect("missing lhs of assignment")),
                opt_type: None,
                rhs: Box::new(parse_pair(pairs.next().expect("missing rhs of assignment"))),
            }
        }
        Rule::assignment_annotated => {
            let mut pairs = pair.into_inner();
            Ast::Assign{
                ident: Ident::from(pairs.next().expect("missing lhs of assignment")),
                opt_type: None, // todo parse type expressions
                rhs: parse_pair(pairs.nth(0).expect("missing rhs of assignment")).into_boxed(),
            }
        }
        Rule::struct_dec => {
            let mut members = vec![];
            let mut iter = pair.into_inner();
            let name = Ident::from(iter.next().expect("struct missing name"));
            for (arg, typ) in crate::util::Tuples::new(iter) {
                members.push((Ident::from(arg), parse_type(typ)));
            }
            Ast::StructDec(name, members)
        }
        r => unreachable!("unanticipated rule: {:?}", r)
    }
}