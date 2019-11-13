
use pest::Parser;
use pest::RuleType;
use pest::prec_climber::{PrecClimber, Operator, Assoc};
use pest::iterators::Pair;
use crate::ast::{Ast, Ident, TypeExpr};
use pest::error::Error;
use pest::*;
use crate::util::Tuples;

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


fn parse_pair(pair: Pair<Rule>) -> Ast {
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::add, Assoc::Left) | Operator::new(Rule::sub, Assoc::Left),
        Operator::new(Rule::mul, Assoc::Left) | Operator::new(Rule::div, Assoc::Right),
        Operator::new(Rule::dot, Assoc::Left)
    ]);

    match pair.as_rule() {
        Rule::line | Rule::statement => parse_pair(pair.into_inner().nth(0).expect("no token inside line or statement")),
        Rule::int =>    Ast::parse_int(pair),
        Rule::ident =>  Ast::Ident(Ident::from(pair)),
        Rule::string => Ast::String(pair.as_str().to_string()),
       // Rule::type_expr => Ast::TypeExpr,
        Rule::infix_expr => {
            climber.climb(pair.into_inner(), parse_pair, |lhs, op, rhs| {
                Ast::Infix(op.as_str().to_string(), Box::new(lhs), Box::new(rhs))
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
            Ast::FnCall{ func, args, kwargs }
        }
        Rule::assignment_inferred => {
            let mut pairs = pair.into_inner();
            Ast::Assign{
                lhs: Ident::from(pairs.next().expect("missing lhs of assignment")),
                opt_type: None,
                rhs: Box::new(parse_pair(pairs.next().expect("missing rhs of assignment"))),
            }
        }
        Rule::assignment_annotated => {
            let mut pairs = pair.into_inner();
            Ast::Assign{
                lhs: Ident::from(pairs.next().expect("missing lhs of assignment")),
                opt_type: Some(TypeExpr::from(pairs.next().expect("missing type of assignment"))),
                rhs: parse_pair(pairs.nth(0).expect("missing rhs of assignment")).into_boxed(),
            }
        }
        Rule::struct_dec => {
            let mut members = vec![];
            let mut iter = pair.into_inner();
            let name = Ident::from(iter.next().expect("struct missing name"));
            for (arg, typ) in crate::util::Tuples::new(iter) {
                members.push((Ident::from(arg), TypeExpr::from(typ)));
            }
            Ast::StructDec(name, members)
        }
        r => unreachable!("unanticipated rule: {:?}", r)
    }
}