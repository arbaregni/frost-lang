use pest::Parser;
use pest::prec_climber::{PrecClimber, Operator, Assoc};
use pest::iterators::Pair;
use pest::error::Error;
use crate::ast::{Ast, AstKind, AstBlock};
use crate::type_inference::Type;
use crate::functions::{FunType, FunDec};
use crate::symbols::SymbolTable;
use std::rc::Rc;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct Grammar;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    begin: usize,
    end: usize,
}
impl Span {
    pub fn new(begin: usize, end: usize) -> Self {
        Self { begin, end }
    }
    /// Given a string and a span, return a multi-line string
    /// that underlines the given portion, and includes surrounding text
    pub fn underline(&self, text: &str) -> String {
        // TODO: line and character numbers
        // figure out where to start
        let mut start_idx = self.begin;
        while self.begin - start_idx <= 7 {
            if start_idx == 0 || text[..start_idx].ends_with('\n') {
                break;
            }
            start_idx -= 1;
        }
        // figure out where to end
        let mut end_idx = self.end;
        while end_idx - self.end <= 7 {
            if end_idx == text.len() || text[end_idx..end_idx+1].starts_with('\n') {
                break;
            }
            end_idx += 1;
        }
        let mut line = text[start_idx..end_idx].to_string();
        if line.is_empty() || !line.ends_with('\n') {
            line.push('\n');
        }
        // push on leading white space
        for _ in start_idx..self.begin {
            line.push(' ');
        }
        // push on underlines
        for _ in self.begin..self.end {
            line.push('^');
        }
        line.push('\n');
        line
    }
}

pub fn parse(source: &str, symbol_table: &mut SymbolTable) -> Result<Vec<Ast>, Error<Rule>> {
    let mut ast_nodes = vec![];
    let pairs = Grammar::parse(Rule::source, source)?;
    for pair in pairs {
        if pair.as_rule() == Rule::EOI { continue; }
        let ast = parse_pair(pair, symbol_table);
        ast_nodes.push(ast);
    }
    Ok(ast_nodes)
}

fn parse_type(pair: Pair<Rule>) -> Type {
    assert_eq!(pair.as_rule(), Rule::type_expr);
    let mut pairs = pair.into_inner();
    let name = pairs.next().expect("type expr missing name");
    match name.as_str() {
        "Void" => Type::Void,
        "Int"  => Type::Int,
        "Real" => Type::Real,
        _ => unimplemented!()
    }
}

fn parse_pair(pair: Pair<Rule>, symbol_table: &mut SymbolTable) -> Ast {
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::add, Assoc::Left) | Operator::new(Rule::sub, Assoc::Left),
        Operator::new(Rule::mul, Assoc::Left) | Operator::new(Rule::div, Assoc::Right),
        Operator::new(Rule::dot, Assoc::Left)
    ]);
    let cpy = pair.clone();
    let kind = match pair.as_rule() {
        Rule::statement => return parse_pair(pair.into_inner().nth(0).expect("no token inside line or statement"), symbol_table),
        Rule::infix_expr => {
            return climber.climb(pair.into_inner(), |pair| parse_pair(pair, symbol_table), |lhs, op, rhs| {
                let builtin = match op.as_str() {
                    "+" => "add",
                    "-" => "sub",
                    "*" => "mul",
                    "/" => "div",
                    _ => panic!("Invalid operator {}", op),
                };
                AstKind::FunCall { func: builtin.to_string(), args: vec![lhs, rhs] }.at_pair(&op)
            })
        }
        Rule::int =>    AstKind::parse_int(pair),
        Rule::real =>   AstKind::parse_real(pair),
        Rule::boole =>  AstKind::parse_boole(pair),
        Rule::ident =>  AstKind::parse_ident(pair),
        Rule::string => AstKind::parse_string(pair),
       // Rule::type_expr => Ast::TypeExpr,
        Rule::if_stmnt => {
            let mut pairs = pair.into_inner();
            let test = parse_pair(pairs.next().expect("if_stmnt missing test"), symbol_table).into_boxed();
            let if_branch = parse_pair(pairs.next().expect("if_stmnt missing body"), symbol_table).into_boxed();
            let else_branch = match pairs.next() {
                Some(pair) => parse_pair(pair, symbol_table),
                None => Ast::new_nil(),
            }.into_boxed();
            AstKind::IfStmnt { test, if_branch, else_branch }
        }
        Rule::fn_call => {
            let mut args = vec![];
            let mut pairs = pair.into_inner();
            let func = pairs.next().expect("fn_call missing fn name").as_str().to_string();
            for pair in pairs {
                match pair.as_rule() {
                    Rule::arg => args.push(parse_pair(pair
                                                 .into_inner()
                                                 .next()
                                                 .expect("fn_call arg missing inner"), symbol_table)),
                    _ => unreachable!()
                }
            }
            AstKind::FunCall { func, args }
        }
        Rule::assignment_inferred => {
            let mut pairs = pair.into_inner();
            AstKind::Assign{
                ident: pairs.next().expect("missing lhs of assignment").as_str().to_string(),
                opt_type: None,
                rhs:parse_pair(pairs.next().expect("missing rhs of assignment"),symbol_table).into_boxed(),
            }
        }
        Rule::assignment_annotated => {
            unimplemented!()
        }
        Rule::struct_dec => {
            let mut fields = vec![];
            let mut iter = pair.into_inner();
            let ident = iter.next().expect("struct missing name").as_str().to_string();
            for (arg, typ) in crate::util::Tuples::new(iter) { //TODO replace with itertools
                fields.push((arg.as_str().to_string(), parse_type(typ)));
            }
            AstKind::StructDec { ident, fields }
        }
        Rule::fun_dec => {
            let mut params = vec![];
            let mut in_types = vec![];
            let mut out_type = None;
            let mut body = None;
            let mut iter = pair.into_inner();
            let ident = iter.next().expect("function missing name").as_str().to_string();
            for inner_pair in iter {
                match inner_pair.as_rule() {
                    Rule::fun_param => {
                        let mut param_iter = inner_pair.into_inner();
                        params.push(param_iter.next().expect("function missing param name").as_str().to_string());
                        in_types.push(parse_type(param_iter.next().expect("function missing param type")));
                    }
                    Rule::type_expr => {
                        out_type = Some(parse_type(inner_pair));
                    }
                    Rule::block => {
                        body = Some(parse_block(inner_pair, symbol_table));
                    }
                    _ => unreachable!("unreachable rule inside function: {:?}", inner_pair.as_rule())
                }
            }
            let body = body.expect("didn't parse body of defined function");
            let out_type = Box::new(out_type.unwrap_or(Type::Void));
            let fun_dec = FunDec::new(params, body, FunType { in_types, out_type });
            let fun_dec = Rc::new(fun_dec);
            AstKind::FunDec { ident, fun_dec }
        }
        Rule::block => {
            AstKind::Block(parse_block(pair, symbol_table))
        }
        // enumerate the rest specifically
          Rule::EOI | Rule::WHITESPACE | Rule::COMMENT | Rule::reserved
        | Rule::term | Rule::infix_op | Rule::add | Rule::sub | Rule::mul | Rule::div | Rule::dot
        | Rule::generic_args | Rule::type_expr | Rule::arg | Rule::expr | Rule::fun_param
        | Rule::source
        => { unreachable!("unreachable rule: {:?}", pair.as_rule()) },
    };
    kind.at_pair(&cpy)
}

fn parse_block(pair: Pair<Rule>, symbol_table: &mut SymbolTable) -> AstBlock {
    pair.into_inner()
        .map(|p| parse_pair(p, symbol_table))
        .collect::<Vec<Ast>>()
        .into()
}
