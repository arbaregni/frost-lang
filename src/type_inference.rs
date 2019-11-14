use std::collections::HashMap;
use crate::error::Error;
use std::fmt::Formatter;
use crate::ast::{Ast, Ident};
use crate::symbols::SymbolTable;
use crate::functions::FnType;
use std::collections::hash_map::Entry;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Void,
    Int,
    Real,
    Type,
    Fn(FnType),
    // type variable, used in inference
    Variable(String),
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match *self {
            Type::Void => write!(f, "Void"),
            Type::Int => write!(f, "Int"),
            Type::Real => write!(f, "Real"),
            Type::Type => write!(f, "Type"),
            Type::Fn(FnType{ ref in_types, ref out_type }) => {
                let in_types = in_types.iter().map(|type_| format!("{}", type_)).collect::<Vec<String>>().join(", ");
                write!(f, "Fn({}) -> {}", in_types, out_type)
            }
            Type::Variable(ref name) => write!(f, "var${}", name),
        }
    }
}
impl Type {
    /// recursively search for the name
    fn contains_var(&self, name: &str) -> bool {
        match *self {
            Type::Void | Type::Int | Type::Real | Type::Type => false,
            Type::Fn(FnType{ ref in_types, ref out_type }) => {
                for in_type in in_types.iter() {
                    if in_type.contains_var(name) {
                        return true;
                    }
                }
                out_type.contains_var(name)
            }
            Type::Variable(ref my_name) => my_name == name
        }
    }
    /// perform the substitution recursively
    fn substitute(&self, sub: &Substitution) -> Type {
        match self {
            Type::Void | Type::Int | Type::Real | Type::Type => self.clone(),
            Type::Fn(FnType{ ref in_types, ref out_type }) => {
                FnType {
                    in_types: in_types.iter().map(|t| t.substitute(sub)).collect(),
                    out_type: Box::new(out_type.substitute(sub))
                }.into()
            }
            Type::Variable(ref my_name) => sub.lookup(my_name, self),
        }
    }
}
struct Context {
    idx: usize,
}
impl Context {
    fn new() -> Context { Context { idx: 0 } }
    fn make_type_variable(&mut self) -> Type {
        let idx = self.idx;
        self.idx += 1;
        Type::Variable(f!("{idx}"))
    }
}

struct Substitution {
    map: HashMap<String, Type>,
}
impl Substitution {
    fn empty() -> Substitution { Substitution { map: HashMap::new() } }
    /// make the substitution from the type variable name to the concrete type
    fn make(var_name: &String, type_: &Type) -> Result<Substitution, Error> {
        if let Type::Variable(v) = type_ {
            if *v == *var_name {
                return Ok(Substitution::empty())
            }
        }
        if type_.contains_var(var_name) {
            return Error::inference(f!("type {type_} contains a reference to itself")).into();
        }
        let mut map = HashMap::new();
        map.insert(var_name.clone(), type_.clone());
        Ok(Substitution{map})
    }
    /// Apply `self` to the given substitution, and return
    fn compose(self, other: Substitution) -> Substitution {
        let mut result = HashMap::new();
        for (id, type_) in self.map.iter() {
            result.insert(id.clone(), type_.clone());
        }
        for (id, type_) in other.map.iter() {
            result.insert(id.clone(), type_.substitute(&self));
        }
        Substitution{map: result}
    }
    /// Lookup the given type variable index, returning it's type or a clone of `type_` if the variable is not found
    fn lookup(&self, name: &str, type_: &Type) -> Type {
        self.map
            .get(name)
            .unwrap_or(type_)
            .clone()
    }
}

/// Construct the substitution which will make these types compatible
    /// Note: this is not symmetric
    ///       t1 is the 'expected' type, t2 is the 'found' type
fn unify(expected: &Type, found: &Type) -> Result<Substitution, Error> {
    // println!("unifying `{}`  &  `{}`...", t1, t2);
    match (expected, found) {
        // if either is a variable, attempt to substitute it with the other
        (Type::Variable(ref name), _) => Substitution::make(name, found),
        (_, Type::Variable(ref name)) => Substitution::make(name, expected),

        (Type::Fn(expect_fn), Type::Fn(found_fn)) => {
            let sub = unify_many(&expect_fn.in_types, &found_fn.in_types)?;
            let expected_out = expect_fn.out_type.substitute(&sub);
            let found_out = found_fn.out_type.substitute(&sub);
            unify(&expected_out, &found_out)
        }
        // otherwise, the types must match exactly
        (_, _) if *expected == *found => Ok(Substitution::empty()),
        _ => Error::inference(f!("type mismatch: expected `{expected}`, found `{found}`")).into()
    }
}
/// Perform unification on each type in turn, returning the composition
/// Note: does not check that the slices are the same len
fn unify_many(types1: &[Type], types2: &[Type]) -> Result<Substitution, Error> {
    let mut sub = Substitution::empty();
    for (t1, t2) in types1.iter().zip(types2.iter()) {
        let sub0 = unify(&t1.substitute(&sub), &t2.substitute(&sub))?;
        sub = sub.compose(sub0);
    }
    Ok(sub)
}

impl Ast {
    fn infer_type(&self, symbols: &mut SymbolTable, ctx: &mut Context) -> Result<(Type, Substitution), Error> {
        match *self {
            Ast::Int(_) => Ok((Type::Int, Substitution::empty())),
            Ast::Real(_) => Ok((Type::Real, Substitution::empty())),
            Ast::TypeExpr(_) => Ok((Type::Type, Substitution::empty())),
            Ast::String(_) => unimplemented!(),
            Ast::Ident(ref name) => if let Some(type_) = symbols.get_type(name) {
                Ok((type_.clone(), Substitution::empty()))
            } else {
                Error::inference(f!("identifier `{name}` is not defined in the current environment")).into()
            },
            Ast::FnCall { ref func, ref args } => {
                // infer the types for the given arguments
                let mut sub0 = Substitution::empty();
                let mut in_types = vec![];
                for arg in args.iter() {
                    // todo apply substitions to the symbol table here
                    let (arg_type, arg_sub) = arg.infer_type(symbols, ctx)?;
                    in_types.push(arg_type);
                    sub0 = sub0.compose(arg_sub);
                }
                let out_type = ctx.make_type_variable();

                // attempt to unify these with what the function is defined to be
                let fn_as_defined = symbols.get_type(func).expect("unbound function");
                let sub1 = unify(fn_as_defined, &FnType{
                    in_types: in_types.clone(),
                    out_type: Box::new(out_type.clone()),
                }.into())?;

                let fn_type = match fn_as_defined.substitute(&sub1) {
                    Type::Fn(fn_type) => fn_type,
                    _ => panic!("fn got subsitited for something other than a function"),
                };

                let sub2 = sub0.compose(sub1);

                // update the type of input arguments
                for in_type in in_types.iter_mut() {
                    *in_type = in_type.substitute(&sub2);
                }

                let sub3 = unify_many(&in_types, &fn_type.in_types)?;
                let sub4 = sub2.compose(sub3);

                Ok((out_type.substitute(&sub4), sub4))
            },
            Ast::Assign{ ref ident, ref opt_type, ref rhs } => {
                let (type_, mut sub) = rhs.infer_type(symbols, ctx)?;
                if let Some(expected_type) = opt_type {
                    sub = sub.compose(unify(expected_type, &type_)?);
                }
                // todo our scope should have the modification where lhs is bound
                update_type(ident, symbols, type_)?;
                Ok((Type::Void, sub))
            },
            Ast::StructDec(_, _) => Ok((Type::Void, Substitution::empty())),
        }
    }
}
fn update_type(ident: &Ident, symbols: &mut SymbolTable, expected_type: Type) -> Result<(), Error> {
    match symbols.get_type_entry(ident) {
        Entry::Occupied(mut occupied) => {
            let sub = unify(&expected_type, occupied.get())?;
            occupied.insert(occupied.get().substitute(&sub));
        },
        Entry::Vacant(vacant) => {
            vacant.insert(expected_type);
        },
    }
    Ok(())
}

pub fn type_check(ast_nodes: &[Ast], symbols: &mut SymbolTable) -> Result<(), Error> {
    let mut ctx = Context::new();
    for node in ast_nodes {
        node.infer_type(symbols, &mut ctx)?;
    }
    Ok(())
}