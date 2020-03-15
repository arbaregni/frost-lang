use std::collections::HashMap;
use crate::error::{Error, ErrorKind, ResultExtension};
use std::fmt::Formatter;
use crate::ast::{Ast, AstKind, AstBlock};
use crate::symbols::{SymbolTable};
use crate::functions::{FunType};
use std::collections::hash_map::Entry;
use std::ops::Deref;
use crate::scope::ScopeId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Void,
    Int,
    Real,
    Boole,
    String,
    Type,
    Fn(FunType),
    // type variable, used in inference
    Variable(String),
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match *self {
            Type::Void => write!(f, "Void"),
            Type::Int => write!(f, "Int"),
            Type::Real => write!(f, "Real"),
            Type::Boole => write!(f, "Boole"),
            Type::String => write!(f, "String"),
            Type::Type => write!(f, "Type"),
            Type::Fn(FunType { ref in_types, ref out_type }) => {
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
            Type::Void | Type::Int | Type::Real | Type::Boole | Type::String | Type::Type => false,
            Type::Fn(FunType { ref in_types, ref out_type }) => {
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
            Type::Void | Type::Int | Type::Real | Type::Boole | Type::String | Type::Type => self.clone(),
            Type::Fn(FunType { ref in_types, ref out_type }) => {
                FunType {
                    in_types: in_types.iter().map(|t| t.substitute(sub)).collect(),
                    out_type: Box::new(out_type.substitute(sub))
                }.into()
            }
            Type::Variable(ref my_name) => sub.lookup(my_name, self),
        }
    }
}
#[derive(Debug, Clone)]
pub struct Quantified {
    pub type_: Type,
    pub variables: Vec<String>,
}
impl Quantified {
    fn get_instantiation(ident: &str, scope_id: ScopeId, symbols: &mut SymbolTable, ctx: &mut Context) -> Type {
        match symbols.quant_entry(ident, scope_id) {
            Entry::Occupied(occupied) => occupied.get().instantiate(ctx),
            Entry::Vacant(_) => panic!("Quantified is vacant"),
        }
    }
    fn instantiate(&self, ctx: &mut Context) -> Type {
        let sub = Substitution::from_bound_variables(&self.variables, ctx);
        self.type_.substitute(&sub)
    }
}
impl std::fmt::Display for Quantified {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "forall {}. {}", self.variables.join(", "), self.type_)
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
    fn from_bound_variables(variables: &[String], ctx: &mut Context) -> Substitution {
        let mut map = HashMap::new();
        for var_name in variables.iter() {
            map.insert(var_name.clone(), ctx.make_type_variable());
        }
        Substitution{map}
    }
    /// make the substitution from the type variable name to the concrete type
    fn make(var_name: &String, type_: &Type) -> Result<Substitution, Error> {
        if let Type::Variable(v) = type_ {
            if *v == *var_name {
                return Ok(Substitution::empty())
            }
        }
        if type_.contains_var(var_name) {
            return ErrorKind::SelfReferentialType{type_: type_.clone()}.into_result();
        }
        let mut map = HashMap::new();
        map.insert(var_name.clone(), type_.clone());
        Ok(Substitution{map})
    }
    /// Apply `self` to the given substitution, and return the composition
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
fn unify(expected: &Type, actual: &Type) -> Result<Substitution, Error> {
    // println!("unifying `{}`  &  `{}`...", t1, t2);
    match (expected, actual) {
        // if either is a variable, attempt to substitute it with the other
        (Type::Variable(ref name), _) => Substitution::make(name, actual),
        (_, Type::Variable(ref name)) => Substitution::make(name, expected),

        (Type::Fn(expect_fn), Type::Fn(found_fn)) => {
            let sub = unify_many(&expect_fn.in_types, &found_fn.in_types)?;
            let expected_out = expect_fn.out_type.substitute(&sub);
            let found_out = found_fn.out_type.substitute(&sub);
            unify(&expected_out, &found_out)
        }
        // otherwise, the types must match exactly
        (_, _) if *expected == *actual => Ok(Substitution::empty()),
        _ => ErrorKind::TypeMismatch{
            expected_type: expected.clone(),
            actual_type: actual.clone(),
        }.into_result()
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
        match self.kind {
            AstKind::Int(_) => Ok((Type::Int, Substitution::empty())),
            AstKind::Real(_) => Ok((Type::Real, Substitution::empty())),
            AstKind::Boole(_) => Ok((Type::Boole, Substitution::empty())),
            AstKind::TypeExpr(_) => Ok((Type::Type, Substitution::empty())),
            AstKind::String(_) => Ok((Type::String, Substitution::empty())),
            AstKind::Ident(ref name) => if let Some(type_) = symbols.get_type(name, self.scope_id) {
                Ok((type_.clone(), Substitution::empty()))
            } else {
                ErrorKind::UndefinedSymbol{name: name.clone(), scope_id: self.scope_id}
                    .into_result()
                    .with_span(self.span)
            },
            AstKind::Block(ref statements) => statements.infer_type(symbols, ctx),
            AstKind::IfStmnt { ref test, ref if_branch, ref else_branch } => {
                let (test_type, mut sub) = test.infer_type(symbols, ctx)?;
                sub = sub.compose(unify(&Type::Boole, &test_type).with_span(self.span)? );
                let out_type = ctx.make_type_variable();
                let (if_branch_type, sub1) = if_branch.infer_type(symbols, ctx)?;
                sub = sub.compose(sub1).compose(unify(&out_type, &if_branch_type).with_span(self.span)?);
                let else_branch_type = if let Some(branch) = else_branch {
                    let (else_branch_type, sub1) = branch.infer_type(symbols, ctx)?;
                    sub = sub.compose(sub1);
                    else_branch_type
                } else {
                    Type::Void
                };
                let out_type = out_type.substitute(&sub);
                sub = sub.compose(unify(&out_type, &else_branch_type).with_span(self.span)?);
                Ok((out_type.substitute(&sub), sub))
            }
            AstKind::FunCall { ref func, ref args } => {
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
                let fn_as_defined = symbols.get_fun(func, self.scope_id)
                                           .ok_or(ErrorKind::UndefinedSymbol{
                                               name: func.to_string(),
                                               scope_id: self.scope_id,
                                              }.into_error()
                                               .with_span(self.span)
                                           )?
                                           .fun_type.clone().into();

                let sub1 = unify(&fn_as_defined, &FunType {
                    in_types: in_types.clone(),
                    out_type: Box::new(out_type.clone()),
                }.into()).with_span(self.span)?;

                let fn_type = match fn_as_defined.substitute(&sub1) {
                    Type::Fn(fn_type) => fn_type,
                    _ => panic!("fn got substituted for something other than a function"),
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
            AstKind::Assign{ ref ident, ref opt_type, ref rhs } => {
                let (type_, mut sub) = rhs.infer_type(symbols, ctx)?;
                if let Some(expected_type) = opt_type {
                    sub = sub.compose(unify(expected_type, &type_).with_span(self.span)?);
                }
                // modify the type in the RHS, where it is bound
                update_type(ident, rhs.scope_id, symbols, type_)?;
                Ok((Type::Void, sub))
            },
            AstKind::FunDec{ ident: _, ref fun_dec } => {
                let scope_id = fun_dec.scope_id();
                let param_iter = fun_dec.params.iter();
                let in_types_iter = fun_dec.fun_type.in_types.iter();
                for (param_name, param_type) in param_iter.zip(in_types_iter) {
                    update_type(param_name, scope_id, symbols, param_type.clone())?;
                }
                let (out_type, sub0) = fun_dec.body.borrow_mut().infer_type(symbols, ctx)?;
                let maybe_span = fun_dec.body.borrow().0.last().map(|n| n.span);
                let sub1 = unify(fun_dec.fun_type.out_type.deref(), &out_type).with_maybe_span(maybe_span)?;
                Ok((Type::Void, sub0.compose(sub1)))
            }
            AstKind::StructDec {..} => Ok((Type::Void, Substitution::empty())),
        }
    }
}

impl AstBlock {
    fn infer_type(&self, symbols: &mut SymbolTable, ctx: &mut Context) -> Result<(Type, Substitution), Error> {
        let mut to_return = (Type::Void, Substitution::empty());
        for statement in self.0.iter() {
            to_return = statement.infer_type(symbols, ctx)?;
        }
        Ok(to_return)
    }
}

/// ensure that the type of a given variable is correct
fn update_type(ident: &str, scope_id: ScopeId, symbols: &mut SymbolTable, expected_type: Type) -> Result<(), Error> {
    match symbols.type_entry(ident, scope_id) {
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
