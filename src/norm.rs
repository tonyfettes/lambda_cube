/// Normalize the parsed AST, use De Brujin index so that we don't have to deal with
/// alpha-equivalence afterwards.

use crate::face;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum Error {
    UndefinedVariable(String),
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq)]
pub struct ForAll {
    pat: String,
    typ: Box<Type>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Str,
    Fun(Box<Type>, Box<Type>),
    Var(String),
    ForAll(ForAll),
}

impl Type {
    pub fn normalize(env: Environment, typ: Type) -> Result<face::Type> {
        match typ {
            Type::Int => Ok(face::Type::Int),
            Type::Str => Ok(face::Type::Str),
            Type::Fun(arg, ret) => Ok(face::Type::fun(
                Self::normalize(env.clone(), *arg)?,
                Self::normalize(env, *ret)?
            )),
            Type::Var(var) => match env.typ.get(&var) {
                Some(idx) => Ok(face::Type::var(idx.clone())),
                None => Err(Error::UndefinedVariable(var)),
            },
            Type::ForAll(for_all) => {
                let new_env = env.typ(for_all.pat);
                Ok(face::Type::ForAll(face::ForAll {
                    env: face::Environment::new(),
                    typ: Box::new(Self::normalize(new_env, *for_all.typ)?)
                }))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pat: String,
    typ: Type,
    exp: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeFunc {
    pat: String,
    exp: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Str(String),
    Var(String),
    Fun(Func),
    App(Box<Expr>, Box<Expr>),
    TypFun(TypeFunc),
    TypApp(Box<Expr>, Type)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    typ: HashMap<String, usize>,
    exp: HashMap<String, usize>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            typ: HashMap::new(),
            exp: HashMap::new()
        }
    }
    pub fn typ(mut self, name: String) -> Self {
        self.typ.insert(name, self.typ.len());
        self
    }
    pub fn exp(mut self, name: String) -> Self {
        self.exp.insert(name, self.exp.len());
        self
    }
}

impl Expr {
    pub fn normalize(env: Environment, exp: Expr) -> Result<face::Expr> {
        match exp {
            Self::Int(int) => Ok(face::Expr::Int(int)),
            Self::Str(str) => Ok(face::Expr::Str(str)),
            Self::Var(var) => match env.exp.get(&var) {
                Some(idx) => Ok(face::Expr::Var(env.exp.len() - idx - 1)),
                None => Err(Error::UndefinedVariable(var))
            },
            Self::Fun(fun) => {
                let new_env = env.exp(fun.pat);
                Ok(face::Expr::Fun(face::Func {
                    typ: Type::normalize(new_env.clone(), fun.typ)?,
                    exp: Box::new(Self::normalize(new_env, *fun.exp)?)
                }))
            },
            Self::App(fun, arg) => Ok(face::Expr::app(
                Self::normalize(env.clone(), *fun)?,
                Self::normalize(env, *arg)?
            )),
            Self::TypFun(fun) => {
                let new_env = env.typ(fun.pat);
                Ok(face::Expr::TypFun(face::TypeFunc {
                    exp: Box::new(Self::normalize(new_env, *fun.exp)?)
                }))
            },
            Self::TypApp(fun, arg) => {
                Ok(face::Expr::typ_app(
                    Self::normalize(env.clone(), *fun)?,
                    Type::normalize(env, arg)?
                ))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod normalize {
        use super::*;

        #[test]
        fn identity() {
            let oid = Expr::Fun(Func {
                pat: "x".to_string(),
                typ: Type::Int,
                exp: Box::new(Expr::Var("x".to_string()))
            });
            let nid = face::Expr::fun(face::Type::Int, face::Expr::var(0));
            assert_eq!(Expr::normalize(Environment::new(), oid), Ok(nid));
        }
    }
}
