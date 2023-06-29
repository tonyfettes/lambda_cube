/// Normalize the parsed AST, use De Brujin index so that we don't have to deal with
/// alpha-equivalence afterwards.

use crate::parse::ast;
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
    pub fn normalize(env: Environment, typ: ast::Type) -> Result<face::Type> {
        match typ {
            ast::Type::Int => Ok(face::Type::Int),
            ast::Type::Str => Ok(face::Type::Str),
            ast::Type::Fun(arg, ret) => Ok(face::Type::fun(
                Self::normalize(env.clone(), *arg)?,
                Self::normalize(env, *ret)?
            )),
            ast::Type::Var(var) => match env.typ.get(&var) {
                Some(idx) => Ok(face::Type::var(idx.clone())),
                None => Err(Error::UndefinedVariable(var)),
            },
            ast::Type::ForAll(pat, typ) => {
                let new_env = env.typ(pat);
                Ok(face::Type::ForAll(face::ForAll {
                    env: face::Environment::new(),
                    typ: Box::new(Self::normalize(new_env, *typ)?)
                }))
            },
            ast::Type::Typ => { Ok(face::Type::Typ) }
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
    pub fn normalize(env: Environment, exp: ast::Term) -> Result<face::Expr> {
        match exp {
            ast::Term::Int(int) => Ok(face::Expr::Int(int)),
            ast::Term::Str(str) => Ok(face::Expr::Str(str)),
            ast::Term::Var(var) => match env.exp.get(&var) {
                Some(idx) => Ok(face::Expr::Var(env.exp.len() - idx - 1)),
                None => Err(Error::UndefinedVariable(var))
            },
            ast::Term::Func(func) => match func.arg.typ {
                ast::Type::Typ => {
                    let new_env = env.typ(func.arg.var);
                    Ok(face::Expr::TypFun(face::TypeFunc {
                        exp: Box::new(Self::normalize(new_env, *func.body)?)
                    }))
                },
                _ => {
                    let new_env = env.exp(func.arg.var);
                    Ok(face::Expr::Fun(face::Func {
                        typ: Type::normalize(new_env.clone(), func.arg.typ)?,
                        exp: Box::new(Self::normalize(new_env, *func.body)?)
                    }))
                }
            },
            ast::Term::Apply(ast::ApplyData { func, arg }) => Ok(face::Expr::app(
                Self::normalize(env.clone(), *func)?,
                Self::normalize(env, *arg)?
            )),
            ast::Term::TypeApply(ast::TypeApplyData { func, arg }) => {
                Ok(face::Expr::typ_app(
                    Self::normalize(env.clone(), *func)?,
                    Type::normalize(env, *arg)?
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
            let oid = ast::Term::Func(ast::FuncData {
                arg: ast::TypedVar {
                    var: "x".to_string(),
                    typ: ast::Type::Int,
                },
                body: Box::new(ast::Term::Var("x".to_string()))
            });
            let nid = face::Expr::fun(face::Type::Int, face::Expr::var(0));
            assert_eq!(Expr::normalize(Environment::new(), oid), Ok(nid));
        }

        #[test]
        fn identity_apply_one() {
            let oid = ast::Term::Func(ast::FuncData {
                arg: ast::TypedVar {
                    var: "x".to_string(),
                    typ: ast::Type::Int,
                },
                body: Box::new(ast::Term::Var("x".to_string()))
            });
            let oap = ast::Term::Apply(ast::ApplyData {
                func: Box::new(oid),
                arg: Box::new(ast::Term::Int(1))
            });
            let nid = face::Expr::fun(face::Type::Int, face::Expr::var(0));
            let nap = face::Expr::app(nid.clone(), face::Expr::Int(1));
            assert_eq!(Expr::normalize(Environment::new(), oap), Ok(nap));
        }
    }
}
