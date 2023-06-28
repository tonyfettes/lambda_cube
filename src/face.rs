use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use super::core;

#[derive(Debug, PartialEq)]
pub struct MismatchedType {
    expect: Type,
    actual: Type
}

#[derive(Debug, PartialEq)]
pub enum Error {
    UndefinedVariable(String),
    MismatchedType(MismatchedType)
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Clone)]
enum FuncImpl {
    Host(Rc<dyn Fn(Expr) -> Result<Expr>>, Type),
    Expr(Box<Expr>)
}

impl PartialEq for FuncImpl {
    fn eq(&self, other: &FuncImpl) -> bool {
        match (self, other) {
            (Self::Host(self_rc, self_typ),
             Self::Host(other_rc, other_typ)) => Rc::ptr_eq(self_rc, other_rc) && self_typ == other_typ,
            (Self::Expr(self_exp), Self::Expr(other_exp)) => self_exp == other_exp,
            _ => false
        }
    }
}

impl fmt::Display for FuncImpl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Host(_, _) => write!(f, "<built-in function>"),
            Self::Expr(term) => write!(f, "{}", term),
        }
    }
}

impl fmt::Debug for FuncImpl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Host(_, _) => write!(f, "<built-in function>"),
            Self::Expr(term) => write!(f, "{}", term),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    env: Environment,
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

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(int) => write!(f, "{}", int),
            Self::Str(str) => write!(f, "\"{}\"", str),
            Self::Var(var) => write!(f, "{}", var),
            Self::Fun(fun) => write!(f, "({} : {} . {})", fun.pat, fun.typ, fun.exp),
            Self::App(fun, arg) => write!(f, "({} {})", fun, arg),
            Self::TypFun(fun) => write!(f, "(Λ {} . {})", fun.pat, fun.exp),
            Self::TypApp(fun, arg) => write!(f, "({} {})", fun, arg),
        }
    }
}

impl Expr {
    pub fn int(int: i64) -> Self { Self::Int(int) }
    pub fn str(str: &str) -> Self { Self::Str(str.to_string()) }
    pub fn var(var: &str) -> Self { Self::Var(var.to_string()) }
    pub fn fun(pat: &str, typ: Type, exp: Self) -> Self {
        Self::Fun(Func {
            env: Environment::new(),
            pat: pat.to_string(),
            typ,
            exp: Box::new(exp)
        })
    }
    pub fn app(fun: Expr, arg: Expr) -> Self {
        Self::App(Box::new(fun), Box::new(arg))
    }
    pub fn typ_fun(pat: &str, exp: Self) -> Self {
        Self::TypFun(TypeFunc {
            pat: pat.to_string(),
            exp: Box::new(exp)
        })
    }
    pub fn typ_app(fun: Self, typ: Type) -> Self {
        Self::TypApp(Box::new(fun), typ)
    }
}

impl Expr {
    pub fn elaborate(ctx: Context, exp: Expr) -> Result<(super::core::Expr, Type)> {
        match exp {
            Self::Int(int) => Ok((core::Expr::Int(int), Type::Int)),
            Self::Str(str) => Ok((core::Expr::Str(str), Type::Str)),
            Self::Var(var) => match ctx.find(&var) {
                Some(typ) => Ok((core::Expr::Var(var), typ)),
                None => Err(Error::UndefinedVariable(var)),
            },
            Self::Fun(fun) => {
                let new_ctx = ctx.push(fun.pat.clone(), fun.typ.clone());
                let (exp, typ) = Self::elaborate(new_ctx, *fun.exp)?;
                Ok((core::Expr::Fun(core::Func {
                    env: core::Environment::new(),
                    pat: fun.pat,
                    exp: core::FuncImpl::Expr(Box::new(exp))
                }), Type::fun(fun.typ, typ)))
            },
            Self::App(fun, arg) => {
                let fun_ctx = ctx.clone();
                let arg_ctx = ctx;
                let (fun_exp, fun_typ) = Self::elaborate(fun_ctx, *fun)?;
                let (arg_exp, arg_typ) = Self::elaborate(arg_ctx, *arg)?;
                match fun_typ {
                    Type::Fun(fun_arg_typ, fun_ret_typ) =>
                        if arg_typ == *fun_arg_typ {
                            Ok((core::Expr::App(Box::new(fun_exp), Box::new(arg_exp)), *fun_ret_typ))
                        } else {
                            Err(Error::MismatchedType(MismatchedType {
                                expect: *fun_arg_typ,
                                actual: arg_typ,
                            }))
                        }
                    _ => Err(Error::MismatchedType(MismatchedType {
                        expect: Type::fun(arg_typ, Type::Var("*".to_string())),
                        actual: fun_typ
                    }))
                }
            },
            Self::TypFun(fun) => {
                let new_ctx = ctx.push(fun.pat.clone(), Type::Typ);
                let (exp, typ) = Self::elaborate(new_ctx, *fun.exp)?;
                Ok((exp, Type::ForAll(ForAll { ctx, pat: fun.pat, typ: Box::new(typ) })))
            },
            Self::TypApp(fun, arg) => {
                match Self::elaborate(ctx, *fun)? {
                    (fun, Type::ForAll(for_all)) => {
                        let new_ctx = for_all.ctx.push(for_all.pat, arg);
                        let typ = Type::evaluate(new_ctx, *for_all.typ)?;
                        Ok((fun, typ))
                    },
                    (_, typ) => Err(Error::MismatchedType(MismatchedType {
                        expect: Type::ForAll(ForAll {
                            ctx: Context::new(),
                            pat: "*".to_string(),
                            typ: Box::new(Type::Typ),
                        }),
                        actual: typ
                    }))
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment(HashMap<String, Expr>);

impl Environment {
    pub fn new() -> Self {
        let env = HashMap::<String, Expr>::new();
        Self(env)
    }
    pub fn find(&self, name: String) -> Option<Expr> {
        match self.0.get(&name) {
            Some(term) => Some(term.clone()),
            None => None
        }
    }
    pub fn push(&self, name: String, term: Expr) -> Self {
        let mut next_envt = self.0.clone();
        next_envt.insert(name, term);
        Self(next_envt)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForAll {
    ctx: Context,
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
    Typ,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "Num"),
            Self::Str => write!(f, "Str"),
            Self::Var(var) => write!(f, "{}", var),
            Self::Fun(pat, exp) => write!(f, "λ {} . {}", pat, exp),
            Self::ForAll(for_all) => write!(f, "∀ {} . {}", for_all.pat, for_all.typ),
            Self::Typ => write!(f, "∗"),
        }
    }
}

impl Type {
    pub fn num() -> Self { Self::Int }
    pub fn str() -> Self { Self::Str }
    pub fn var(var: &str) -> Self { Self::Var(var.to_string()) }
    pub fn fun(pat: Type, exp: Type) -> Self { Self::Fun(Box::new(pat), Box::new(exp)) }
    pub fn for_all(pat: &str, exp: Type) -> Self {
        Self::ForAll(ForAll {
            ctx: Context::new(),
            pat: pat.to_string(),
            typ: Box::new(exp)
        })
    }
}

impl Type {
    pub fn evaluate(ctx: Context, typ: Type) -> Result<Type> {
        match typ {
            Self::Int => Ok(Self::Int),
            Self::Str => Ok(Self::Str),
            Self::Fun(arg, ret) => {
                let ret_ctx = ctx.clone();
                let arg_ctx = ctx;
                Ok(Self::fun(
                    Self::evaluate(arg_ctx, *arg)?,
                    Self::evaluate(ret_ctx, *ret)?,
                ))
            },
            Self::Var(var) => match ctx.find(&var) {
                Some(typ) => Ok(typ),
                None => Err(Error::UndefinedVariable(var))
            },
            Self::ForAll(for_all) => Ok(Self::ForAll(for_all)),
            Self::Typ => Ok(Self::Typ)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Context(HashMap<String, Type>);

impl Context {
    pub fn new() -> Context {
        Context(HashMap::new())
    }
    pub fn find(&self, name: &String) -> Option<Type> {
        match self.0.get(name) {
            Some(term) => Some(term.clone()),
            None => None
        }
    }
    pub fn push(&self, name: String, ty: Type) -> Self {
        let mut new_ctx = self.0.clone();
        new_ctx.insert(name, ty.clone());
        Self(new_ctx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod elaborate {
        use super::*;

        macro_rules! assert_elaborate {
            ($face:expr, $core:expr, $type:expr) => {
                assert_eq!(Expr::elaborate(Context::new(), $face), Ok(($core, $type)));
            }
        }

        #[test]
        fn fun() {
            assert_elaborate!(
                Expr::fun("x", Type::Int, Expr::var("x")),
                core::Expr::fun("x", core::Expr::var("x")),
                Type::fun(Type::Int, Type::Int)
            );
        }

        #[test]
        fn app() {
            assert_elaborate!(
                Expr::app(Expr::fun("x", Type::Int, Expr::var("x")), Expr::int(1)),
                core::Expr::app(core::Expr::fun("x", core::Expr::var("x")), core::Expr::int(1)),
                Type::Int
            );
        }

        #[test]
        fn typ_fun() {
            assert_elaborate!(
                Expr::typ_fun("a", Expr::fun("x", Type::var("a"), Expr::var("x"))),
                core::Expr::fun("x", core::Expr::var("x")),
                Type::for_all("a", Type::fun(Type::var("a"), Type::var("a")))
            );
        }

        #[test]
        fn typ_app() {
            assert_elaborate!(
                Expr::typ_app(Expr::typ_fun("a", Expr::fun("x", Type::var("a"), Expr::var("x"))), Type::Int),
                core::Expr::fun("x", core::Expr::var("x")),
                Type::fun(Type::Int, Type::Int)
            );
        }
    }

    mod display {
        use super::*;

        #[test]
        fn fun() {
            let fun = Expr::fun("x", Type::Int, Expr::var("x"));
            assert_eq!(format!("{}", fun), "(x : Num . x)");
        }

        #[test]
        fn app() {
            let fun = Expr::fun("x", Type::Int, Expr::var("x"));
            let arg = fun.clone();
            let app = Expr::app(fun.clone(), arg.clone());
            assert_eq!(format!("{}", app), format!("({} {})", fun, arg));
        }

        #[test]
        fn typ_fun() {
            let fun = Expr::fun("x", Type::Int, Expr::var("x"));
            let typ_fun = Expr::typ_fun("a", fun.clone());
            assert_eq!(format!("{}", typ_fun), format!("(Λ {} . {})", "a", fun));
        }

        #[test]
        fn typ_app() {
            let fun = Expr::fun("x", Type::Int, Expr::var("x"));
            let typ_fun = Expr::typ_fun("a", fun.clone());
            let typ_app = Expr::typ_app(typ_fun.clone(), Type::Int);
            assert_eq!(format!("{}", typ_app), format!("({} {})", typ_fun, Type::Int));
        }
    }
}
