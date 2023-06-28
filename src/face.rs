use std::fmt;
use std::rc::Rc;
use super::core;

// Typing context, i.e. A -> B means variable A has type B
#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    typ: usize,
    exp: Vec<Type>
}

impl Context {
    pub fn new() -> Self {
        Context {
            typ: 0,
            exp: Vec::new(),
        }
    }
    pub fn typ(mut self) -> Self {
        self.typ += 1;
        self
    }
    pub fn exp(mut self, typ: Type) -> Self {
        self.exp.push(typ);
        self
    }
}

impl std::fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for idx in 0..self.typ {
            writeln!(f, "{} : ∗", idx)?;
        }
        for (idx, typ) in self.exp.iter().enumerate() {
            writeln!(f, "{} : {}", idx, typ)?;
        }
        Ok(())
    }
}

// Type alias mapping, i.e. A -> B means type variable A is an alias of type B.
#[derive(Debug, Clone, PartialEq)]
pub struct Environment(Vec<Type>);

impl Environment {
    pub fn new() -> Self {
        Self(Vec::new())
    }
    pub fn find(&self, name: usize) -> Option<Type> {
        match self.0.iter().nth_back(name) {
            Some(typ) => Some(typ.clone()),
            None => None
        }
    }
    pub fn push(mut self, typ: Type) -> Self {
        self.0.push(typ);
        self
    }
}

impl std::fmt::Display for Environment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (idx, typ) in self.0.iter().enumerate() {
            writeln!(f, "{} ↪  {}", idx, typ)?;
        }
        Ok(())
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct ForAll {
    pub env: Environment,
    pub typ: Box<Type>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Str,
    Fun(Box<Type>, Box<Type>),
    Var(usize),
    ForAll(ForAll),
    Typ,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "Num"),
            Self::Str => write!(f, "Str"),
            Self::Var(var) => write!(f, "{}\u{20d6}", var),
            Self::Fun(pat, exp) => write!(f, "({} →  {})", pat, exp),
            Self::ForAll(for_all) => write!(f, "(∀ _ →  {})", for_all.typ),
            Self::Typ => write!(f, "∗"),
        }
    }
}

impl Type {
    pub fn num() -> Self { Self::Int }
    pub fn str() -> Self { Self::Str }
    pub fn var(var: usize) -> Self { Self::Var(var) }
    pub fn fun(pat: Type, exp: Type) -> Self { Self::Fun(Box::new(pat), Box::new(exp)) }
    pub fn for_all(exp: Type) -> Self {
        Self::ForAll(ForAll {
            env: Environment::new(),
            typ: Box::new(exp)
        })
    }
}

impl Type {
    pub fn evaluate(env: Environment, typ: Type) -> Result<Type> {
        match typ {
            Self::Int => Ok(Self::Int),
            Self::Str => Ok(Self::Str),
            Self::Fun(arg, ret) => {
                let ret_env = env.clone();
                let arg_env = env;
                Ok(Self::fun(
                    Self::evaluate(arg_env, *arg)?,
                    Self::evaluate(ret_env, *ret)?,
                ))
            },
            Self::Var(var) => match env.find(var) {
                Some(typ) => Ok(typ),
                None => Err(Error::UndefinedVariable(var))
            },
            Self::ForAll(for_all) => Ok(Self::ForAll(for_all)),
            Self::Typ => Ok(Self::Typ)
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct MismatchedType {
    expect: Type,
    actual: Type
}

#[derive(Debug, PartialEq)]
pub enum Error {
    UndefinedVariable(usize),
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
    pub typ: Type,
    pub exp: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeFunc {
    pub exp: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Str(String),
    Var(usize),
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
            Self::Var(var) => write!(f, "{}\u{20d6}", var),
            Self::Fun(fun) => write!(f, "(λ _ : {} →  {})", fun.typ, fun.exp),
            Self::App(fun, arg) => write!(f, "({} {})", fun, arg),
            Self::TypFun(fun) => write!(f, "(Λ _ →  {})", fun.exp),
            Self::TypApp(fun, arg) => write!(f, "({} @ {})", fun, arg),
        }
    }
}

impl Expr {
    pub fn int(int: i64) -> Self { Self::Int(int) }
    pub fn str(str: &str) -> Self { Self::Str(str.to_string()) }
    pub fn var(var: usize) -> Self { Self::Var(var) }
    pub fn fun(typ: Type, exp: Self) -> Self {
        Self::Fun(Func {
            typ,
            exp: Box::new(exp)
        })
    }
    pub fn app(fun: Expr, arg: Expr) -> Self {
        Self::App(Box::new(fun), Box::new(arg))
    }
    pub fn typ_fun(exp: Self) -> Self {
        Self::TypFun(TypeFunc {
            exp: Box::new(exp)
        })
    }
    pub fn typ_app(fun: Self, typ: Type) -> Self {
        Self::TypApp(Box::new(fun), typ)
    }
}

impl Expr {
    pub fn elaborate(ctx: Context, env: Environment, exp: Expr) -> Result<(super::core::Expr, Type)> {
        match exp.clone() {
            Self::Int(int) => Ok((core::Expr::Int(int), Type::Int)),
            Self::Str(str) => Ok((core::Expr::Str(str), Type::Str)),
            Self::Var(var) => match ctx.exp.iter().nth_back(var) {
                Some(typ) => Ok((core::Expr::Var(var), typ.clone())),
                None => Err(Error::UndefinedVariable(var))
            },
            Self::Fun(fun) => {
                let new_ctx = ctx.exp(fun.typ.clone());
                let (exp, typ) = Self::elaborate(new_ctx, env.clone(), *fun.exp)?;
                Ok((core::Expr::Fun(core::Func {
                    env: core::Environment::new(),
                    exp: core::FuncImpl::Expr(Box::new(exp))
                }), Type::fun(fun.typ, typ)))
            },
            Self::App(fun, arg) => {
                let (fun_exp, fun_typ) = Self::elaborate(ctx.clone(), env.clone(), *fun)?;
                let (arg_exp, arg_typ) = Self::elaborate(ctx, env, *arg)?;
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
                        expect: Type::fun(arg_typ, Type::Var(0)),
                        actual: fun_typ
                    }))
                }
            },
            Self::TypFun(fun) => {
                let new_ctx = ctx.typ();
                let (exp, typ) = Self::elaborate(new_ctx, env.clone(), *fun.exp)?;
                Ok((exp, Type::ForAll(ForAll { env, typ: Box::new(typ) })))
            },
            Self::TypApp(fun, arg) => {
                match Self::elaborate(ctx, env.clone(), *fun)? {
                    (fun, Type::ForAll(for_all)) => {
                        let new_env = for_all.env.push(arg);
                        let typ = Type::evaluate(new_env, *for_all.typ)?;
                        Ok((fun, typ))
                    },
                    (_, typ) => Err(Error::MismatchedType(MismatchedType {
                        expect: Type::ForAll(ForAll {
                            env: Environment::new(),
                            typ: Box::new(Type::Typ),
                        }),
                        actual: typ
                    }))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod elaborate {
        use super::*;

        macro_rules! assert_elaborate {
            ($face:expr, $core:expr, $type:expr) => {
                assert_eq!(Expr::elaborate(Context::new(), Environment::new(), $face), Ok(($core, $type)));
            }
        }

        #[test]
        fn fun() {
            assert_elaborate!(
                Expr::fun(Type::Int, Expr::var(0)),
                core::Expr::fun(core::Expr::var(0)),
                Type::fun(Type::Int, Type::Int)
            );
        }

        #[test]
        fn app() {
            assert_elaborate!(
                Expr::app(Expr::fun(Type::Int, Expr::var(0)), Expr::int(1)),
                core::Expr::app(core::Expr::fun(core::Expr::var(0)), core::Expr::int(1)),
                Type::Int
            );
        }

        #[test]
        fn typ_fun() {
            assert_elaborate!(
                Expr::typ_fun(Expr::fun(Type::var(0), Expr::var(0))),
                core::Expr::fun(core::Expr::var(0)),
                Type::for_all(Type::fun(Type::var(0), Type::var(0)))
            );
        }

        #[test]
        fn typ_app() {
            assert_elaborate!(
                Expr::typ_app(Expr::typ_fun(Expr::fun(Type::var(0), Expr::var(0))), Type::Int),
                core::Expr::fun(core::Expr::var(0)),
                Type::fun(Type::Int, Type::Int)
            );
        }

        #[test]
        fn typ_fun_complex() {
            assert_elaborate!(
                Expr::fun(
                    Type::for_all(Type::fun(Type::var(0), Type::var(0))),
                    Expr::typ_fun(
                        Expr::fun(
                            Type::var(0),
                            Expr::app(Expr::typ_app(Expr::var(1), Type::var(0)), Expr::var(0))
                        )
                    )
                ),
                core::Expr::fun(core::Expr::fun(core::Expr::app(core::Expr::var(1), core::Expr::var(0)))),
                Type::fun(
                    Type::for_all(Type::fun(Type::var(0), Type::var(0))),
                    Type::for_all(Type::fun(Type::var(0), Type::var(0)))
                )
            );
        }

        #[test]
        fn typ_app_complex() {
            // ∀ a . a -> a
            let id_typ_a = Type::for_all(Type::fun(Type::var(0), Type::var(0)));
            // ∀ b . b -> b
            let id_typ_b = Type::for_all(Type::fun(Type::var(0), Type::var(0)));
            // x a y
            let x_a_y = Expr::app(Expr::typ_app(Expr::var(1), Type::var(0)), Expr::var(0));
            // Λ a . λ (y : a) . x a y
            let la_ly_x_a_y = Expr::typ_fun(Expr::fun(Type::var(0), x_a_y));
            // λ (x : ∀ b . b -> b) . Λ a . λ y . x a y
            let lx_la_ly_x_a_y = Expr::fun(id_typ_b, la_ly_x_a_y);
            // Λ b . λ (x : b) . x
            let id_fun_b = Expr::typ_fun(Expr::fun(Type::var(0), Expr::var(0)));

            // λ y . x y
            let ly_x_y = core::Expr::fun(core::Expr::app(core::Expr::var(1), core::Expr::var(0)));
            // λ x . λ y . x y
            let lx_ly_x_y = core::Expr::fun(ly_x_y);
            // λ x . x
            let lx_x = core::Expr::fun(core::Expr::var(0));
            assert_elaborate!(
                Expr::app(lx_la_ly_x_a_y, id_fun_b),
                core::Expr::app(lx_ly_x_y, lx_x),
                id_typ_a
            );
        }
    }

    mod display {
        use super::*;

        #[test]
        fn fun() {
            let fun = Expr::fun(Type::Int, Expr::var(0));
            assert_eq!(format!("{}", fun), "(λ _ : Num →  0⃖)");
        }

        #[test]
        fn app() {
            let fun = Expr::fun(Type::Int, Expr::var(0));
            let arg = fun.clone();
            let app = Expr::app(fun.clone(), arg.clone());
            assert_eq!(format!("{}", app), format!("({} {})", fun, arg));
        }

        #[test]
        fn typ_fun() {
            let fun = Expr::fun(Type::Int, Expr::var(0));
            let typ_fun = Expr::typ_fun(fun.clone());
            assert_eq!(format!("{}", typ_fun), format!("(Λ _ →  {})", fun));
        }

        #[test]
        fn typ_app() {
            let fun = Expr::fun(Type::Int, Expr::var(0));
            let typ_fun = Expr::typ_fun(fun.clone());
            let typ_app = Expr::typ_app(typ_fun.clone(), Type::Int);
            assert_eq!(format!("{}", typ_app), format!("({} @ {})", typ_fun, Type::Int));
        }
    }
}
