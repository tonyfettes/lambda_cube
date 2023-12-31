use std::fmt;
use std::rc::Rc;
use crate::core;

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
    pub fn push(mut self, typ: Type) -> Self {
        match typ {
            Type::Typ => {
                self.typ += 1;
                self
            }
            _ => {
                self.exp.push(typ);
                self
            }
        }
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
    Sum(Box<Type>, Box<Type>),
    Pro(Box<Type>, Box<Type>),
    Var(usize),
    ForAll(ForAll),
    Typ,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Str => write!(f, "Str"),
            Self::Fun(pat, exp) => write!(f, "({} →  {})", pat, exp),
            Self::Sum(fst, snd) => write!(f, "({} + {})", fst, snd),
            Self::Pro(fst, snd) => write!(f, "({} × {})", fst, snd),
            Self::Var(var) => write!(f, "{}\u{20d6}", var),
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
    pub fn sum(fst: Type, snd: Type) -> Self {
        // let fst_fun_typ = Self::fun(fst, Self::var(0));
        // let snd_fun_typ = Self::fun(snd, Self::var(0));
        // Self::for_all(Self::fun(fst_fun_typ, Self::fun(snd_fun_typ, Self::var(0))))
        Self::Sum(Box::new(fst), Box::new(snd))
    }
    pub fn pro(fst: Type, snd: Type) -> Self {
        // Self::for_all(Self::fun(Self::fun(fst, Self::fun(snd, Self::var(0))), Self::var(0)))
        Self::Pro(Box::new(fst), Box::new(snd))
    }
    pub fn for_all(exp: Type) -> Self {
        Self::ForAll(ForAll {
            env: Environment::new(),
            typ: Box::new(exp)
        })
    }
}

impl Type {
    pub fn evaluate(ctx: Context, env: Environment, typ: Type) -> Result<Type> {
        match typ {
            Self::Int => Ok(Self::Int),
            Self::Str => Ok(Self::Str),
            Self::Fun(arg, ret) => {
                Ok(Self::fun(
                    Self::evaluate(ctx.clone(), env.clone(), *arg)?,
                    Self::evaluate(ctx, env, *ret)?,
                ))
            },
            Self::Sum(fst, snd) => {
                Ok(Self::fun(
                    Self::evaluate(ctx.clone(), env.clone(), *fst)?,
                    Self::evaluate(ctx, env, *snd)?
                ))
            },
            Self::Pro(fst, snd) => {
                Ok(Self::fun(
                    Self::evaluate(ctx.clone(), env.clone(), *fst)?,
                    Self::evaluate(ctx, env, *snd)?
                ))
            },
            Self::Var(var) => match env.find(var) {
                Some(typ) => Ok(typ),
                None => match var < ctx.typ  {
                    true => Ok(Self::Var(var)),
                    false => Err(Error::UndefinedVariable(var))
                }
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

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UndefinedVariable(var) => write!(f, "Variable {} is not defined", var),
            Self::MismatchedType(typ) =>
                write!(f, "Type of argument {} is not consistent with the type of parameter {}",
                       typ.actual, typ.expect),
        }
    }
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
            Self::Expr(exp) => write!(f, "{}", exp),
        }
    }
}

impl fmt::Debug for FuncImpl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Host(_, _) => write!(f, "<built-in function>"),
            Self::Expr(exp) => write!(f, "{:?}", exp),
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
pub enum Inj {
    L,
    R
}

impl fmt::Display for Inj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::L => write!(f, "L"),
            Self::R => write!(f, "R"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Str(String),
    Var(usize),
    Fun(Func),
    App(Box<Expr>, Box<Expr>),
    Inj(Inj, Type, Box<Expr>),
    Con(Box<Expr>, Box<Expr>),
    TypFun(TypeFunc),
    TypApp(Box<Expr>, Type),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(int) => write!(f, "{}", int),
            Self::Str(str) => write!(f, "\"{}\"", str),
            Self::Var(var) => write!(f, "{}\u{20d6}", var),
            Self::Fun(fun) => write!(f, "(λ _ : {} →  {})", fun.typ, fun.exp),
            Self::App(fun, arg) => write!(f, "({} {})", fun, arg),
            Self::Inj(inj, typ, exp) => write!(f, "({} {})", inj, exp),
            Self::Con(lexp, rexp) => write!(f, "({} , {})", lexp, rexp),
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
    pub fn con(fst: Expr, snd: Expr) -> Self {
        Self::Con(Box::new(fst), Box::new(snd))
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
    pub fn shift(exp: Expr) -> Expr {
        match exp {
            Self::Int(int) => Self::Int(int),
        }
    }
    pub fn shift(env: Option<usize>, exp: Expr, idx: usize) -> Expr {
        let inc_env = |env| {
            match env {
                None => Some(0),
                Some(env) => Some(env + 1)
            }
        };
        match exp {
            Self::Int(int) => Self::Int(int),
            Self::Str(str) => Self::Str(str),
            Self::Var(var) =>
                match env {
                    Some(env) =>
                        if var <= env {
                            Self::Var(var)
                        } else {
                            Self::Var(var + idx)
                        }
                    None => Self::Var(var + idx)
                }
            Self::Fun(fun) => Self::Fun(Func {
                exp: Box::new(Self::shift(inc_env(env), *fun.exp, idx)),
                ..fun
            }),
            Self::App(fun, arg) => Self::app(Self::shift(env, *fun, idx), Self::shift(env, *arg, idx)),
            Self::Inj(inj, typ, exp) => Self::Inj(inj, typ, Box::new(Self::shift(env, *exp, idx + 2))),
            Self::Con(fst, snd) => Self::Con(
                Box::new(Self::shift(env, *fst, idx + 1)),
                Box::new(Self::shift(env, *snd, idx + 1))
            ),
            Self::TypFun(typ_fun) => Self::TypFun(TypeFunc {
                exp: Box::new(Self::shift(env, *typ_fun.exp, idx))
            }),
            Self::TypApp(fun, arg) => Self::TypApp(
                Box::new(Self::shift(env, *fun, idx)),
                arg
            ),
        }
    }
    pub fn elaborate(ctx: Context, env: Environment, exp: Expr) -> Result<(core::Expr, Type)> {
        match exp {
            Self::Int(int) => Ok((core::Expr::Int(int), Type::Int)),
            Self::Str(str) => Ok((core::Expr::Str(str), Type::Str)),
            Self::Var(var) => match ctx.exp.iter().nth_back(var) {
                Some(typ) => Ok((core::Expr::Var(var), typ.clone())),
                None => Err(Error::UndefinedVariable(var))
            },
            Self::Fun(fun) => {
                let arg_typ = Type::evaluate(ctx.clone(), env.clone(), fun.typ.clone());
                let new_ctx = ctx.push(fun.typ.clone());
                let (exp, typ) = Self::elaborate(new_ctx, env.clone(), *fun.exp)?;
                Ok((core::Expr::fun(exp), Type::fun(fun.typ, typ)))
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
                        expect: arg_typ,
                        actual: fun_typ
                    }))
                }
            },
            Self::Inj(inj, typ, exp) => {
                let res_typ = Type::evaluate(ctx.clone(), env.clone(), typ)?;
                let (inj_exp, inj_typ) = Self::elaborate(ctx, env, *exp)?;
                let typ = match inj {
                    Inj::L => Type::sum(inj_typ, res_typ),
                    Inj::R => Type::sum(res_typ, inj_typ)
                };
                let inj_idx = match inj {
                    Inj::L => 1,
                    Inj::R => 0
                };
                use crate::core::Expr;
                let fun = Expr::fun;
                let app = Expr::app;
                let var = Expr::var;
                Ok((fun(fun(app(var(inj_idx), inj_exp))), typ))
            },
            Self::Con(fst, snd) => {
                let (fst_exp, fst_typ) = Self::elaborate(ctx.clone(), env.clone(), *fst)?;
                let (snd_exp, snd_typ) = Self::elaborate(ctx, env, *snd)?;
                let typ = Type::pro(fst_typ, snd_typ);
                use crate::core::Expr;
                let fun = Expr::fun;
                let app = Expr::app;
                let var = Expr::var;
                Ok((fun(app(app(var(0), fst_exp), snd_exp)), typ))
            },
            Self::TypFun(fun) => {
                let new_ctx = ctx.push(Type::Typ);
                let (exp, typ) = Self::elaborate(new_ctx, env.clone(), *fun.exp)?;
                Ok((exp, Type::ForAll(ForAll { env, typ: Box::new(typ) })))
            },
            Self::TypApp(fun, arg) => {
                match Self::elaborate(ctx.clone(), env.clone(), *fun)? {
                    (fun, Type::ForAll(for_all)) => {
                        let new_env = for_all.env.push(Type::evaluate(ctx.clone(), env, arg)?);
                        let typ = Type::evaluate(ctx, new_env, *for_all.typ)?;
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

        #[test]
        fn product_projection() {
            let fst_val = Expr::int(0);
            let snd_val = Expr::int(1);
            let con_val = Expr::con(fst_val, snd_val);
            let fst_prj = Expr::fun(Type::Int, Expr::fun(Type::Int, Expr::var(1)));
            let snd_prj = Expr::fun(Type::Int, Expr::fun(Type::Int, Expr::var(0)));
            let (exp, typ) =
                Expr::elaborate(Context::new(), Environment::new(), Expr::app(con_val.clone(), fst_prj)).unwrap();
            assert!(typ == Type::Int);
            let result =
                core::Expr::evaluate(core::Environment::new(), exp).unwrap();
            assert!(result == core::Expr::var(0));
            let (exp, typ) =
                Expr::elaborate(Context::new(), Environment::new(), Expr::app(con_val, snd_prj)).unwrap();
            assert!(typ == Type::Int);
            let result =
                core::Expr::evaluate(core::Environment::new(), exp).unwrap();
            assert!(result == core::Expr::var(1));
        }
    }

    mod display {
        use super::*;

        #[test]
        fn fun() {
            let fun = Expr::fun(Type::Int, Expr::var(0));
            assert_eq!(format!("{}", fun), "(λ _ : Int →  0⃖)");
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
