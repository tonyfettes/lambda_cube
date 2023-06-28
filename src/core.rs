/// Core Calculus, only for dynamic evaluation

use std::rc::Rc;
use std::vec::Vec;

// We don't include detailed information here, since well-typed program should not throw these
// errors during runtime.
#[derive(Debug, PartialEq)]
pub enum Error {
    UndefinedVariable,
    MismatchedType,
}

type Result<T> = std::result::Result<T, Error>;

// We can definitly do some optimization over the vector - for now it's copied every time it's changed.
#[derive(Debug, Clone, PartialEq)]
pub struct Environment(Vec<Expr>);

impl Environment {
    pub fn new() -> Self {
        Self(Vec::new())
    }
    pub fn find(&self, name: usize) -> Option<Expr> {
        Some(self.0.get(self.0.len() - name - 1)?.clone())
    }
    pub fn push(mut self, expr: Expr) -> Environment {
        self.0.push(expr);
        self
    }
}

// We're using function pointer (dynamic dispatch) here anyway, so maybe it's better to define a
// Eval trait and use `dyn Eval` here?
#[derive(Clone)]
pub enum FuncImpl {
    Host(Rc<dyn Fn(Expr) -> Result<Expr>>),
    Expr(Box<Expr>),
}

impl PartialEq for FuncImpl {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Host(self_rc), Self::Host(other_rc)) => Rc::ptr_eq(self_rc, other_rc),
            (Self::Expr(self_exp), Self::Expr(other_exp)) => self_exp == other_exp,
            _ => false
        }
    }
}

impl std::fmt::Debug for FuncImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Host(_) => write!(f, "<built-in function>"),
            Self::Expr(exp) => write!(f, "{:?}", exp),
        }
    }
}

impl FuncImpl {
    pub fn evaluate(env: Environment, exp: FuncImpl, arg: Expr) -> Result<Expr> {
        match exp {
            Self::Host(exp) => (*exp)(arg),
            Self::Expr(exp) => Expr::evaluate(env.push(arg), *exp)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub env: Environment,
    pub exp: FuncImpl,
}

impl Func {
    pub fn evaluate(fun: Func, arg: Expr) -> Result<Expr> {
        FuncImpl::evaluate(fun.env, fun.exp, arg)
    }
}

// TODO: Use De Bruiji index.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Str(String),
    Var(usize),
    Fun(Func),
    App(Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn int(int: i64) -> Self { Self::Int(int) }
    pub fn str(str: &str) -> Self { Self::Str(str.to_string()) }
    pub fn var(var: usize) -> Self { Self::Var(var) }
    pub fn fun(exp: Self) -> Self {
        Self::Fun(Func {
            env: Environment::new(),
            exp: FuncImpl::Expr(Box::new(exp)),
        })
    }
    pub fn app(fun: Self, arg: Self) -> Self {
        Self::App(Box::new(fun), Box::new(arg))
    }
}

impl Expr {
    pub fn evaluate(env: Environment, exp: Expr) -> Result<Expr> {
        match exp {
            Self::Int(int) => Ok(Self::Int(int)),
            Self::Str(str) => Ok(Self::Str(str)),
            Self::Var(var) => match env.find(var) {
                Some(exp) => Ok(exp.clone()),
                None => Err(Error::UndefinedVariable),
            },
            Self::Fun(fun) => Ok(Self::Fun(Func { env, exp: fun.exp, ..fun })),
            Self::App(fun, arg) => match Self::evaluate(env.clone(), *fun)? {
                // TODO: We copied `env` here, could we save it?
                Self::Fun(fun) => Func::evaluate(fun, Self::evaluate(env, *arg)?),
                _ => Err(Error::MismatchedType)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod evaluate {
        use super::*;

        #[test]
        fn identity() {
            let id = Expr::fun(Expr::var(0));
            let one = Expr::int(1);
            let app = Expr::app(id, one);
            let result = Expr::evaluate(Environment::new(), app);
            assert_eq!(result, Ok(Expr::int(1)));
        }

        #[test]
        fn natural() {
            // x -> f -> x
            let zero = Expr::fun(Expr::fun(Expr::var(1)));
            // n -> x -> f -> f n
            let succ = Expr::fun(Expr::fun(Expr::fun(Expr::app(Expr::var(0), Expr::var(2)))));
            // x -> x
            let id = Expr::fun(Expr::var(0));
            // succ zero = x -> f -> f (x -> f -> x)
            let one = Expr::app(succ, zero.clone());
            // (succ zero) 1 = f -> f (x -> f -> x)
            let n_one = Expr::app(one, Expr::int(1));
            // ((succ zero) 1) id = id (x -> f -> x) = x -> f -> x = zero
            let n_zero = Expr::app(n_one, id);

            assert_eq!(Expr::evaluate(Environment::new(), n_zero), Ok(zero));
        }
    }

    // Test cases copied from Untyped Lambda Calculus.
    // #[test]
    // fn test_term_eval() {
    //     let id = Expr::Fun(Func {
    //         env: Environment::new(),
    //         pat: "x".to_string(),
    //         typ: Type::Int,
    //         exp: FuncImpl::Expr(Box::new(Expr::Var("x".to_string()))),
    //     });
    //     let one = Expr::Int(1);
    //     let app = Expr::App(Box::new(id), Box::new(one));
    //     let result = Expr::eval(Environment::new(), app);
    //     assert_eq!(result, Ok(Expr::Int(1)));
    // }

    // fn host(f: impl Fn(Expr) -> Result<Expr> + 'static, para_type: Type, body_type: Type) -> Expr {
    //     Expr::Fun(Func {
    //         env: Environment::new(),
    //         pat: String::new(),
    //         typ: para_type,
    //         exp: FuncImpl::Host(Rc::new(f), body_type)
    //     })
    // }

    // #[test]
    // fn test_term_arithm() {
    //     let one = Expr::Int(1);
    //     let two = Expr::Int(2);
    //     let plus = host(move |term| match term {
    //         Expr::Int(a) => Ok(host(move |term| match term {
    //             Expr::Int(b) => Ok(Expr::Int(a + b)),
    //             _ => Err(Error::MismatchedType(Type::Int)),
    //         }, Type::Int, Type::Int)),
    //         _ => Err(Error::MismatchedType(Type::Int)),
    //     }, Type::Int, Type::fun(Type::Int, Type::Int));
    //     let ap_left = Expr::App(Box::new(plus), Box::new(one));
    //     let ap_right = Expr::App(Box::new(ap_left), Box::new(two));
    //     let result = Expr::eval(Environment::new(), ap_right);
    //     assert_eq!(result, Ok(Expr::Int(3)));
    // }
}
