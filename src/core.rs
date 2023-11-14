/// Core Calculus, only for dynamic evaluation

use std::rc::Rc;
use std::vec::Vec;

// We don't include detailed information here, since well-typed program should not throw these
// errors during runtime.
#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    FreeVariable,
    MismatchedType,
    DividedByZero,
}

// We can definitly do some optimization over the vector - for now it's copied every time it's changed.
#[derive(Debug, Clone, PartialEq)]
pub struct Environment(Vec<Expr>);

impl Environment {
    pub fn new() -> Self {
        Self(Vec::new())
    }
    pub fn get(&self, name: usize) -> Option<Expr> {
        Some(self.0.get(self.0.len() - name - 1)?.clone())
    }
    pub fn put(mut self, expr: Expr) -> Environment {
        self.0.push(expr);
        self
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

// We're using function pointer (dynamic dispatch) here anyway, so maybe it's better to define a
// Eval trait and use `dyn Eval` here?
#[derive(Clone)]
pub enum Implementation {
    Host(Rc<dyn Fn(Environment, Expr) -> Expr>),
    Expr(Box<Expr>),
}

impl PartialEq for Implementation {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Host(self_rc), Self::Host(other_rc)) => Rc::ptr_eq(self_rc, other_rc),
            (Self::Expr(self_exp), Self::Expr(other_exp)) => self_exp == other_exp,
            _ => false
        }
    }
}

impl std::fmt::Debug for Implementation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Host(_) => write!(f, "<built-in function>"),
            Self::Expr(exp) => write!(f, "{:?}", exp),
        }
    }
}

impl Implementation {
    pub fn evaluate(env: Environment, fun: Implementation, arg: Expr) -> Expr {
        match fun {
            Self::Host(exp) => (*exp)(env, arg),
            Self::Expr(exp) => Expr::evaluate(env.put(arg), *exp)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Str(String),
    Var(usize),
    Fun(Implementation),
    Env(Environment, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Err(Error, Box<Expr>)
}

impl Expr {
    pub fn int(int: i64) -> Self {
        Self::Int(int)
    }
    pub fn str(str: &str) -> Self {
        Self::Str(str.to_string())
    }
    pub fn var(var: usize) -> Self {
        Self::Var(var)
    }
    pub fn fun(exp: Self) -> Self {
        Self::Fun(Implementation::Expr(Box::new(exp)))
    }
    pub fn app(fun: Self, arg: Self) -> Self {
        Self::App(Box::new(fun), Box::new(arg))
    }
    pub fn env(env: Environment, exp: Self) -> Self {
        Self::Env(env, Box::new(exp))
    }
    pub fn err(err: Error, exp: Self) -> Self {
        Self::Err(err, Box::new(exp))
    }
}

impl Expr {
    // Evaluates an expression. It shall annotate errornous nodes.
    pub fn evaluate(env: Environment, exp: Expr) -> Expr {
        match exp {
            Self::Int(int) => Self::Int(int),
            Self::Str(str) => Self::Str(str),
            Self::Var(var) => match env.get(var) {
                Some(exp) => exp,
                None => Self::err(Error::FreeVariable, Self::Var(var))
            },
            Self::Fun(fun) => Self::env(env, Self::Fun(fun)),
            Self::App(fun, arg) => {
                let fun = Self::evaluate(env.clone(), *fun);
                let arg = Self::evaluate(env, *arg);
                match fun {
                    // TODO: We copied `env` here, could we save it?
                    Self::Env(env, exp) => match *exp {
                        Self::Fun(fun) => Implementation::evaluate(env, fun, arg),
                        exp => Self::err(Error::MismatchedType, exp)
                    }
                    exp => Self::err(Error::MismatchedType, exp)
                }
            }
            Self::Env(env, exp) => Self::evaluate(env, *exp),
            Self::Err(err, exp) => Self::Err(err, exp)
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
            assert_eq!(result, Expr::int(1));
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

            assert_eq!(Expr::evaluate(Environment::new(), n_zero), Expr::env(Environment::new(), zero));
        }

        fn host(f: impl Fn(Environment, Expr) -> Expr + 'static) -> Expr {
            Expr::Fun(Implementation::Host(Rc::new(f)))
        }

        #[test]
        fn arithmetic() {
            let one = Expr::Int(1);
            let two = Expr::Int(2);
            let add = host(|env, exp| match exp {
                Expr::Int(a) => {
                    let f = host(move |_, exp| match exp {
                        Expr::Int(b) => Expr::Int(a + b),
                        exp => Expr::err(Error::MismatchedType, exp),
                    });
                    Expr::env(env, f)
                }
                exp => Expr::err(Error::MismatchedType, exp),
            });
            // ((+ 1) 2)
            let exp = Expr::app(Expr::app(add, one), two);
            let result = Expr::evaluate(Environment::new(), exp);
            assert_eq!(result, Expr::Int(3));
        }
    }
}
