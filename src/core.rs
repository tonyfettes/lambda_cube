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

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

// We can definitly do some optimization over the vector - for now it's copied every time it's changed.
#[derive(Debug, Clone, PartialEq)]
pub struct Environment(Vec<Expr>);

impl std::fmt::Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (idx, typ) in self.0.iter().enumerate() {
            write!(f, "{} ↪ {}, ", idx, typ)?;
        }
        Ok(())
    }
}

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

#[derive(Clone)]
pub struct Ptr(Rc<dyn Fn(Environment) -> Expr>);

impl std::fmt::Debug for Ptr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<built-in function {}>", self)
    }
}

impl PartialEq for Ptr {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl std::fmt::Display for Ptr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", Rc::as_ptr(&self.0) as *const () as usize)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Inst {
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    IntMod,
}

impl std::fmt::Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IntAdd => write!(f, "+"),
            Self::IntSub => write!(f, "-"),
            Self::IntMul => write!(f, "*"),
            Self::IntDiv => write!(f, "/"),
            Self::IntMod => write!(f, "%"),
        }
    }
}

impl Inst {
    fn new(self: Self) -> Expr {
        Expr::fun(Expr::Fun(Func::Ins(self)))
    }
    fn evaluate(env: Environment, ins: Self) -> Expr {
        let opl = match env.get(1) {
            Some(Expr::Int(opl)) => opl,
            Some(_) => return Expr::err(Error::MismatchedType, Expr::Fun(Func::Ins(ins))),
            None => return Expr::err(Error::FreeVariable, Expr::Fun(Func::Ins(ins))),
        };
        let opr = match env.get(0) {
            Some(Expr::Int(opr)) => opr,
            Some(_) => return Expr::err(Error::MismatchedType, Expr::Fun(Func::Ins(ins))),
            None => return Expr::err(Error::FreeVariable, Expr::Fun(Func::Ins(ins))),
        };
        match ins {
            Self::IntAdd => Expr::Int(opl + opr),
            Self::IntSub => Expr::Int(opl - opr),
            Self::IntMul => Expr::Int(opl * opr),
            Self::IntDiv => Expr::Int(opl / opr),
            Self::IntMod => Expr::Int(opl % opr),
        }
    }
}

// We're using function pointer (dynamic dispatch) here anyway, so maybe it's better to define a
// Eval trait and use `dyn Eval` here?
#[derive(Debug, Clone, PartialEq)]
pub enum Func {
    Ptr(Ptr),
    Exp(Box<Expr>),
    Ins(Inst),
    Int(i64),
    Nat(u64),
    Str(String),
    Nul
}

impl Func {
    fn exp(exp: Expr) -> Func {
        Self::Exp(Box::new(exp))
    }
}

impl std::fmt::Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ptr(ptr) => write!(f, "{}", ptr),
            Self::Exp(exp) => write!(f, "{}", exp),
            Self::Ins(ins) => write!(f, "{}", ins),
            Self::Int(int) => write!(f, "{}", int),
            Self::Nat(nat) => write!(f, "{}", nat),
            Self::Str(str) => write!(f, "\"{}\"", str),
            Self::Nul      => write!(f, "!"),
        }
    }
}

impl Func {
    pub fn evaluate(env: Environment, fun: Func) -> Expr {
        match fun {
            Self::Ptr(ptr) => (*ptr.0)(env),
            Self::Ins(ins) => Inst::evaluate(env, ins),
            Self::Exp(exp) => Expr::evaluate(env, *exp),
            Self::Int(int) => {
                let opl = match env.get(1) {
                    Some(opl) => opl,
                    None => return Expr::err(Error::MismatchedType, Expr::Fun(Self::Int(int)))
                };
                let opr = match env.get(0) {
                    Some(opr) => opr,
                    None => return Expr::err(Error::MismatchedType, Expr::Fun(Self::Int(int)))
                };
                if int >= 0 {
                    let nat = Expr::Nat(int as u64);
                    Expr::evaluate(env.put(nat), opl)
                } else {
                    let nat = Expr::Nat((-int - 1) as u64);
                    Expr::evaluate(env.put(nat), opl)
                }
            }
            Self::Nat(nat) => {
                let opl = match env.get(1) {
                    Some(opl) => opl,
                    None => return Expr::err(Error::MismatchedType, Expr::Fun(Self::Nat(nat)))
                };
                let opr = match env.get(0) {
                    Some(opr) => opr,
                    None => return Expr::err(Error::MismatchedType, Expr::Fun(Self::Nat(nat)))
                };
                if nat == 0 {
                    Expr::evaluate(env.put(Expr::Nat(0)), opl)
                } else {
                    Expr::evaluate(env.put(Expr::Nat(nat - 1)), opl)
                }
            }
            Self::Str(str) => {
                let opl = match env.get(1) {
                    Some(opl) => opl,
                    None => return Expr::err(Error::MismatchedType, Expr::Fun(Self::Str(str)))
                };
                let opr = match env.get(0) {
                    Some(opr) => opr,
                    None => return Expr::err(Error::MismatchedType, Expr::Fun(Self::Str(str)))
                };
                if str.len() == 0 {
                    Expr::evaluate(env.put(Expr::Fun(Func::Nul)), opl)
                } else {
                    let str: String = str.chars().skip(1).collect();
                    Expr::evaluate(env.put(Expr::Str(str)), opr)
                }
            }
            Self::Nul => panic!("Nul!"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Nat(u64),
    Int(i64),
    Str(String),
    Var(usize),
    Fun(Func),
    Env(Environment, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Err(Error, Box<Expr>)
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nat(int) => write!(f, "{}", int),
            Self::Int(int) => write!(f, "{}", int),
            Self::Str(str) => write!(f, "\"{}\"", str),
            Self::Var(var) => write!(f, "{}\u{20d6}", var),
            Self::Fun(fun) => write!(f, "(λ · → {})", fun),
            Self::Env(env, exp) => write!(f, "[{}]{}", env, exp),
            Self::App(fun, arg) => write!(f, "({} {})", fun, arg),
            Self::Err(err, exp) => write!(f, "{}! {}", err, exp)
        }
    }
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
        Self::Fun(Func::exp(exp))
    }
    pub fn app(fun: Self, arg: Self) -> Self {
        Self::App(Box::new(fun), Box::new(arg))
    }
    pub fn env(env: Environment, exp: Self) -> Self {
        Self::Env(env, Box::new(exp))
    }
    pub fn err(err: Error, exp: Expr) -> Self {
        Self::Err(err, Box::new(exp))
    }
}

impl Expr {
    // Evaluates an expression. It shall annotate errornous nodes.
    fn evaluate(env: Environment, exp: Expr) -> Expr {
        println!("======== evaluate =========");
        println!("env = {}", env);
        println!("exp = {}", exp);
        match exp {
            Self::Nat(nat) => Self::Nat(nat),
            Self::Int(int) => Self::Int(int),
            Self::Str(str) => Self::Str(str),
            Self::Var(var) => match env.get(var) {
                Some(exp) => exp,
                None => Self::err(Error::FreeVariable, Self::Var(var))
            }
            Self::Fun(fun) => Self::env(env, Self::Fun(fun)),
            Self::App(fun, arg) => {
                let fun = Self::evaluate(env.clone(), *fun);
                let arg = Self::evaluate(env.clone(), *arg);
                match fun {
                    Self::Env(env, exp) => match *exp {
                        Self::Fun(fun) => {
                            Func::evaluate(env.put(arg), fun)
                        }
                        exp => Self::err(Error::MismatchedType, exp)
                    }
                    Self::Fun(fun) => {
                        Func::evaluate(Environment::new().put(arg), fun)
                    }
                    Self::Int(int) => {
                        Self::evaluate(Environment::new().put(arg), Self::Fun(Func::Int(int)))
                    }
                    Self::Nat(nat) => {
                        Self::evaluate(Environment::new().put(arg), Self::Fun(Func::Nat(nat)))
                    }
                    Self::Err(err, exp) => Self::app(Self::Err(err, exp), arg),
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

            assert_eq!(Expr::evaluate(Environment::new(), n_zero), Expr::evaluate(Environment::new(), zero));
        }

        fn host(f: impl Fn(Environment) -> Expr + 'static) -> Expr {
            Expr::Fun(Func::Ptr(Ptr(Rc::new(f))))
        }

        #[test]
        fn hosting() {
            let one = Expr::Int(1);
            let two = Expr::Int(2);
            let add = host(|env| match env.get(0) {
                Some(Expr::Int(a)) => {
                    let f = host(move |env| match env.get(0) {
                        Some(Expr::Int(b)) => Expr::Int(a + b),
                        Some(exp) => Expr::err(Error::MismatchedType, exp),
                        None => Expr::err(Error::FreeVariable, Expr::var(0))
                    });
                    Expr::env(env, f)
                }
                Some(exp) => Expr::err(Error::MismatchedType, exp),
                None => Expr::err(Error::FreeVariable, Expr::var(0))
            });
            // ((+ 1) 2)
            let exp = Expr::app(Expr::app(add, one), two);
            let result = Expr::evaluate(Environment::new(), exp);
            assert_eq!(result, Expr::Int(3));
        }

        #[test]
        fn arithmetic() {
            let one = Expr::Int(1);
            let two = Expr::Int(2);
            let add = Inst::new(Inst::IntAdd);
            // ((+ 1) 2)
            let exp = Expr::app(Expr::app(add, one), two);
            let result = Expr::evaluate(Environment::new(), exp);
            assert_eq!(result, Expr::Int(3));
        }

        #[test]
        fn int_match() {
            let oct = Expr::Int(10);
            let hex = Expr::Int(16);
            let zero = Expr::Nat(0);
            let exp = Expr::app(Expr::app(zero, oct.clone()), hex);
            let result = Expr::evaluate(Environment::new(), exp);
            assert_eq!(result, oct);
        }
    }
}
