use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum Error {
    UndefinedVariable(String),
    MismatchedType(Type)
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Clone)]
enum FuncImpl<Env> {
    Host(Rc<dyn Fn(Term<Env>) -> Result<Term<Env>>>, Type),
    Term(Box<Term<Env>>)
}

impl<Env> PartialEq for FuncImpl<Env> {
    fn eq(&self, _: &FuncImpl<Env>) -> bool {
        false
    }
}

impl<Env> fmt::Display for FuncImpl<Env> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Host(_, _) => write!(f, "<built-in function>"),
            Self::Term(term) => write!(f, "{}", term),
        }
    }
}

impl<Env> fmt::Debug for FuncImpl<Env> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Host(_, _) => write!(f, "<built-in function>"),
            Self::Term(term) => write!(f, "{}", term),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func<Env> {
    env: Env,
    pat: String,
    typ: Type,
    exp: FuncImpl<Env>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term<Env> {
    Num(u64),
    Str(String),
    Var(String),
    Fun(Func<Env>),
    App(Box<Term<Env>>, Box<Term<Env>>),
}

impl<Env> fmt::Display for Term<Env> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Num(num) => {
                write!(f, "{}", num)
            },
            Self::Str(str) => {
                write!(f, "\"{}\"", str)
            }
            Self::Var(name) => {
                write!(f, "{}", name)
            }
            Self::Fun(fun) => {
                write!(f, "({} : {} . {})", fun.pat, fun.typ, fun.exp)
            }
            Self::App(fun, arg) => {
                write!(f, "({} {})", fun, arg)
            }
        }
    }
}

impl Term<Environment> {
    pub fn eval(env: Environment, expr: Term<Environment>) -> Result<Term<Environment>> {
        match expr {
            Self::Num(num) => Ok(Self::Num(num)),
            Self::Str(str) => Ok(Self::Str(str)),
            Self::Var(name) => match env.find(&name) {
                Some(term) => Self::eval(env, term),
                None => Ok(Self::Var(name))
            }
            Self::Fun(fun) => Ok(Self::Fun(Func::<Environment> {
                env,
                pat: fun.pat,
                typ: fun.typ,
                exp: fun.exp
            })),
            Self::App(lier, cant) => {
                let lier = Self::eval(env, *lier)?;
                match lier {
                    Self::Fun(fun) => match fun.exp {
                        FuncImpl::Host(exp, _) => (*exp)(*cant),
                        FuncImpl::Term(exp) => {
                            let env = fun.env.push(fun.pat, *cant);
                            Self::eval(env, *exp)
                        }
                    },
                    _ => Ok(Self::App(Box::new(lier), cant)),
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment(HashMap<String, Term<Environment>>);

impl Environment {
    pub fn init() -> Self {
        Self(HashMap::new())
    }
    pub fn find(&self, name: &String) -> Option<Term<Environment>> {
        match self.0.get(name) {
            Some(term) => Some(term.clone()),
            None => None
        }
    }
    pub fn push(&self, name: String, term: Term<Environment>) -> Self {
        let mut next_envt = self.0.clone();
        next_envt.insert(name, term);
        Self(next_envt)
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Num,
    Str,
    Fun(Box<Type>, Box<Type>)
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Num => write!(f, "Num"),
            Self::Str => write!(f, "Str"),
            Self::Fun(pat, exp) => write!(f, "{} => {}", pat, exp),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Num, Type::Num) => true,
            (Type::Str, Type::Str) => true,
            (Type::Fun(self_para, self_body),
             Type::Fun(other_para, other_body)) =>
                self_para.eq(other_para) && self_body.eq(other_body),
            _ => false
        }
    }
}

impl Type {
    pub fn num() -> Self { Self::Num }
    pub fn str() -> Self { Self::Str }
    pub fn fun(pat: Type, exp: Type) -> Self { Self::Fun(Box::new(pat), Box::new(exp)) }
}

pub struct Context(HashMap<String, Type>);

impl Context {
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

fn synthesize(ctx: &Context, term: &Term<Context>) -> Option<Type> {
    match term {
        Term::Num(_) => Some(Type::Num),
        Term::Str(_) => Some(Type::Str),
        Term::Var(var) => ctx.find(var),
        Term::Fun(fun) => {
            let exp_typ = 
                match &fun.exp {
                    FuncImpl::Host(_, typ) => typ.clone(),
                    FuncImpl::Term(exp) => {
                        let new_ctx = ctx.push(fun.pat.clone(), fun.typ.clone());
                        synthesize(&new_ctx, &*exp)?
                    }
                };
            Some(Type::Fun(Box::new(fun.typ.clone()), Box::new(exp_typ)))
        },
        Term::App(lier, cant) => {
            let lier = synthesize(&ctx, &lier)?;
            let cant = synthesize(&ctx, &cant)?;
            match lier {
                Type::Fun(para_type, body_type) => {
                    if *para_type == cant {
                        Some(*body_type)
                    } else {
                        None
                    }
                },
                _ => None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_term_eval() {
        let id = Term::Fun(Func::<Environment> {
            env: Environment::init(),
            pat: "x".to_string(),
            typ: Type::Num,
            exp: FuncImpl::Term(Box::new(Term::Var("x".to_string()))),
        });
        let one = Term::Num(1);
        let app = Term::App(Box::new(id), Box::new(one));
        let result = Term::eval(Environment::init(), app);
        assert_eq!(result, Ok(Term::Num(1)));
    }

    #[test]
    fn test_term_arithm() {
        let one = Term::<Environment>::Num(1);
        let two = Term::<Environment>::Num(2);
        let add_impl = |term| match term {
            Term::Num(a) => Ok(Term::Fun(Func {
                env: Environment::init(),
                pat: String::new(),
                typ: Type::Num,
                exp: FuncImpl::Host(Rc::new(move |term| match term {
                    Term::Num(b) => Ok(Term::Num(a + b)),
                    _ => Err(Error::MismatchedType(Type::Num)),
                }), Type::fun(Type::Num, Type::Num)),
            })),
            _ => Err(Error::MismatchedType(Type::Num)),
        };
        let plus = Term::Fun(Func {
            env: Environment::init(),
            pat: String::new(),
            typ: Type::Num,
            exp: FuncImpl::Host(Rc::new(add_impl), Type::fun(Type::Num, Type::fun(Type::Num, Type::Num)))
        });
        let ap_left = Term::App(Box::new(plus), Box::new(one));
        let ap_right = Term::App(Box::new(ap_left), Box::new(two));
        let result = Term::eval(Environment::init(), ap_right);
        assert_eq!(result, Ok(Term::Num(3)));
    }

    #[test]
    fn test_term_display() {
        let id = Term::Fun(Func::<()> {
            env: (),
            pat: "x".to_string(),
            typ: Type::Num,
            exp: FuncImpl::Term(Box::new(Term::Var("x".to_string()))),
        });
        let term = Term::App(Box::new(id.clone()), Box::new(id));
        assert_eq!(format!("{}", term), "((x : Num . x) (x : Num . x))");
    }
}
