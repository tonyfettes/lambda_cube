use std::collections::HashMap;
use std::fmt;

#[derive(Clone)]
pub enum Term {
    Var(String),
    Fun(Environment, String, Box<Term>),
    App(Box<Term>, Box<Term>),
}

impl Term {
    pub fn eval(envt: Environment, expr: Term) -> Term {
        match expr {
            Term::Var(name) => match envt.find(&name) {
                Some(term) => Self::eval(envt, term),
                None => Term::Var(name),
            },
            Term::Fun(_, patt, expr) => Term::Fun(envt, patt, expr),
            Term::App(lier, cant) => {
                let lier_result = Self::eval(envt, *lier);
                match lier_result {
                    Term::Fun(next_envt, patt, body) => {
                        let envt = next_envt.push(patt, *body.clone());
                        Self::eval(envt, *body)
                    }
                    _ => Term::App(Box::new(lier_result), cant),
                }
            }
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(name) => {
                write!(f, "{}", name)
            }
            Self::Fun(_, patt, expr) => {
                write!(f, "({}. {})", patt, expr)
            }
            Self::App(fun, arg) => {
                write!(f, "({} {})", fun, arg)
            }
        }
    }
}

#[derive(Clone)]
pub struct Environment(HashMap<String, Term>);

impl Environment {
    pub fn init() -> Self {
        Self(HashMap::new())
    }
    pub fn find(&self, name: &String) -> Option<Term> {
        match self.0.get(name) {
            Some(term) => Some(term.clone()),
            None => None,
        }
    }
    pub fn push(&self, name: String, term: Term) -> Self {
        let mut next_envt = self.0.clone();
        next_envt.insert(name, term);
        Self(next_envt)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_term_display() {
        let id = Term::Fun(
            Environment::init(),
            "x".to_string(),
            Box::new(Term::Var("x".to_string())),
        );
        let term = Term::App(Box::new(id.clone()), Box::new(id));
        assert_eq!(format!("{}", term), "((x. x) (x. x))");
    }
}
