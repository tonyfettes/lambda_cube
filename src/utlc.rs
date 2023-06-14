use std::collections::HashMap;

#[derive(Clone)]
pub enum Term {
    Num(u64),
    Str(String),
    Var(String),
    Fun(Environment, String, Box<Term>),
    App(Box<Term>, Box<Term>),
}

impl Term {
    pub fn eval(env: Environment, expr: Term) -> Term {
        match expr {
            Term::Num(num) => Term::Num(num),
            Term::Str(str) => Term::Str(str),
            Term::Var(name) =>
                match env.find(&name) {
                    Some(term) => Self::eval(env, term),
                    None => Term::Var(name)
                }
            Term::Fun(_, patt, expr) => Term::Fun(env, patt, expr),
            Term::App(lier, cant) => {
                let lier_result = Self::eval(env, *lier);
                match lier_result {
                    Term::Fun(new_env, patt, body) => {
                        let env = new_env.push(patt, *body.clone());
                        Self::eval(env, *body)
                    },
                    _ => Term::App(Box::new(lier_result), cant),
                }
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
            None => None
        }
    }
    pub fn push(&self, name: String, term: Term) -> Self {
        let mut next_envt = self.0.clone();
        next_envt.insert(name, term);
        Self(next_envt)
    }
}
