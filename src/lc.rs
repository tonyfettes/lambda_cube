use std::collections::HashMap;

#[derive(Clone)]
pub enum Term {
    Var(String),
    Fun(Envt, String, Box<Term>),
    App(Box<Term>, Box<Term>),
}

impl Term {
    pub fn eval(envt: Envt, expr: Term) -> Term {
        match expr {
            Term::Var(name) =>
                match envt.find(&name) {
                    Some(term) => Self::eval(envt, term),
                    None => Term::Var(name)
                }
            Term::Fun(_, patt, expr) => Term::Fun(envt, patt, expr),
            Term::App(lier, cant) => {
                let lier_result = Self::eval(envt, *lier);
                match lier_result {
                    Term::Fun(next_envt, patt, body) => {
                        let envt = next_envt.push(patt, *body.clone());
                        Self::eval(envt, *body)
                    },
                    _ => Term::App(Box::new(lier_result), cant),
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct Envt(HashMap<String, Term>);

impl Envt {
    pub fn init() -> Envt {
        Envt(HashMap::new())
    }
    pub fn find(&self, name: &String) -> Option<Term> {
        match self.0.get(name) {
            Some(term) => Some(term.clone()),
            None => None
        }
    }
    pub fn push(&self, name: String, term: Term) -> Envt {
        let mut next_envt = self.0.clone();
        next_envt.insert(name, term);
        Envt(next_envt)
    }
}
