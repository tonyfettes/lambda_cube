use std::collections::HashMap;

#[derive(Clone)]
pub enum Term {
    Var(String),
    Fun(Environment, String, Box<Term>),
    App(Box<Term>, Box<Term>),
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

#[derive(Clone)]
pub enum Type {
    Atom,
    Func(Box<Type>, Box<Type>)
}

pub struct Context(HashMap<String, Type>);

impl Context {
    pub fn find(&self, name: &String) -> Option<Type> {
        match self.0.get(name) {
            Some(term) => Some(term.clone()),
            None => None
        }
    }
}
