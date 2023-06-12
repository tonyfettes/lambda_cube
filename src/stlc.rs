use std::collections::HashMap;

#[derive(Clone)]
pub enum Term {
    Var(String),
    Fun(Envt, String, Box<Term>),
    App(Box<Term>, Box<Term>),
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
