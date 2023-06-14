use std::collections::HashMap;

#[derive(Clone)]
pub enum Term<E> {
    Num(u64),
    Str(String),
    Var(String),
    Fun(E, String, Type, Box<Term<E>>),
    App(Box<Term<E>>, Box<Term<E>>),
}

#[derive(Clone)]
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

#[derive(Clone)]
pub enum Type {
    Num,
    Str,
    Fun(Box<Type>, Box<Type>)
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Num, Type::Num) => true,
            (Type::Str, Type::Str) => true,
            (Type::Fun(self_para, self_body),
             Type::Fun(other_para, other_body)) =>
                self_para.eq(other_para) && self_body.eq(self_body),
            _ => false
        }
    }
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
        Term::Var(name) => ctx.find(name),
        Term::Fun(_, name, para_type, body) => {
            let new_ctx = ctx.push(name.clone(), para_type.clone());
            let body_type = synthesize(&new_ctx, &body)?;
            Some(Type::Fun(Box::new(para_type.clone()), Box::new(body_type)))
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
