#[derive(PartialEq, Debug)]
pub struct TypedVar {
    pub var: String,
    pub typ: Option<String>,
}

#[derive(PartialEq, Debug)]
pub struct FuncData {
    pub arg: TypedVar,
    pub body: Box<Term>,
}

#[derive(PartialEq, Debug)]
pub struct ApplyData {
    pub func: Box<Term>,
    pub arg: Box<Term>,
}

#[derive(PartialEq, Debug)]
pub enum Term {
    Int(i64),
    Str(String),
    Var(String), // The name of the variable
    Func(FuncData),
    Apply(ApplyData),
}

#[derive(PartialEq, Debug)]
pub struct RawAst {
    pub expr: Term,
}
