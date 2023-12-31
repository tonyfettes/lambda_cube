use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Int,
    Str,
    Var(String),
    Fun(Box<Type>, Box<Type>),
    ForAll(String, Box<Type>),
    Typ,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Str => write!(f, "Str"),
            Self::Var(var) => write!(f, "{}", var),
            Self::Fun(arg, ret) => write!(f, "{} -> {}", arg, ret),
            Self::ForAll(pat, exp) => write!(f, "∀ {} . {}", pat, exp),
            Self::Typ => write!(f, "∗"),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct TypedVar {
    pub var: String,
    pub typ: Type,
}

impl fmt::Display for TypedVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.var)?;
        write!(f, ":{}", self.typ)
    }
}

#[derive(PartialEq, Debug)]
pub struct FuncData {
    pub arg: TypedVar,
    pub body: Box<Term>,
}

impl fmt::Display for FuncData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}.{})", self.arg, self.body)
    }
}

#[derive(PartialEq, Debug)]
pub struct ApplyData {
    pub func: Box<Term>,
    pub arg: Box<Term>,
}

impl fmt::Display for ApplyData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.func, self.arg)
    }
}

#[derive(PartialEq, Debug)]
pub struct TypeApplyData {
    pub func: Box<Term>,
    pub arg: Box<Type>,
}

impl fmt::Display for TypeApplyData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} @ {}", self.func, self.arg)
    }
}

#[derive(PartialEq, Debug)]
pub enum Term {
    Int(i64),
    Str(String),
    Var(String), // The name of the variable
    Func(FuncData),
    Apply(ApplyData),
    TypeApply(TypeApplyData),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Int(num) => write!(f, "{}", num),
            Term::Str(s) => write!(f, "\"{}\"", s),
            Term::Var(var) => write!(f, "{}", var),
            Term::Func(func) => write!(f, "{}", func),
            Term::Apply(apply) => write!(f, "{}", apply),
            Term::TypeApply(apply) => write!(f, "{}", apply),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct RawAst {
    pub expr: Term,
}

impl fmt::Display for RawAst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_typed_var_display() {
        let typed_var = TypedVar {
            var: "x".to_string(),
            typ: Type::Int,
        };
        assert_eq!(format!("{}", typed_var), "x:Int");

        let typed_var = TypedVar {
            var: "y".to_string(),
            typ: Type::Typ,
        };
        assert_eq!(format!("{}", typed_var), "y:∗");
    }

    #[test]
    fn test_func_data_display() {
        let typed_var = TypedVar {
            var: "x".to_string(),
            typ: Type::Int,
        };
        let term = Term::Int(42);
        let func_data = FuncData {
            arg: typed_var,
            body: Box::new(term),
        };
        assert_eq!(format!("{}", func_data), "(x:Int.42)");
    }

    #[test]
    fn test_apply_data_display() {
        let term1 = Term::Var("f".to_string());
        let term2 = Term::Int(42);
        let apply_data = ApplyData {
            func: Box::new(term1),
            arg: Box::new(term2),
        };
        assert_eq!(format!("{}", apply_data), "f 42");
    }

    #[test]
    fn test_term_display() {
        let term = Term::Int(42);
        assert_eq!(format!("{}", term), "42");

        let term = Term::Str("Hello, world!".to_string());
        assert_eq!(format!("{}", term), "\"Hello, world!\"");

        let term = Term::Var("x".to_string());
        assert_eq!(format!("{}", term), "x");

        let typed_var = TypedVar {
            var: "y".to_string(),
            typ: Type::Int,
        };
        let term = Term::Func(FuncData {
            arg: typed_var,
            body: Box::new(Term::Int(42)),
        });
        assert_eq!(format!("{}", term), "(y:Int.42)");

        let term1 = Term::Var("f".to_string());
        let term2 = Term::Int(42);
        let apply_data = ApplyData {
            func: Box::new(term1),
            arg: Box::new(term2),
        };
        let term = Term::Apply(apply_data);
        assert_eq!(format!("{}", term), "f 42");
    }

    #[test]
    fn test_raw_ast_display() {
        let term1 = Term::Var("add".to_string());
        let term2 = Term::Int(42);
        let apply_data = ApplyData {
            func: Box::new(term1),
            arg: Box::new(term2),
        };
        let raw_ast = RawAst {
            expr: Term::Apply(apply_data),
        };
        assert_eq!(format!("{}", raw_ast), "add 42");
    }
}
