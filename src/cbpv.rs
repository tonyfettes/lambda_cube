use super::core;

#[derive(Debug, PartialEq)]
pub enum Error {
    UndefinedVariable(usize),
    MismatchedType(Type)
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq)]
pub struct Context(Vec<ValueType>);

impl Context {
    pub fn new() -> Self {
        Self(Vec::new())
    }
    pub fn push(mut self, typ: ValueType) -> Self {
        self.0.push(typ);
        self
    }
}

// Type alias mapping, i.e. A -> B means type variable A is an alias of type B.
#[derive(Debug, Clone, PartialEq)]
pub struct Environment(Vec<ValueType>);

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Int,
    Str,
    Thunk(Box<ComputationType>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComputationType {
    Put(ValueType),
    Get(Box<ComputationType>, Box<ComputationType>),
    Force(ValueType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Value(ValueType),
    Computation(ComputationType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueExpr {
    Int(i64),
    Str(String),
    Var(usize),
    Thunk(Box<ComputationExpr>),
}

impl ValueExpr {
    pub fn elaborate(ctx: Context, env: Environment, exp: Self) -> Result<(super::core::Expr, ValueType)> {
        match exp {
            Self::Int(int) =>
                Ok((core::Expr::Int(int), ValueType::Int)),
            Self::Str(str) =>
                Ok((core::Expr::Str(str), ValueType::Str)),
            Self::Var(var) => match ctx.0.iter().nth_back(var) {
                Some(typ) => Ok((core::Expr::Var(var), typ.clone())),
                None => Err(Error::UndefinedVariable(var))
            },
            Self::Thunk(exp) => {
                let (exp, typ) = ComputationExpr::elaborate(ctx, env, *exp)?;
                Ok((core::Expr::fun(exp), ValueType::Thunk(Box::new(typ))))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComputationExpr {
    Put(ValueExpr),
    Get(Box<ComputationExpr>, Box<ComputationExpr>),
    Force(ValueExpr),
}

impl ComputationExpr {
    pub fn elaborate(ctx: Context, env: Environment, exp: Self) -> Result<(core::Expr, ComputationType)> {
        match exp {
            Self::Put(val) => {
                let (exp, typ) = ValueExpr::elaborate(ctx, env, val)?;
                Ok((exp, ComputationType::Put(typ)))
            },
            Self::Get(ret, exp) => match Self::elaborate(ctx.clone(), env.clone(), *ret)? {
                (rexp, ComputationType::Put(rtyp)) => {
                    let ctx = ctx.push(rtyp);
                    let (bexp, btyp) = Self::elaborate(ctx, env, *exp)?;
                    Ok((core::Expr::app(core::Expr::fun(bexp), rexp), btyp))
                },
                (_, typ) => Err(Error::MismatchedType(Type::Computation(typ)))
            },
            Self::Force(val) => match ValueExpr::elaborate(ctx, env, val)? {
                (exp, ValueType::Thunk(typ)) => {
                    Ok((core::Expr::app(exp, core::Expr::Int(0)), *typ))
                },
                (_, typ) => Err(Error::MismatchedType(Type::Value(typ)))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Value(ValueExpr),
    Computation(ComputationExpr)
}

impl Expr {
    pub fn elaborate(ctx: Context, env: Environment, exp: Expr) -> Result<(super::core::Expr, Type)> {
        match exp {
            Self::Value(value) => {
                let (exp, typ) = ValueExpr::elaborate(ctx, env, value)?;
                Ok((exp, Type::Value(typ)))
            },
            Self::Computation(computation) => {
                let (exp, typ) = ComputationExpr::elaborate(ctx, env, computation)?;
                Ok((exp, Type::Computation(typ)))
            },
        }
    }
}

mod tests {
}
