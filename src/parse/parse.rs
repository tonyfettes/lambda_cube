use crate::parse::ast::{ApplyData, FuncData, RawAst, Term, TypedVar};
use crate::parse::number::number;
use crate::parse::string::string_with_escape;
use nom::character::complete::multispace0;
use nom::Compare;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1},
    combinator::{all_consuming, map, opt, recognize},
    error::{Error, ErrorKind, ParseError},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, preceded, terminated},
    AsChar, Err, IResult, InputTake, InputTakeAtPosition, Parser,
};
use std::boxed::Box;

// Parser for identifiers
pub fn identifier(input: &str) -> IResult<&str, String> {
    println!("id {}", input);
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        str::to_string,
    )(input)
}

// Parser for types
fn type_parser(input: &str) -> IResult<&str, String> {
    println!("typ {}", input);
    identifier(input)
}

// Parser for raw variables
fn variable(input: &str) -> IResult<&str, String> {
    println!("var {}", input);
    identifier(input)
}

// Parser for variables with optional type annotation
fn typed_variable(input: &str) -> IResult<&str, TypedVar> {
    println!("typ_var {}", input);
    let (input, var) = variable(input)?;
    let (input, typ) = opt(preceded(tag(":"), type_parser))(input)?;

    Ok((input, TypedVar { var, typ }))
}

// Parser for function definitions
fn function(input: &str) -> IResult<&str, FuncData> {
    println!("fun {}", input);
    let (input, arg) = terminated(typed_variable, tag("."))(input)?;
    let (input, body) = expression(input)?;
    Ok((
        input,
        FuncData {
            arg,
            body: Box::new(body),
        },
    ))
}

// Parser for function applications
fn application(input: &str) -> IResult<&str, ApplyData> {
    println!("app {}", input);
    let (input, mut terms) = separated_list1(tag(" "), map(expression_no_apply, Box::new))(input)?;
    if terms.len() <= 1 {
        // At least 2 terms are expected
        Err(Err::Error(Error::from_error_kind(
            input,
            ErrorKind::SeparatedList,
        )))
    } else {
        // build up AST. Apply is left associate
        let remainder = terms.split_off(2);
        // It's safe to `unwrap` here because at least 2 elements are there
        let first = ApplyData {
            arg: terms.pop().unwrap(),
            func: terms.pop().unwrap(),
        };

        let apply = remainder.into_iter().fold(first, |acc, xs| ApplyData {
            func: Box::new(Term::Apply(acc)),
            arg: xs,
        });
        Ok((input, apply))
    }
}

fn parenthesized<'a, I: Clone, O, E: ParseError<I>, F>(f: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: Parser<I, O, E>,
    I: InputTake + Compare<&'static str>,
{
    delimited(tag("("), f, tag(")"))
}

fn token<I: Clone, O, E: ParseError<I>, F>(f: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: Parser<I, O, E>,
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    delimited(multispace0, f, multispace0)
}

fn expression_no_apply(input: &str) -> IResult<&str, Term> {
    println!("exp_no_app {}", input);
    alt((
        parenthesized(expression),
        map(function, Term::Func),
        map(string_with_escape, Term::Str),
        map(number, Term::Int),
        map(variable, Term::Var),
    ))(input)
}

// Parser for expressions
fn expression(input: &str) -> IResult<&str, Term> {
    println!("exp {}", input);
    alt((map(application, Term::Apply), expression_no_apply))(input)
}

// Helper function to parse an entire program
fn parse_program(input: &str) -> IResult<&str, RawAst> {
    println!("program {}", input);
    all_consuming(map(expression, |expr| RawAst { expr }))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_untyped_func() {
        let input = "x.x";
        let (input, result) = function(input).unwrap();
        assert_eq!(input, "");
        assert_eq!(
            result.arg,
            TypedVar {
                var: "x".to_string(),
                typ: None
            }
        );
        assert_eq!(*result.body, Term::Var("x".to_string()))
    }

    #[test]
    fn test_typed_func() {
        let input = "x:Int.x";
        let (input, result) = function(input).unwrap();
        assert_eq!(input, "");
        assert_eq!(
            result.arg,
            TypedVar {
                var: "x".to_string(),
                typ: Some("Int".to_string())
            }
        );
        assert_eq!(*result.body, Term::Var("x".to_string()))
    }

    #[test]
    fn test_simple_apply() {
        let input = "x y";
        let (input, result) = application(input).unwrap();
        assert_eq!(input, "");
        assert_eq!(*result.func, Term::Var("x".to_string()));
        assert_eq!(*result.arg, Term::Var("y".to_string()));
    }

    #[test]
    fn test_nested_apply() {
        let input = "x y z";
        let (input, result) = application(input).unwrap();
        assert_eq!(input, "");
        assert_eq!(result.arg, Box::new(Term::Var("z".to_string())));
        match *result.func {
            Term::Apply(apply) => {
                assert_eq!(*apply.func, Term::Var("x".to_string()));
                assert_eq!(*apply.arg, Term::Var("y".to_string()));
            }
            _ => unreachable!("Not parsed as nested applies"),
        }
    }

    #[test]
    fn test_redundent_brackets() {
        let input_parenthesized = "(x y)";
        let input_not_parenthesized = "x y";
        let result_parenthesized = parse_program(input_parenthesized).unwrap();
        let result_not_parenthesized = parse_program(input_not_parenthesized).unwrap();
        assert_eq!(result_parenthesized, result_not_parenthesized)
    }

    #[test]
    fn test_parenthesized_function_apply() {
        let input = "(x.x) y";
        let (input, result) = parse_program(input).unwrap();
        assert_eq!(input, "");
        assert_eq!(
            result,
            RawAst {
                expr: Term::Apply(ApplyData {
                    func: Box::new(Term::Func(FuncData {
                        arg: TypedVar {
                            var: "x".to_string(),
                            typ: None
                        },
                        body: Box::new(Term::Var("x".to_string()))
                    })),
                    arg: Box::new(Term::Var("y".to_string()))
                })
            }
        )
    }
}
