use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case},
    character::complete::one_of,
    combinator::{map_res, recognize},
    error::ParseError,
    multi::{many0, many1},
    sequence::{pair, preceded, terminated},
    Err, IResult, Parser,
};

pub fn number(input: &str) -> IResult<&str, i64> {
    alt((
        match_base_n("b", 2),
        match_base_n("o", 8),
        match_base_n("d", 10),
        match_base_n("x", 16),
    ))(input)
}

fn match_base_n(c: &str, base: u32) -> impl Fn(&str) -> IResult<&str, i64> + '_ {
    let index: usize = (base + base.saturating_sub(10)) as usize;
    move |i: &str| {
        map_res(
            preceded(
                opt_if(base == 10, pair(tag("0"), tag_no_case(c))),
                recognize(many1(terminated(
                    one_of(&"0123456789aAbBcCdDeEfF"[0..index]),
                    many0(tag("_")),
                ))),
            ),
            |out: &str| i64::from_str_radix(&str::replace(&out, "_", ""), base),
        )(i)
    }
}

fn opt_if<I: Clone, O, E: ParseError<I>, F>(
    b: bool,
    mut f: F,
) -> impl FnMut(I) -> IResult<I, Option<O>, E>
where
    F: Parser<I, O, E>,
{
    move |input: I| {
        let i = input.clone();
        match (f.parse(input), b) {
            (Ok((i, o)), _) => Ok((i, Some(o))),
            (Err(Err::Error(_)), true) => Ok((i, None)),
            (Err(e), _) => Err(e),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_binary_number() {
        let input = "0b101";
        let (input, result) = number(input).unwrap();
        assert_eq!(input, "");
        assert_eq!(result, 0b101);
    }
}
