use nom::{
    character::complete::{char, one_of},
    combinator::{map_res, recognize},
    error::{FromExternalError, ParseError},
    multi::{many0, many1},
    sequence::delimited,
    IResult,
};

pub fn newline(input: &str) -> IResult<&str, ()> {
    let (input, _) = char('\n')(input)?;
    Ok((input, ()))
}

pub fn newline0(input: &str) -> IResult<&str, ()> {
    let (input, _) = many0(newline)(input)?;
    Ok((input, ()))
}

pub fn bind<'a, F: 'a, T: Copy, O, E: ParseError<&'a str> + FromExternalError<&'a str, ()>>(
    inner: F,
    val: T,
) -> impl FnMut(&'a str) -> IResult<&'a str, T, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    map_res(inner, move |_| -> Result<T, ()> { Ok(val) })
}

pub fn digits(input: &str) -> IResult<&str, &str> {
    recognize(many1(one_of("0123456789")))(input)
}

pub fn parse_i64(input: &str) -> IResult<&str, i64> {
    map_res(digits, |input: &str| i64::from_str_radix(input, 10))(input)
}

// TODO: Use generics to refactor parse_u64 and parse_i64
pub fn parse_u64(input: &str) -> IResult<&str, u64> {
    map_res(digits, |input: &str| u64::from_str_radix(input, 10))(input)
}

pub fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    // See https://c9x.me/compile/doc/il-v1.1.html#Spacing
    delimited(many0(one_of(" \t")), inner, many0(one_of(" \t")))
}
