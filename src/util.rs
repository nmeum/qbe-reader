use nom::{
    character::complete::char, error::ParseError, multi::many0, sequence::delimited, IResult,
};

pub fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(many0(char(' ')), inner, many0(char(' ')))
}
