use nom::{
    character::complete::one_of, error::ParseError, multi::many0, sequence::delimited, IResult,
};

pub fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    // See https://c9x.me/compile/doc/il-v1.1.html#Spacing
    delimited(many0(one_of(" \t")), inner, many0(one_of(" \t")))
}
