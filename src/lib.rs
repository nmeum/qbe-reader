mod util;
use crate::util::*;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, none_of},
    combinator::{map_res, opt},
    multi::many0,
    sequence::{delimited, tuple, terminated},
    IResult,
};

#[derive(Debug, PartialEq)]
pub enum BaseType {
    Word,
    Long,
    Single,
    Double,
}

#[derive(Debug, PartialEq)]
pub enum Linkage {
    Export,
    Thread,
    Section(String, Option<String>),
}

// STRING := '"' .... '"'
fn parse_string(input: &str) -> IResult<&str, String> {
    // TODO: Does qbe support escaping in strings?
    map_res(
        delimited(char('"'), many0(none_of("\"")), char('"')),
        |v| -> Result<String, ()> { Ok(v.into_iter().collect()) },
    )(input)
}

////////////////////////////////////////////////////////////////////////

// LINKAGE :=
//     'export'
//   | 'thread'
//   | 'section' SECNAME
//   | 'section' SECNAME SECFLAGS
//
// SECNAME  := '"' .... '"'
// SECFLAGS := '"' .... '"'
pub fn _parse_linkage(input: &str) -> IResult<&str, Linkage> {
    alt((
        map_res(tag("export"), |_| -> Result<Linkage, ()> {
            Ok(Linkage::Export)
        }),
        map_res(tag("thread"), |_| -> Result<Linkage, ()> {
            Ok(Linkage::Thread)
        }),
        map_res(
            tuple((tag("section"), ws(parse_string), opt(parse_string))),
            |(_, secname, secflags)| -> Result<Linkage, ()> {
                Ok(Linkage::Section(secname, secflags))
            },
        ),
    ))(input)
}

// Parse linkage terminated with one or more newline characters.
pub fn parse_linkage(input: &str) -> IResult<&str, Linkage> {
    terminated(ws(_parse_linkage), newlines)(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string() {
        assert_eq!(parse_string("\"foobar\""), Ok(("", String::from("foobar"))));
        assert_eq!(parse_string("\"\""), Ok(("", String::from(""))));
    }

    #[test]
    fn linkage() {
        // Primitive linkage directives
        assert_eq!(parse_linkage("export\n"), Ok(("", Linkage::Export)));
        assert_eq!(parse_linkage("thread\n"), Ok(("", Linkage::Thread)));

        // Section linkage directive
        assert_eq!(
            parse_linkage("section \"foobar\"\n"),
            Ok(("", Linkage::Section(String::from("foobar"), None)))
        );
        assert_eq!(
            parse_linkage(" section \"foo\"	\"bar\" \n"),
            Ok((
                "",
                Linkage::Section(String::from("foo"), Some(String::from("bar")))
            ))
        );
    }
}
