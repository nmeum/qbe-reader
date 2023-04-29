mod util;
use crate::util::*;

use nom::{
    number::complete::{double, float},
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, none_of, alpha1, alphanumeric1},
    combinator::{map_res, opt, recognize},
    multi::{many0, many0_count},
    sequence::{preceded, delimited, terminated, tuple, pair},
    IResult,
};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BaseType {
    Word,
    Long,
    Single,
    Double,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ExtType {
    Base(BaseType),
    Byte,
    Halfword,
}

#[derive(Debug, PartialEq)]
pub enum Const {
    Number(i64),
    SFP(f32),
    DFP(f64),
    Ident(String),
}

#[derive(Debug, PartialEq)]
pub enum Linkage {
    Export,
    Thread,
    Section(String, Option<String>),
}

////////////////////////////////////////////////////////////////////////

// STRING := '"' .... '"'
fn parse_string(input: &str) -> IResult<&str, String> {
    // TODO: Does qbe support escaping in strings?
    map_res(
        delimited(char('"'), recognize(many0(none_of("\""))), char('"')),
        |s| -> Result<String, ()> { Ok(String::from(s)) },
    )(input)
}

////////////////////////////////////////////////////////////////////////

// BASETY := 'w' | 'l' | 's' | 'd'
pub fn parse_base_type(input: &str) -> IResult<&str, BaseType> {
    alt((
        bind(char('w'), BaseType::Word),
        bind(char('l'), BaseType::Long),
        bind(char('s'), BaseType::Single),
        bind(char('d'), BaseType::Double),
    ))(input)
}

// EXTTY := BASETY | 'b' | 'h'
pub fn parse_ext_type(input: &str) -> IResult<&str, ExtType> {
    alt((
        bind(char('b'), ExtType::Byte),
        bind(char('h'), ExtType::Halfword),
        map_res(parse_base_type, |ty| -> Result<ExtType, ()> {
            Ok(ExtType::Base(ty))
        }),
    ))(input)
}

// Parse an identifier. This is not defined in the "formal" EBNF
// grammar of QBE but the existing QBE parser parses alphanumeric
// characters including '.' and '_'.
//
// See https://c9x.me/git/qbe.git/tree/parse.c?h=v1.1#n302
pub fn parse_ident(input: &str) -> IResult<&str, String> {
    map_res(
        recognize(
            pair(
                alt((alpha1, tag("."), tag("_"))),
                many0_count(alt((alphanumeric1, tag("$"), tag("."), tag("_"))))
            )
        ),
        |s| -> Result<String, ()> { Ok(String::from(s)) },
    )(input)
}

// CONST :=
//     ['-'] NUMBER  # Decimal integer
//   | 's_' FP       # Single-precision float
//   | 'd_' FP       # Double-precision float
//   | $IDENT        # Global symbol
pub fn parse_const(input: &str) -> IResult<&str, Const> {
    // TODO: Ensure that floating point parser matches what QBE's parser does.
    // See: https://c9x.me/git/qbe.git/tree/parse.c?h=v1.1#n245
    alt((
        map_res(
            tuple((opt(char('-')), ws(digits))),
            |(pfx, n)| -> Result<Const, ()> {
                match pfx {
                    Some(_) => Ok(Const::Number(-n)),
                    None    => Ok(Const::Number(n))
                }
            }
        ),
        preceded(
            tag("s_"),
            map_res(
                float,
                |f| -> Result<Const, ()> { Ok(Const::SFP(f)) }
            )
        ),
        preceded(
            tag("d_"),
            map_res(
                double,
                |f| -> Result<Const, ()> { Ok(Const::DFP(f)) }
            )
        ),
        preceded(
            char('$'),
            map_res(
                parse_ident,
                |s| -> Result<Const, ()> { Ok(Const::Ident(s)) }
            )
        )
    ))(input)
}

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
    terminated(ws(_parse_linkage), newline0)(input)
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
        assert_eq!(parse_linkage("export"), Ok(("", Linkage::Export)));
        assert_eq!(parse_linkage("thread"), Ok(("", Linkage::Thread)));

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

    #[test]
    fn types() {
        // Base types
        assert_eq!(parse_base_type("w"), Ok(("", BaseType::Word)));
        assert_eq!(parse_base_type("l"), Ok(("", BaseType::Long)));
        assert_eq!(parse_base_type("s"), Ok(("", BaseType::Single)));
        assert_eq!(parse_base_type("d"), Ok(("", BaseType::Double)));

        // Extented types
        assert_eq!(
            parse_ext_type("s"),
            Ok(("", ExtType::Base(BaseType::Single)))
        );
        assert_eq!(parse_ext_type("h"), Ok(("", ExtType::Halfword)));
    }

    #[test]
    fn ident() {
        assert_eq!(parse_ident("_foo"), Ok(("", String::from("_foo"))));
        assert_eq!(parse_ident("foobar"), Ok(("", String::from("foobar"))));
        assert_eq!(parse_ident("foo$_.23__"), Ok(("", String::from("foo$_.23__"))));
    }

    #[test]
    fn constant() {
        // Number
        assert_eq!(parse_const("23"), Ok(("", Const::Number(23))));
        assert_eq!(parse_const("-5"), Ok(("", Const::Number(-5))));

        // Identifier
        assert_eq!(parse_const("$foobar"), Ok(("", Const::Ident(String::from("foobar")))));
        assert_eq!(parse_const("$_fo$$r"), Ok(("", Const::Ident(String::from("_fo$$r")))));

        // Float
        assert_eq!(parse_const("s_23.42"), Ok(("", Const::SFP(23.42))));
        assert_eq!(parse_const("s_23."), Ok(("", Const::SFP(23.))));

        // Double
        assert_eq!(parse_const("d_11e-1"), Ok(("", Const::DFP(1.1))));
    }
}
