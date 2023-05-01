mod error;
mod parser;
mod types;
mod util;

use nom::{
    branch::alt, combinator::map_res, multi::separated_list1, sequence::terminated, IResult,
};

use crate::error::Error;
use std::convert;
use std::fs::File;
use std::io::Read;
use std::path;

#[derive(Debug)]
pub enum Definition {
    Type(types::TypeDef),
    Data(types::DataDef),
    Func(types::FuncDef),
}

fn parse_def(input: &str) -> IResult<&str, Definition> {
    alt((
        map_res(parser::typedef, |ty| -> Result<Definition, ()> {
            Ok(Definition::Type(ty))
        }),
        map_res(parser::datadef, |data| -> Result<Definition, ()> {
            Ok(Definition::Data(data))
        }),
        map_res(parser::funcdef, |func| -> Result<Definition, ()> {
            Ok(Definition::Func(func))
        }),
    ))(input)
}

fn parse_defs(input: &str) -> IResult<&str, Vec<Definition>> {
    terminated(
        separated_list1(util::ws(util::newline0), parse_def),
        util::newline0,
    )(input)
}

////////////////////////////////////////////////////////////////////////

pub fn parse_file<'a, P: convert::AsRef<path::Path>>(fp: P) -> Result<Vec<Definition>, Error> {
    let mut file = File::open(fp)?;

    // TODO: mmap(2) file instead of reading it into memory.
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    let (input, defs) = parse_defs(&buf)?;
    if input != "" {
        Err(Error::IncompleteParse)
    } else {
        Ok(defs)
    }
}
