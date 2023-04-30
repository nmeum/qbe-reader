use std::io;
use std::str;

#[derive(Debug)]
pub enum Error {
    IncompleteParse,
    ParsingError(String, nom::error::ErrorKind),
    IoError(io::Error),
}

impl From<nom::Err<nom::error::Error<&str>>> for Error {
    fn from(e: nom::Err<nom::error::Error<&str>>) -> Self {
        match e {
            nom::Err::Incomplete(_) => Error::IncompleteParse,
            nom::Err::Error(e) | nom::Err::Failure(e) => {
                Error::ParsingError(e.input.to_string(), e.code)
            }
        }
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::IoError(e)
    }
}
