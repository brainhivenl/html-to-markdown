use std::{
    borrow::Cow,
    fmt::{self, Display},
    io,
    string::FromUtf8Error,
};

#[derive(Debug)]
pub enum Error {
    IOError(io::Error),
    ParseError(Cow<'static, str>),
    Utf8Error(FromUtf8Error),
    Other(Cow<'static, str>),
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::IOError(e) => e.fmt(f),
            Error::ParseError(e) => e.fmt(f),
            Error::Utf8Error(e) => e.fmt(f),
            Error::Other(e) => e.fmt(f),
        }
    }
}

impl From<Cow<'static, str>> for Error {
    fn from(s: Cow<'static, str>) -> Self {
        Self::Other(s)
    }
}

impl From<&'static str> for Error {
    fn from(s: &'static str) -> Self {
        Self::Other(s.into())
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Self::IOError(err)
    }
}

impl From<FromUtf8Error> for Error {
    fn from(err: FromUtf8Error) -> Self {
        Self::Utf8Error(err)
    }
}
