pub mod parse;

#[derive(Debug, PartialEq)]
pub enum ParsingError {
    PatternNotFound(String),
    CannotParseAnEmptyString,
}

pub type ParserRes<A, E = ParsingError> = std::result::Result<(A, String), E>;
