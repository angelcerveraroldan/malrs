use crate::parse::ParsingError;

use super::ParserRes;

pub trait Parser {
    type Output;

    /// Parse the input string, if the parser is sucessful, it will return Ok((parsed, rest)),
    /// where parsed is the data that was parsed from the string, and the rest is what was left
    /// over.
    ///
    /// If parsing did not suceed, then an error will be returned
    fn parse(&self, input: &str) -> ParserRes<Self::Output>;

    /// Parse the output (see parse function), and if sucessful, map the parsed output
    fn parse_and_map<F, MappedOutput>(&self, input: &str, f: F) -> ParserRes<MappedOutput>
    where
        F: FnOnce(Self::Output) -> MappedOutput,
    {
        self.parse(input).map(|(a, rest)| (f(a), rest))
    }
}

pub struct AndThenParser<A, B, C> {
    pub first_parse: A,
    pub second_parse: B,
    combinator: C,
}

impl<A, B, C> AndThenParser<A, B, C>
where
    A: Parser,
    B: Parser,
{
    pub fn new(first_parse: A, second_parse: B) -> AndThenParser<A, B, IdentityAndCombinator> {
        AndThenParser {
            first_parse,
            second_parse,
            combinator: IdentityAndCombinator,
        }
    }
}

pub trait AndCombinator<A, B> {
    type Combined;
    fn combine(&self, pair: (A, B)) -> Self::Combined;
}

pub struct IdentityAndCombinator;

impl<A, B> AndCombinator<A, B> for IdentityAndCombinator {
    type Combined = (A, B);
    fn combine(&self, pair: (A, B)) -> Self::Combined {
        pair
    }
}

impl<A, B, C> Parser for AndThenParser<A, B, C>
where
    A: Parser,
    B: Parser,
    C: AndCombinator<A::Output, B::Output>,
{
    type Output = C::Combined;
    fn parse(&self, input: &str) -> ParserRes<Self::Output> {
        let (a, rest) = A::parse(&self.first_parse, input)?;
        let (b, rest) = B::parse(&self.second_parse, &rest)?;
        Ok((C::combine(&self.combinator, (a, b)), rest))
    }
}

pub struct OrThenParser<A, B>
where
    A: Parser,
    B: Parser,
{
    pub first_parse: A,
    pub second_parse: B,
}

impl<A, B, CommonOut> Parser for OrThenParser<A, B>
where
    A: Parser<Output = CommonOut>,
    B: Parser<Output = CommonOut>,
{
    type Output = CommonOut;
    fn parse(&self, input: &str) -> ParserRes<Self::Output> {
        let aparse = A::parse(&self.first_parse, input);
        let Err(_) = aparse else {
            return aparse;
        };

        let bparse = B::parse(&self.second_parse, input);
        let Err(_) = bparse else {
            return bparse;
        };

        Err(ParsingError::PatternNotFound(
            "Or Parser didnt match either of the branches".to_string(),
        ))
    }
}

pub struct ParseIf<A>(pub fn(A) -> bool);

impl Parser for ParseIf<char> {
    type Output = char;
    fn parse(&self, input: &str) -> ParserRes<Self::Output> {
        let maybe_first_char = input.chars().next();
        if let Some(true) = maybe_first_char.map(self.0) {
            return Ok((maybe_first_char.unwrap(), input.chars().skip(1).collect()));
        }
        Err(ParsingError::PatternNotFound(
            "if predicate not met".to_string(),
        ))
    }
}

pub struct ParseWhile(pub fn(char) -> bool);

impl Parser for ParseWhile {
    type Output = String;
    fn parse(&self, input: &str) -> ParserRes<Self::Output> {
        let taken = input.chars().take_while(|&x| self.0(x));
        let rest = input.chars().skip_while(|&x| self.0(x));
        Ok((taken.collect(), rest.collect()))
    }
}

#[cfg(test)]
mod test_parsers {
    use super::{ParseIf, ParseWhile, Parser};

    #[test]
    fn parse_if() {
        let numericalp = ParseIf::<char>(|c| c.is_numeric());
        let expectedchar = Ok(('1', "sdf".to_string()));
        let expectednume = Ok((1, "sdf".to_string()));
        assert_eq!(numericalp.parse("1sdf"), expectedchar);
        assert_eq!(
            numericalp.parse_and_map("1sdf", |c| c.to_digit(10).unwrap()),
            expectednume
        );
    }

    #[test]
    fn whitespace_parser() {
        let whitespacep = ParseWhile(|c| c.is_whitespace());
        let expected = Ok(("    ".to_string(), "hello ".to_string()));
        assert_eq!(whitespacep.parse("    hello "), expected)
    }
}
