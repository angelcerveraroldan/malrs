/*
*
* Mini Parser Combinator Library
*
* */

/// The ty typest must all be elements of the same enum
#[macro_export]
macro_rules! match_parse {
    ( $e:expr => $t:ty ) => {
        ParseMatch($e).with_mapping(&|_| $t)
    };

    ( $e:expr => $t:expr, $($ee:expr => $tt:expr,)* ) => {
        ParseMatch($e).with_mapping(&|_| $t)
            $(.otherwise( ParseMatch($ee).with_mapping(&|_| $tt) ))*
    };
}

use std::fmt::Debug;

#[derive(Debug, PartialEq)]
pub enum ParsingError {
    PatternNotFound(String),
    CannotParseAnEmptyString,
    MappingError(String),
}

pub type ParserRes<A, E = ParsingError> = std::result::Result<(A, String), E>;

pub trait Parser
where
    Self: Sized,
{
    type Output: Debug;

    /// Parse the input string, if the parser is sucessful, it will return Ok((parsed, rest)),
    /// where parsed is the data that was parsed from the string, and the rest is what was left
    /// over.
    ///
    /// If parsing did not suceed, then an error will be returned
    fn parse(&self, input: &str) -> ParserRes<Self::Output>;

    /// Parse the output (see parse function), and if sucessful, map the parsed output
    fn parse_and_then_map<F, MappedOutput>(&self, input: &str, f: F) -> ParserRes<MappedOutput>
    where
        F: FnOnce(Self::Output) -> MappedOutput,
    {
        self.parse(input).map(|(a, rest)| (f(a), rest))
    }

    /// Make a new parser that consists of this parser, followed by another parser.
    ///
    /// The output will be sucessful iff both parsers are sucessful
    fn and_then<P>(self, other: P) -> AndThenParser<Self, P, IdentityAndCombinator>
    where
        P: Parser,
    {
        AndThenParser::from((self, other))
    }

    fn and_then_combine_with<P, C>(self, other: P, combinator: C) -> AndThenParser<Self, P, C>
    where
        P: Parser,
        C: AndCombinator<Self::Output, P::Output>,
    {
        AndThenParser::from((self, other, combinator))
    }

    /// Make a new parser that consists of this parser OR another parser.
    ///
    /// This new parser will run both parsers in order, and return the first sucessful one
    fn otherwise<P>(self, other: P) -> OrThenParser<Self, P>
    where
        P: Parser,
    {
        OrThenParser::from((self, other))
    }

    fn with_mapping<'a, T>(self, mapping: &'a dyn Fn(Self::Output) -> T) -> MapParser<Self, T> {
        MapParser {
            parser: self,
            mapping,
        }
    }

    fn with_try_mapping<'a, T>(
        self,
        try_map: &'a dyn Fn(Self::Output) -> Option<T>,
    ) -> TryMapParser<Self, T> {
        TryMapParser {
            parser: self,
            try_map,
        }
    }
}

pub struct AndThenParser<A, B, C>
where
    A: Parser,
    B: Parser,
    C: AndCombinator<A::Output, B::Output>,
{
    pub first_parse: A,
    pub second_parse: B,
    combinator: C,
}

impl<A, B, C> AndThenParser<A, B, C>
where
    A: Parser,
    B: Parser,
    C: AndCombinator<A::Output, B::Output>,
{
    pub fn combine<NC>(self, combinator: NC) -> AndThenParser<A, B, NC>
    where
        NC: AndCombinator<A::Output, B::Output>,
    {
        AndThenParser {
            first_parse: self.first_parse,
            second_parse: self.second_parse,
            combinator,
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

pub struct KeepFirstOutputOnly;

impl<A, B> AndCombinator<A, B> for KeepFirstOutputOnly {
    type Combined = A;
    fn combine(&self, (a, _): (A, B)) -> Self::Combined {
        a
    }
}

pub struct KeepSecondOutputOnly;

impl<A, B> AndCombinator<A, B> for KeepSecondOutputOnly {
    type Combined = B;
    fn combine(&self, (_, b): (A, B)) -> Self::Combined {
        b
    }
}

pub struct KeepNone;

impl<A, B> AndCombinator<A, B> for KeepNone {
    type Combined = ();
    fn combine(&self, _: (A, B)) -> Self::Combined {
        ()
    }
}

impl<A, B, C> From<(A, B, C)> for AndThenParser<A, B, C>
where
    A: Parser,
    B: Parser,
    C: AndCombinator<A::Output, B::Output>,
{
    fn from((first_parse, second_parse, combinator): (A, B, C)) -> Self {
        Self {
            first_parse,
            second_parse,
            combinator,
        }
    }
}

impl<A, B> From<(A, B)> for AndThenParser<A, B, IdentityAndCombinator>
where
    A: Parser,
    B: Parser,
{
    fn from((first_parse, second_parse): (A, B)) -> Self {
        Self {
            first_parse,
            second_parse,
            combinator: IdentityAndCombinator,
        }
    }
}

impl<A, B, C> Parser for AndThenParser<A, B, C>
where
    A: Parser,
    B: Parser,
    C: AndCombinator<A::Output, B::Output>,
    C::Combined: Debug,
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

impl<A, B> From<(A, B)> for OrThenParser<A, B>
where
    A: Parser,
    B: Parser,
{
    fn from((ap, bp): (A, B)) -> Self {
        Self {
            first_parse: ap,
            second_parse: bp,
        }
    }
}

impl<A, B, CommonOut> Parser for OrThenParser<A, B>
where
    A: Parser<Output = CommonOut>,
    B: Parser<Output = CommonOut>,
    CommonOut: Debug,
{
    type Output = CommonOut;
    fn parse(&self, input: &str) -> ParserRes<Self::Output> {
        let aparse = self.first_parse.parse(input);
        if aparse.is_ok() {
            return aparse;
        }

        let bparse = self.second_parse.parse(input);
        if bparse.is_ok() {
            return bparse;
        };

        Err(ParsingError::PatternNotFound(
            "Or Parser didnt match either of the branches".to_string(),
        ))
    }
}

// TODO: Implement TryMap Fn(P::Output) -> Option<T>
pub struct MapParser<'a, P: Parser, T> {
    pub parser: P,
    pub mapping: &'a dyn Fn(P::Output) -> T,
}

pub struct TryMapParser<'a, P: Parser, T> {
    pub parser: P,
    pub try_map: &'a dyn Fn(P::Output) -> Option<T>,
}

impl<'a, P, T> Parser for MapParser<'a, P, T>
where
    P: Parser,
    T: Debug,
{
    type Output = T;
    fn parse(&self, input: &str) -> ParserRes<Self::Output> {
        self.parser.parse_and_then_map(input, &self.mapping)
    }
}

impl<'a, P, T> Parser for TryMapParser<'a, P, T>
where
    P: Parser,
    T: Debug,
{
    type Output = T;
    fn parse(&self, input: &str) -> ParserRes<Self::Output> {
        let (p, rest) = self.parser.parse(input)?;
        match (self.try_map)(p) {
            None => Err(ParsingError::MappingError("mapping failed".to_string())),
            Some(mapped_val) => Ok((mapped_val, rest)),
        }
    }
}

pub struct ParseMatch<A>(pub A)
where
    A: Into<String>;

impl<A> Parser for ParseMatch<A>
where
    A: Into<String> + Clone,
{
    type Output = String;
    fn parse(&self, input: &str) -> ParserRes<Self::Output> {
        let match_str: String = self.0.clone().into();
        if !input.starts_with(&match_str) {
            return Err(ParsingError::PatternNotFound(format!(
                "{} did not match pattern: {}",
                input, match_str
            )));
        }
        let rest = input.chars().skip(match_str.len()).collect();
        Ok((match_str, rest))
    }
}

pub struct ParseIf(pub fn(char) -> bool);

impl Parser for ParseIf {
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

#[derive(Debug, Clone)]
pub struct ParseWhileOrNothing(pub fn(char) -> bool);

impl Parser for ParseWhileOrNothing {
    type Output = String;
    fn parse(&self, input: &str) -> ParserRes<Self::Output> {
        let taken = input.chars().take_while(|&x| self.0(x)).collect::<String>();
        let rest = input.chars().skip_while(|&x| self.0(x)).collect::<String>();
        Ok((taken, rest))
    }
}

#[derive(Debug, Clone)]
pub struct ParseWhile(pub fn(char) -> bool);

impl Parser for ParseWhile {
    type Output = String;
    fn parse(&self, input: &str) -> ParserRes<Self::Output> {
        let taken = input.chars().take_while(|&x| self.0(x)).collect::<String>();
        if taken.is_empty() {
            return Err(ParsingError::PatternNotFound(
                "no characters matched predicate".to_string(),
            ));
        }
        let rest = input.chars().skip_while(|&x| self.0(x)).collect::<String>();
        Ok((taken, rest))
    }
}

#[cfg(test)]
mod test_parsers {
    use super::{
        AndCombinator, AndThenParser, IdentityAndCombinator, MapParser, ParseIf, ParseMatch,
        ParseWhile, Parser,
    };

    #[test]
    fn parse_if() {
        let numericalp = ParseIf(|c| c.is_numeric());
        let expectedchar = Ok(('1', "sdf".to_string()));
        let expectednume = Ok((1, "sdf".to_string()));
        assert_eq!(numericalp.parse("1sdf"), expectedchar);
        assert_eq!(
            numericalp.parse_and_then_map("1sdf", |c| c.to_digit(10).unwrap()),
            expectednume
        );
    }

    #[test]
    fn whitespace_parser() {
        let whitespacep = ParseWhile(|c| c.is_whitespace());
        let expected = Ok(("    ".to_string(), "hello ".to_string()));
        assert_eq!(whitespacep.parse("    hello "), expected)
    }

    #[test]
    fn and_combinator() {
        let whitespacep = ParseWhile(|c| c.is_whitespace());
        let numericalp = ParseIf(|c| c.is_numeric());
        let ws_numeric = AndThenParser::from((whitespacep, numericalp));
        let ans = ws_numeric.parse(" 1hey1").unwrap();
        let exp = ((" ".to_string(), '1'), "hey1".to_string());
        assert_eq!(ans, exp);
    }

    #[test]
    fn mapped_parser() {
        let numericalp = ParseWhile(|c| c.is_numeric());
        let num_p = MapParser {
            parser: numericalp,
            mapping: &|x| x.parse::<i32>().unwrap(),
        };

        let acc = num_p.parse("123a").unwrap();
        let exp = (123, "a".to_string());
        assert_eq!(acc, exp);
    }

    #[test]
    fn or_parse() {}

    #[test]
    fn match_parser() {
        let p = ParseMatch("Hello!");

        assert_eq!(
            p.parse("Hello!There"),
            Ok(("Hello!".to_string(), "There".to_string()))
        );

        assert_eq!(p.parse("  Hello!There").is_ok(), false);
    }

    #[test]
    fn macro_match_parse() {
        #[derive(Debug, PartialEq)]
        enum T {
            A,
            B,
            C,
        }

        let p = match_parse!(
                "a" => T::A,
                "b" => T::B,
                "c" => T::C,
        );

        assert_eq!(p.parse("a  "), Ok((T::A, "  ".to_string())));
        assert_eq!(p.parse("b  "), Ok((T::B, "  ".to_string())));
    }
}
