/*
* Language:
*
* base  := numb | binop <expr>*
* binop := + | - | / | *
* numb  := digit* | digit* . digit*
* expr  := numb | ( <binop> <expr>* )
*
* Sample usage:
*
* ( + 1 2 )
*
* + 1 2
*
* + 1 (* 2 3)
*
* */

use crate::{
    match_parse,
    parser_comb::{
        KeepFirstOutputOnly, KeepNone, KeepSecondOutputOnly, ParseMatch, ParseWhile,
        ParseWhileOrNothing, Parser,
    },
    tokens::{Binops, Expression, Primitives},
};

pub fn parse_whitespace() -> impl Parser {
    ParseWhileOrNothing(|c| c.is_whitespace())
}

//  ____       _           _ _   _             ____
// |  _ \ _ __(_)_ __ ___ (_) |_(_)_   _____  |  _ \ __ _ _ __ ___  ___ _ __
// | |_) | '__| | '_ ` _ \| | __| \ \ / / _ \ | |_) / _` | '__/ __|/ _ \ '__|
// |  __/| |  | | | | | | | | |_| |\ V /  __/ |  __/ (_| | |  \__ \  __/ |
// |_|   |_|  |_|_| |_| |_|_|\__|_| \_/ \___| |_|   \__,_|_|  |___/\___|_|

pub fn integer_parser() -> impl Parser<Output = Primitives> {
    parse_whitespace()
        .and_then_combine_with(ParseWhile(|c| c.is_numeric()), KeepSecondOutputOnly)
        .with_try_mapping(&|int_str| int_str.parse::<i64>().ok().map(Into::<Primitives>::into))
}

pub fn float_parser() -> impl Parser<Output = Primitives> {
    let parse_numeric = ParseWhile(|c| c.is_numeric());
    let optional_parse_numeric = ParseWhileOrNothing(|c| c.is_numeric());
    let parse_separator = ParseMatch('.');

    parse_whitespace()
        .and_then_combine_with(parse_numeric, KeepSecondOutputOnly)
        .and_then_combine_with(parse_separator, KeepFirstOutputOnly)
        .and_then(optional_parse_numeric)
        .with_try_mapping(&|(integer_part, decimal_part)| {
            format!("{integer_part}.{decimal_part}")
                .parse::<f64>()
                .ok()
                .map(Into::<Primitives>::into)
        })
}

pub fn number_parser() -> impl Parser<Output = Primitives> {
    float_parser().otherwise(integer_parser())
}

pub fn boolean_parser() -> impl Parser<Output = Primitives> {
    let boolp = match_parse!(
        "true"  => Primitives::True,
        "false" => Primitives::False,
    );

    parse_whitespace().and_then_combine_with(boolp, KeepSecondOutputOnly)
}

pub fn primitive_parser() -> impl Parser<Output = Primitives> {
    number_parser().otherwise(boolean_parser())
}

//  ____  _
// | __ )(_)_ __   __ _ _ __ _   _
// |  _ \| | '_ \ / _` | '__| | | |
// | |_) | | | | | (_| | |  | |_| |
// |____/|_|_| |_|\__,_|_|   \__, |
//   ___                     |___/ _
//  / _ \ _ __   ___ _ __ __ _| |_(_) ___  _ __  ___
// | | | | '_ \ / _ \ '__/ _` | __| |/ _ \| '_ \/ __|
// | |_| | |_) |  __/ | | (_| | |_| | (_) | | | \__ \
//  \___/| .__/ \___|_|  \__,_|\__|_|\___/|_| |_|___/
//       |_|

pub fn binop_parser() -> impl Parser<Output = Binops> {
    let p = match_parse! {
        '+'  => Binops::Plus,
        '-'  => Binops::Minus,
        '*'  => Binops::Mul,
        '/'  => Binops::Div,
        '%'  => Binops::Mod,
        "==" => Binops::Equals,
    };

    parse_whitespace().and_then_combine_with(p, KeepSecondOutputOnly)
}

/// Parser that will handle an opening bracket, with
/// any amount of whitespace beforehand
fn open_brace() -> impl Parser<Output = ()> {
    parse_whitespace()
        .and_then(ParseMatch("("))
        .combine(KeepNone)
}

/// Parser that will handle an closing bracket, with
/// any amount of whitespace beforehand
fn close_brace() -> impl Parser<Output = ()> {
    parse_whitespace()
        .and_then(ParseMatch(")"))
        .combine(KeepNone)
}

pub struct ExpressionParser;

impl Parser for ExpressionParser {
    type Output = Expression;
    fn parse(&self, input: &str) -> crate::parser_comb::ParserRes<Self::Output> {
        if let Ok((primitive, rest)) = primitive_parser().parse(input) {
            return Ok((Expression::Done(primitive), rest));
        }

        let (_, rest) = open_brace().parse(input)?;
        let (binop, mut rest) = binop_parser().parse(&rest)?;
        let mut expressions = vec![];
        loop {
            match ExpressionParser::parse(&self, &rest) {
                Ok((expr, r)) => {
                    rest = r;
                    expressions.push(expr)
                }
                _ => break,
            };
        }
        let (_, rest) = close_brace().parse(&rest)?;
        Ok((Expression::Node(binop, expressions), rest))
    }
}

pub fn expresssion_parser() -> impl Parser<Output = Expression> {
    ExpressionParser
}

#[cfg(test)]
mod test_parsers {
    use crate::{
        parse::float_parser,
        parser_comb::Parser,
        tokens::Primitives::{self, *},
    };

    use super::{boolean_parser, integer_parser, ExpressionParser};

    #[test]
    fn parser_integer() {
        let p = integer_parser();

        assert_eq!(
            p.parse("1234.123   ").unwrap(),
            (Int(1234), ".123   ".to_string())
        );

        assert_eq!(
            p.parse("9.123   ").unwrap(),
            (Int(9), ".123   ".to_string())
        );

        assert_eq!(
            p.parse("  9.123   ").unwrap(),
            (Int(9), ".123   ".to_string())
        );

        assert_eq!(
            p.parse("   aa123  "),
            Err(crate::parser_comb::ParsingError::PatternNotFound(
                "no characters matched predicate".to_string(),
            ))
        );
    }

    #[test]
    fn parser_float() {
        let p = float_parser();

        assert_eq!(
            p.parse("1234.123   a").unwrap(),
            (Float(1234.123), "   a".to_string())
        );

        assert_eq!(
            p.parse("9.123a1   ").unwrap(),
            (Float(9.123), "a1   ".to_string())
        );

        assert_eq!(p.parse("  9. ").unwrap(), (Float(9.0), " ".to_string()));

        assert_eq!(
            p.parse("   aa123  "),
            Err(crate::parser_comb::ParsingError::PatternNotFound(
                "no characters matched predicate".to_string(),
            ))
        );
    }

    #[test]
    fn parser_number() {}

    #[test]
    fn parser_boolean() {
        let p = boolean_parser();
        assert_eq!(p.parse("true then").unwrap(), (True, " then".to_string()));
        assert_eq!(p.parse("false then").unwrap(), (False, " then".to_string()));
    }

    #[test]
    fn expression_parse() {
        let p = ExpressionParser;

        assert!(p.parse("(+ 1 2)").is_ok());
        assert!(p.parse("(+ 1 true)").is_ok());
    }
}
