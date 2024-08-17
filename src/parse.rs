use std::{fmt::Debug, ops::Deref, string::ParseError};

macro_rules! parse_many_as {
    ($input:expr, $fpattern: expr => $ft:expr, $($pattern: expr => $t:expr,)*) => {
        $fpattern.parse_this($input).map(|(_, rest)| ($ft, rest))
        $(.or_else(|_| {
            $pattern.parse_this($input).map(|(_, rest)| ($t, rest))
        }))*
    };
}
#[derive(PartialEq)]
pub enum ParserError {
    PatternNotFound(String),
}

impl Debug for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::PatternNotFound(s) => {
                f.write_fmt(format_args!("Pattern not found: {}", s))
            }
        }
    }
}

pub struct Parser<A>(Result<(A, String), ParserError>);

pub trait Parse
where
    Self: Sized,
{
    fn parse_from(input: &String) -> Result<(Self, String), ParserError>;

    fn parse_many(input: &String) -> Result<(Vec<Self>, String), ParseError> {
        let mut v = vec![];
        let mut rest = input.clone();
        loop {
            let Ok((s, nrest)) = Self::parse_from(&rest) else {
                break;
            };

            v.push(s);
            rest = nrest;
        }

        Ok((v, rest))
    }
}

pub trait ParseThis
where
    Self: PartialEq + Sized,
{
    fn parse_this(&self, input: &String) -> Result<(Self, String), ParserError>;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Bottom(NumberTypes),
    Node(ExpressionNode),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionNode {
    pub operator: NumberOperation,
    pub children_count: usize,
    pub children: Vec<Expression>,
}

impl ExpressionNode {
    fn new(operator: NumberOperation, children: Vec<Expression>) -> Self {
        Self {
            operator,
            children_count: children.len(),
            children,
        }
    }
}

impl Parse for Expression {
    fn parse_from(input: &String) -> Result<(Self, String), ParserError> {
        if let Ok((base, rest)) = NumberTypes::parse_from(input) {
            return Ok((Expression::Bottom(base), rest));
        }

        // If it is not a base type
        let (_, rest) =
            Whitespace::parse_from(input).and_then(|(_, rest)| '('.parse_this(&rest))?;

        let (op, rest) = Whitespace::parse_from(&rest)
            .and_then(|(_, rest)| NumberOperation::parse_from(&rest))?;

        let mut children = vec![];
        let mut rest = rest;
        loop {
            let (child, rem) = match Whitespace::parse_from(&rest)
                .and_then(|(_, rest)| Expression::parse_from(&rest))
            {
                Ok(x) => x,
                Err(_) => break,
            };

            rest = rem;
            children.push(child);
        }

        let (_, rest) =
            Whitespace::parse_from(&rest).and_then(|(_, rest)| ')'.parse_this(&rest))?;

        Ok((Expression::Node(ExpressionNode::new(op, children)), rest))
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Whitespace;

impl Parse for Whitespace {
    fn parse_from(input: &String) -> Result<(Self, String), ParserError> {
        let whitespace = [' ', '\t'];

        let rest = input
            .chars()
            .skip_while(|c| whitespace.contains(c))
            .collect::<String>();

        Ok((Whitespace, rest))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumberOperation {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Equality,
    NotEq,
}

impl Parse for NumberOperation {
    fn parse_from(input: &String) -> Result<(Self, String), ParserError> {
        parse_many_as!(
            input,
            '+' => NumberOperation::Add,
            '-' => NumberOperation::Sub,
            '*' => NumberOperation::Mul,
            '/' => NumberOperation::Div,
            '^' => NumberOperation::Pow,
            "==" => NumberOperation::Equality,
            "!=" => NumberOperation::NotEq,
        )
        .map_err(|_| ParserError::PatternNotFound("Could not find NumberOperation".to_string()))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumberTypes {
    Integer(i64),
    Float(f64),
}

impl Parse for NumberTypes {
    fn parse_from(input: &String) -> Result<(Self, String), ParserError> {
        // Remove the whitespace
        let (_, input) = Whitespace::parse_from(input)?;

        // First we try to parse an f64
        if let Ok((f, rest)) = f64::parse_from(&input) {
            return Ok((NumberTypes::Float(f), rest));
        }

        // If not f64, try i64
        if let Ok((i, rest)) = i64::parse_from(&input) {
            return Ok((NumberTypes::Integer(i), rest));
        }

        // If neither pattern matched
        Err(ParserError::PatternNotFound(
            "Could not parse base type (not float or integer)".to_string(),
        ))
    }
}

impl ParseThis for String {
    fn parse_this(&self, input: &String) -> Result<(Self, String), ParserError> {
        if input.starts_with(self) {
            return Ok((self.clone(), input.get(self.len()..).unwrap().to_string()));
        }

        Err(ParserError::PatternNotFound(format!(
            "{} did not match {}",
            input.get(0..self.len()).unwrap(),
            self
        )))
    }
}

impl ParseThis for &'static str {
    fn parse_this(&self, input: &String) -> Result<(Self, String), ParserError> {
        if input.starts_with(self) {
            return Ok((
                self,
                input.to_string().get(self.len()..).unwrap().to_string(),
            ));
        }

        Err(ParserError::PatternNotFound(format!(
            "{} did not match {}",
            input.get(0..self.len()).unwrap(),
            self
        )))
    }
}

impl ParseThis for char {
    fn parse_this(&self, input: &String) -> Result<(Self, String), ParserError> {
        if input.starts_with(*self) {
            Ok((self.clone(), input.get(1..).unwrap().to_string()))
        } else {
            Err(ParserError::PatternNotFound(format!(
                "Character not matched: Expected \"{}\" to start with '{}'",
                input, self,
            )))
        }
    }
}

impl Parse for i64 {
    fn parse_from(input: &String) -> Result<(Self, String), ParserError> {
        let digits = input
            .chars()
            .take_while(|c| c.is_digit(10))
            .collect::<String>();

        let length = digits.len();
        let number: Result<i64, _> = digits.parse();

        match number {
            Ok(n) if length != 0 => {
                let rest = input.chars().skip(length).collect::<String>();
                Ok((n, rest))
            }
            _ => Err(ParserError::PatternNotFound(
                "Did not match integer".to_string(),
            )),
        }
    }
}

impl Parse for f64 {
    fn parse_from(input: &String) -> Result<(Self, String), ParserError> {
        let mut separator_found = false;
        let mut is_first_separator = |c: &char| {
            if separator_found {
                return false;
            }

            if *c != '.' && *c != ',' {
                return false;
            }

            separator_found = true;
            true
        };

        let number_str = input
            .chars()
            .take_while(|c| c.is_digit(10) || is_first_separator(c) || c.eq(&'_'))
            .filter(|c| !c.eq(&'_'))
            .collect::<String>();

        let length = number_str.len();
        if length == 0 || !separator_found {
            return Err(ParserError::PatternNotFound(
                "Did not match float".to_string(),
            ));
        }
        let float = number_str.parse::<f64>().unwrap();
        let rest = input.get(length..).unwrap().to_string();
        Ok((float, rest))
    }
}

#[cfg(test)]
mod parser_test {
    use crate::parse::{Expression, NumberOperation, Parse, Whitespace};

    use super::{NumberTypes, ParseThis};

    #[test]
    fn parse_opeartion() {
        assert_eq!(
            NumberOperation::parse_from(&"+adsf".to_string()),
            Ok((NumberOperation::Add, "adsf".to_string()))
        );

        assert_eq!(
            NumberOperation::parse_from(&"!=adsf".to_string()),
            Ok((NumberOperation::NotEq, "adsf".to_string()))
        );

        assert_eq!(
            i64::parse_from(&"3.123a".to_string()),
            Ok((3, String::from(".123a")))
        );

        assert_eq!(
            f64::parse_from(&"3.123a".to_string()),
            Ok((3.123, String::from("a")))
        );
    }

    #[test]
    fn bad_parse() {
        assert!(NumberOperation::parse_from(&"adsf".to_string()).is_err())
    }

    #[test]
    fn base_type_parse() {
        assert_eq!(
            NumberTypes::parse_from(&"3.14a".into()),
            Ok((NumberTypes::Float(3.14), String::from("a")))
        );

        assert_eq!(
            NumberTypes::parse_from(&"3asdf".into()),
            Ok((NumberTypes::Integer(3), String::from("asdf")))
        );

        assert_eq!(
            NumberTypes::parse_from(&"    3asdf".into()),
            Ok((NumberTypes::Integer(3), String::from("asdf")))
        );

        assert!(NumberTypes::parse_from(&"   x3asdf".into()).is_err());

        assert!(NumberTypes::parse_from(&"x3asdf".into()).is_err());
    }

    #[test]
    fn parse_whitespace() {
        assert_eq!(
            Whitespace::parse_from(&"   hey".to_string()),
            Ok((Whitespace, "hey".to_string()))
        );
    }

    #[test]
    fn parse_expression() {}

    #[test]
    fn parse_this_string() {
        let res = "hello"
            .to_string()
            .parse_this(&String::from("hello there!"))
            .unwrap();

        assert_eq!(res, (String::from("hello"), String::from(" there!")));

        let res = ""
            .to_string()
            .parse_this(&String::from("hello there!"))
            .unwrap();

        assert_eq!(res, (String::from(""), String::from("hello there!")));
    }

    #[test]
    fn parse_many() {
        let x = NumberTypes::parse_many(&String::from("3 1.0 2 asfd")).unwrap();
        assert_eq!(
            x,
            (
                vec![
                    NumberTypes::Integer(3),
                    NumberTypes::Float(1.0),
                    NumberTypes::Integer(2)
                ],
                String::from(" asfd")
            )
        );
    }
}
