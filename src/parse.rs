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

use std::fmt::Debug;

use crate::parser_comb::{
    KeepFirstOutputOnly, KeepSecondOutputOnly, ParseIf, ParseWhile, ParseWhileOrNothing, Parser,
};

pub fn integer_parser() -> impl Parser {
    ParseWhileOrNothing(|c| c.is_whitespace()).and_then(ParseWhile(|c| c.is_numeric()))
}

pub fn float_parser() -> impl Parser<Output = impl Debug> {
    ParseWhileOrNothing(|c| c.is_whitespace())
        .and_then_combine_with(ParseWhile(|c| c.is_numeric()), KeepSecondOutputOnly)
        .and_then_combine_with(ParseIf(|c| c == '.'), KeepFirstOutputOnly)
        .and_then(ParseWhile(|c| c.is_numeric()))
        .with_try_mapping(&|(integer_part, decimal_part)| {
            format!("{integer_part}.{decimal_part}").parse::<f64>().ok()
        })
}
