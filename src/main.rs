use std::io::{Read, Write};

use parse::{float_parser, ExpressionParser};
use parser_comb::Parser;

pub mod evaluate;
pub mod parse;
pub mod parser_comb;
pub mod tokens;

fn repl() {
    let mut buffer = String::new();
    let stdin = std::io::stdin();

    let parser = ExpressionParser;

    println!("Welcome to malrs v0.0.0.1. Happy coding!\n");
    loop {
        print!("malrs> ");
        let _ = std::io::stdout().flush();
        buffer.clear();
        stdin.read_line(&mut buffer).unwrap();

        match parser.parse(&buffer) {
            Ok((_, rest)) if !rest.is_empty() && rest != "\n" => {
                println!("Input was not an expression!")
            }
            Ok((tree, _)) => match tree.eval() {
                Ok(answer) => println!("{:?}", answer),
                Err(error) => println!("{:?}", error),
            },
            Err(err) => println!("{:?}", err),
        }
        println!()
    }
}

fn main() {
    repl()
}
