use std::io::{Read, Write};

use parse::float_parser;
use parser_comb::Parser;

pub mod evaluate;
pub mod parse;
pub mod parser_comb;

fn repl() {
    let mut buffer = String::new();
    let stdin = std::io::stdin();

    let parser = float_parser();

    println!("Welcom eto malrs v0.0.0.1. Happy coding!\n");
    loop {
        print!("malrs> ");
        let _ = std::io::stdout().flush();
        buffer.clear();
        stdin.read_line(&mut buffer).unwrap();
        println!("{:?}", parser.parse(&buffer))
    }
}

fn main() {
    repl()
}
