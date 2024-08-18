use std::io::{Read, Write};

use parse::Parse;

pub mod evaluate;
pub mod parse;

fn repl() {
    let mut buffer = String::new();
    let stdin = std::io::stdin();

    println!("Welcom eto malrs v0.0.0.1. Happy coding!\n");
    loop {
        print!("malrs> ");
        let _ = std::io::stdout().flush();
        buffer.clear();
        stdin.read_line(&mut buffer).unwrap();
        match parse::Expression::parse_from(&buffer) {
            Ok((a, _)) => match a {
                parse::Expression::Bottom(n) => println!("{:?}", n),
                parse::Expression::Node(n) => println!("{:?}", n.evaluate()),
            },
            Err(e) => println!("{:?}", e),
        }
    }
}

fn main() {
    repl();
}
