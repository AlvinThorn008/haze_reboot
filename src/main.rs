mod lexer;
mod token;
mod ast;
mod parser;
mod ast_print;
mod visitor;
mod error;

use std::fs;

fn main() {

    let bytes = fs::read("prog_file.txt").expect("`prog_file.txt` should exist");
    let program = String::from_utf8(bytes).unwrap();

    let mut preparsed = parser::Parser::new(&program);

    let parsed = preparsed.parse().unwrap();

    // println!("{}", ast_print::print_stmts(parsed, &program));

    fs::write("ast.json", ast_print::print_stmts(parsed, &program)).expect("ast.json should exist");

    // println!("{:#?}", preparsed.parse().unwrap());
}

fn g(x: i32 ) }
