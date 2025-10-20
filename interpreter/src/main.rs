use std::{
    env::args,
    fs::read_to_string,
    io::{Write, stdin, stdout},
};
mod expressions;
mod interpreting;
mod parsing;
mod scanning;
mod statements;
use scanning::get_tokens;

use crate::{interpreting::interpret, parsing::parse};

fn main() {
    let args: Vec<_> = args().collect();

    if args.len() > 2 {
        println!("Usage: jlox [script]");
    } else if args.len() == 2 {
        run_file(&args[1]);
    } else {
        run_prompt();
    }
}

fn run_file(file_name: &str) {
    let program = read_to_string(file_name).expect("file content to not be empty");
    run(&program);
}

fn run_prompt() {
    let mut line = String::new();

    loop {
        print!(">>> ");
        stdout().flush().expect("failed to flush stdout");
        stdin()
            .read_line(&mut line)
            .expect("user input to not not be empty");
        run(&line);
        line.clear();
    }
}

fn run(program: &str) {
    let Ok(tokens) = get_tokens(program) else {
        return;
    };

    let statements = parse(&tokens);
    interpret(&statements);
}
