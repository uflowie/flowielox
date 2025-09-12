use std::{env::args, fs::read_to_string, io::stdin};
mod scanning;
use scanning::get_tokens;

fn main() {
    let args: Vec<_> = args().collect();

    if args.len() > 3 {
        println!("Usage: jlox [script]");
    } else if args.len() == 3 {
        run_file(&args[0]);
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
        stdin()
            .read_line(&mut line)
            .expect("user input to not not be empty");
        run(&line);
        line.clear();
    }
}

fn run(program: &str) {
    println!("{}", program);
    let tokens = get_tokens(program);
}

