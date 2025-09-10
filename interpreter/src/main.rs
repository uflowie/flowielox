use std::{env::args, fs::read_to_string, io::stdin};

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
}

fn get_tokens(program: &str) -> Result<Vec<Token>, ScanningError> {
    let mut tokens = vec![];
    let mut start = 0;
    let mut line = 1;
    let mut curr = 0;
    let chars: Vec<char> = program.chars().collect();

    while curr < chars.len() {
        let token = match chars[curr] {
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            ',' => Token::Comma,
            '.' => Token::Dot,
            '-' => Token::Minus,
            '+' => Token::Plus,
            ';' => Token::Semicolon,
            '*' => Token::Star,
            _ => Token::EOF,
        };
    }

    tokens.push(Token::EOF);
    Ok(tokens)
}

enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier(String),
    String(String),
    Number(i64),
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    EOF,
}

enum ScanningError {}
