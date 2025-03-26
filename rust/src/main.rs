use std::collections::HashMap;
use std::io::{self, Write};


#[derive(Debug, PartialEq, Clone)]
enum Token {
    Number(f64),
    Identifier(String),
    Plus,
    Minus,
    Multiply,
    Divide,
    Equals,
    LeftParen,
    RightParen,
}


struct Lexer {
    input: String,
    position: usize,
}

impl Lexer {
    fn new(input: String) -> Self {
        Lexer { input, position: 0 }
    }

    fn peek_char(&self) -> Option<char> {
        self.input.chars().nth(self.position)
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.peek_char();
        if ch.is_some() {
            self.position += 1;
        }
        ch
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek_char() {
            if !c.is_whitespace() {
                break;
            }
            self.next_char();
        }
    }

    fn read_number(&mut self) -> Token {
        let mut number = String::new();
        let mut has_decimal = false;

        while let Some(c) = self.peek_char() {
            if c.is_digit(10) {
                number.push(c);
                self.next_char();
            } else if c == '.' && !has_decimal {
                number.push(c);
                has_decimal = true;
                self.next_char();
            } else {
                break;
            }
        }

        Token::Number(number.parse().unwrap())
    }

    fn read_identifier(&mut self) -> Token {
        let mut identifier = String::new();

        while let Some(c) = self.peek_char() {
            if c.is_alphanumeric() || c == '_' {
                identifier.push(c);
                self.next_char();
            } else {
                break;
            }
        }

        Token::Identifier(identifier)
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        match self.peek_char() {
            None => None,
            Some(c) => {
                match c {
                    '0'..='9' => Some(self.read_number()),
                    '+' => {
                        self.next_char();
                        Some(Token::Plus)
                    }
                    '-' => {
                        self.next_char();
                        Some(Token::Minus)
                    }
                    '*' => {
                        self.next_char();
                        Some(Token::Multiply)
                    }
                    '/' => {
                        self.next_char();
                        Some(Token::Divide)
                    }
                    '=' => {
                        self.next_char();
                        Some(Token::Equals)
                    }
                    '(' => {
                        self.next_char();
                        Some(Token::LeftParen)
                    }
                    ')' => {
                        self.next_char();
                        Some(Token::RightParen)
                    }
                    _ if c.is_alphabetic() => Some(self.read_identifier()),
                    _ => {
                        println!("Unexpected character: {}", c);
                        self.next_char();
                        self.next_token()
                    }
                }
            }
        }
    }

    fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next_token() {
            tokens.push(token);
        }
        tokens
    }
}


struct Parser {
    tokens: Vec<Token>,
    position: usize,
    variables: HashMap<String, f64>,
}

#[derive(Debug)]
enum EvalError {
    DivideByZero,
    UndefinedVariable(String),
    SyntaxError(String),
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            position: 0,
            variables: HashMap::new(),
        }
    }

    fn peek_token(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    fn next_token(&mut self) -> Option<Token> {
        if self.position < self.tokens.len() {
            let token = self.tokens[self.position].clone();
            self.position += 1;
            Some(token)
        } else {
            None
        }
    }

    fn parse_expression(&mut self) -> Result<f64, EvalError> {
        match self.peek_token() {
            Some(Token::Identifier(..)) => {
                // Check for assignment
                let var_name = match self.next_token() {
                    Some(Token::Identifier(name)) => name,
                    _ => return Err(EvalError::SyntaxError("Expected identifier".to_string())),
                };

                if let Some(Token::Equals) = self.peek_token() {
                    self.next_token(); // consume the equals
                    let value = self.parse_expression()?;
                    self.variables.insert(var_name, value);
                    Ok(value)
                } else {
                    // Just a variable reference
                    match self.variables.get(&var_name) {
                        Some(value) => {
                            let result = *value;
                            self.parse_operation(result)
                        }
                        None => Err(EvalError::UndefinedVariable(var_name)),
                    }
                }
            }
            _ => {
                let value = self.parse_term()?;
                self.parse_operation(value)
            }
        }
    }

    fn parse_term(&mut self) -> Result<f64, EvalError> {
        match self.next_token() {
            Some(Token::Number(n)) => Ok(n),
            Some(Token::LeftParen) => {
                let result = self.parse_expression()?;
                match self.next_token() {
                    Some(Token::RightParen) => Ok(result),
                    _ => Err(EvalError::SyntaxError("Expected )".to_string())),
                }
            }
            Some(Token::Identifier(name)) => match self.variables.get(&name) {
                Some(value) => Ok(*value),
                None => Err(EvalError::UndefinedVariable(name)),
            },
            _ => Err(EvalError::SyntaxError("Unexpected token".to_string())),
        }
    }

    fn parse_operation(&mut self, left: f64) -> Result<f64, EvalError> {
        match self.peek_token() {
            Some(Token::Plus) => {
                self.next_token();
                let right = self.parse_term()?;
                self.parse_operation(left + right)
            }
            Some(Token::Minus) => {
                self.next_token();
                let right = self.parse_term()?;
                self.parse_operation(left - right)
            }
            Some(Token::Multiply) => {
                self.next_token();
                let right = self.parse_term()?;
                self.parse_operation(left * right)
            }
            Some(Token::Divide) => {
                self.next_token();
                let right = self.parse_term()?;
                if right == 0.0 {
                    return Err(EvalError::DivideByZero);
                }
                self.parse_operation(left / right)
            }
            _ => Ok(left),
        }
    }

    fn evaluate(&mut self) -> Result<f64, EvalError> {
        self.position = 0;
        self.parse_expression()
    }
}

fn main() {
    println!("Rust Calculator Interpreter");
    println!("Enter expressions or 'quit'/'exit' to quit");

    let mut variables = HashMap::new();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        let input = input.trim();
        if input == "quit" || input == "exit" {
            break;
        }

        if input.is_empty() {
            continue;
        }

        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.tokenize();
        
        let mut parser = Parser::new(tokens);
        parser.variables = variables.clone();

        match parser.evaluate() {
            Ok(result) => {
                println!("{}", result);
                variables = parser.variables;
            }
            Err(error) => {
                match error {
                    EvalError::DivideByZero => println!("Error: Division by zero"),
                    EvalError::UndefinedVariable(var) => {
                        println!("Error: Undefined variable '{}'", var)
                    }
                    EvalError::SyntaxError(msg) => println!("Syntax Error: {}", msg),
                }
            }
        }
    }
} 