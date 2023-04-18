use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token {
    VariableX,
    Value(i64),
    Addition,
    Subtraction,
    Multiplication,
    Division,
    ParenLeft,
    ParenRight,
    Eof,
}

impl Token {
    fn is_operand(&self) -> bool {
        matches!(self, Token::VariableX | Token::Value(_))
    }

    fn is_operator(&self) -> bool {
        matches!(
            self,
            Token::Addition | Token::Subtraction | Token::Multiplication | Token::Division
        )
    }
}

struct Tokenizer<'a> {
    input: &'a [u8],
    current_token: Option<Token>,
}

impl<'a> Tokenizer<'a> {
    fn new(input: &'a [u8]) -> Self {
        Self {
            input,
            current_token: None,
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(b) = self.input.first() {
            if b.is_ascii_whitespace() {
                self.input = &self.input[1..];
            } else {
                return;
            }
        }
    }

    fn consume_digits(&mut self) -> &[u8] {
        let original_input = self.input;
        let mut count = 0;
        while let Some(b) = self.input.first() {
            if b.is_ascii_digit() {
                self.input = &self.input[1..];
                count += 1;
            } else {
                break;
            }
        }
        &original_input[..count]
    }

    fn next(&mut self) -> Token {
        if let Some(current_token) = self.current_token {
            self.current_token = None;
            return current_token;
        }

        self.skip_whitespace();

        if let Some(&b) = self.input.first() {
            if b.is_ascii_digit() {
                let digits = self.consume_digits();
                let digits = unsafe { std::str::from_utf8_unchecked(digits) };
                return Token::Value(digits.parse::<i64>().unwrap());
            } else if b == b'x' {
                self.input = &self.input[1..];
                return Token::VariableX;
            } else if b == b'+' {
                self.input = &self.input[1..];
                return Token::Addition;
            } else if b == b'-' {
                self.input = &self.input[1..];
                return Token::Subtraction;
            } else if b == b'*' {
                self.input = &self.input[1..];
                return Token::Multiplication;
            } else if b == b'/' {
                self.input = &self.input[1..];
                return Token::Division;
            } else if b == b'(' {
                self.input = &self.input[1..];
                return Token::ParenLeft;
            } else if b == b')' {
                self.input = &self.input[1..];
                return Token::ParenRight;
            }
        }

        Token::Eof
    }

    fn peek(&mut self) -> Token {
        if let Some(current_token) = self.current_token {
            return current_token;
        }

        let next_token = self.next();
        self.current_token = Some(next_token);
        next_token
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Tree {
    VariableX,
    Value(i64),
    Addition(Box<Tree>, Box<Tree>),
    Subtraction(Box<Tree>, Box<Tree>),
    Multiplication(Box<Tree>, Box<Tree>),
    Division(Box<Tree>, Box<Tree>),
}

impl Display for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tree::VariableX => write!(f, "x"),
            Tree::Value(v) => write!(f, "{v}"),
            Tree::Addition(lhs, rhs) => write!(f, "({lhs} + {rhs})"),
            Tree::Subtraction(lhs, rhs) => write!(f, "({lhs} - {rhs})"),
            Tree::Multiplication(lhs, rhs) => write!(f, "({lhs} * {rhs})"),
            Tree::Division(lhs, rhs) => write!(f, "({lhs} / {rhs})"),
        }
    }
}

impl Tree {
    fn new(input: &str) -> Self {
        let mut parser = Parser::new(input);
        parser.parse()
    }

    fn contains_x(&self) -> bool {
        match self {
            Tree::VariableX => true,
            Tree::Value(_) => false,
            Tree::Addition(lhs, rhs)
            | Tree::Subtraction(lhs, rhs)
            | Tree::Multiplication(lhs, rhs)
            | Tree::Division(lhs, rhs) => lhs.contains_x() || rhs.contains_x(),
        }
    }
}

struct Parser<'a> {
    tokenizer: Tokenizer<'a>,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            tokenizer: Tokenizer::new(input.as_bytes()),
        }
    }

    fn parse(&mut self) -> Tree {
        self.parse_expression()
    }

    fn parse_expression(&mut self) -> Tree {
        let mut root = self.parse_operand();
        loop {
            let operator = {
                let next = self.tokenizer.peek();
                if !next.is_operator() {
                    return root;
                }
                self.tokenizer.next()
            };

            let rhs = self.parse_operand();
            root = match operator {
                Token::Addition => Tree::Addition(Box::new(root), Box::new(rhs)),
                Token::Subtraction => Tree::Subtraction(Box::new(root), Box::new(rhs)),
                Token::Multiplication => Tree::Multiplication(Box::new(root), Box::new(rhs)),
                Token::Division => Tree::Division(Box::new(root), Box::new(rhs)),
                Token::VariableX
                | Token::Value(_)
                | Token::Eof
                | Token::ParenLeft
                | Token::ParenRight => panic!("Expected an operator"),
            };
        }
    }

    fn parse_operand(&mut self) -> Tree {
        match self.tokenizer.next() {
            Token::VariableX => Tree::VariableX,
            Token::Value(v) => Tree::Value(v),
            Token::ParenLeft => {
                let result = self.parse_expression();
                if !matches!(self.tokenizer.next(), Token::ParenRight) {
                    panic!("Expected a closing parenthesis");
                }
                result
            }
            Token::Addition
            | Token::Subtraction
            | Token::Multiplication
            | Token::Division
            | Token::ParenRight
            | Token::Eof => panic!("Unexpected token"),
        }
    }
}

struct Solver {
    lhs: Tree,
    rhs: Tree,
}

impl Solver {
    fn new(input: &str) -> Self {
        let mut split = input.split('=');
        let lhs = Tree::new(split.next().unwrap());
        let rhs = Tree::new(split.next().unwrap());
        Self { lhs, rhs }
    }

    fn solve(mut self) -> Self {
        loop {
            match self.lhs {
                Tree::VariableX => return self,
                Tree::Value(v) => panic!("Unexpected value {v}"),
                Tree::Addition(lhs, rhs) => {
                    if lhs.contains_x() {
                        self.lhs = *lhs;
                        self.rhs = Tree::Subtraction(Box::new(self.rhs), rhs);
                    } else {
                        self.lhs = *rhs;
                        self.rhs = Tree::Subtraction(Box::new(self.rhs), lhs);
                    }
                }
                Tree::Subtraction(lhs, rhs) => {
                    if lhs.contains_x() {
                        self.lhs = *lhs;
                        self.rhs = Tree::Addition(Box::new(self.rhs), rhs);
                    } else {
                        self.lhs = *rhs;
                        self.rhs = Tree::Subtraction(lhs, Box::new(self.rhs));
                    }
                }
                Tree::Multiplication(lhs, rhs) => {
                    if lhs.contains_x() {
                        self.lhs = *lhs;
                        self.rhs = Tree::Division(Box::new(self.rhs), rhs);
                    } else {
                        self.lhs = *rhs;
                        self.rhs = Tree::Division(Box::new(self.rhs), lhs);
                    }
                }
                Tree::Division(lhs, rhs) => {
                    if lhs.contains_x() {
                        self.lhs = *lhs;
                        self.rhs = Tree::Multiplication(Box::new(self.rhs), rhs);
                    } else {
                        self.lhs = *rhs;
                        self.rhs = Tree::Division(lhs, Box::new(self.rhs));
                    }
                }
            }
        }
    }

    fn lhs(&self) -> &Tree {
        &self.lhs
    }

    fn rhs(&self) -> &Tree {
        &self.rhs
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solver_test_1() {
        let solver = Solver::new("(x + 50) * 10 - 15 = 10");
        let solver = solver.solve();
        let lhs = solver.lhs();
        let rhs = solver.rhs();
        assert_eq!(format!("{lhs} = {rhs}"), "x = (((10 + 15) / 10) - 50)");
    }

    #[test]
    fn solver_test_2() {
        let solver = Solver::new("((x + 10) + 30) = 10 + 20");
        let solver = solver.solve();
        let lhs = solver.lhs();
        let rhs = solver.rhs();
        assert_eq!(format!("{lhs} = {rhs}"), "x = (((10 + 20) - 30) - 10)");
    }

    #[test]
    fn solver_test_3() {
        let solver = Solver::new("((x + 10) + (20 + 30)) = 10 + 20");
        let solver = solver.solve();
        let lhs = solver.lhs();
        let rhs = solver.rhs();
        assert_eq!(
            format!("{lhs} = {rhs}"),
            "x = (((10 + 20) - (20 + 30)) - 10)"
        );
    }

    #[test]
    fn solver_test_4() {
        let solver = Solver::new("((20 + 30) + (x + 10)) = 10 + 20");
        let solver = solver.solve();
        let lhs = solver.lhs();
        let rhs = solver.rhs();
        assert_eq!(
            format!("{lhs} = {rhs}"),
            "x = (((10 + 20) - (20 + 30)) - 10)"
        );
    }

    #[test]
    fn solver_test_5() {
        let solver = Solver::new("(((3 * x) * 4) * 5) = 1");
        let solver = solver.solve();
        let lhs = solver.lhs();
        let rhs = solver.rhs();
        assert_eq!(format!("{lhs} = {rhs}"), "x = (((1 / 5) / 4) / 3)");
    }

    #[test]
    fn solver_test_6() {
        let solver = Solver::new("(((3 / x) * 4) * 5) = 1");
        let solver = solver.solve();
        let lhs = solver.lhs();
        let rhs = solver.rhs();
        assert_eq!(format!("{lhs} = {rhs}"), "x = (3 / ((1 / 5) / 4))");
    }

    #[test]
    fn solver_test_7() {
        let solver = Solver::new("(((3 / (x + 10)) * 4) * 5) = 1");
        let solver = solver.solve();
        let lhs = solver.lhs();
        let rhs = solver.rhs();
        assert_eq!(format!("{lhs} = {rhs}"), "x = ((3 / ((1 / 5) / 4)) - 10)");
    }

    #[test]
    fn parser_test_1() {
        let mut parser = Parser::new("(x + 50) * 10 - 15");
        let tree = parser.parse();
        assert_eq!(
            tree,
            Tree::Subtraction(
                Box::new(Tree::Multiplication(
                    Box::new(Tree::Addition(
                        Box::new(Tree::VariableX),
                        Box::new(Tree::Value(50)),
                    )),
                    Box::new(Tree::Value(10)),
                )),
                Box::new(Tree::Value(15)),
            )
        );
        assert_eq!(format!("{tree}"), "(((x + 50) * 10) - 15)");
    }

    #[test]
    fn parser_test_2() {
        let mut parser = Parser::new("((x + 10) + 30)");
        let tree = parser.parse();
        assert_eq!(
            tree,
            Tree::Addition(
                Box::new(Tree::Addition(
                    Box::new(Tree::VariableX),
                    Box::new(Tree::Value(10)),
                )),
                Box::new(Tree::Value(30)),
            )
        );
        assert_eq!(format!("{tree}"), "((x + 10) + 30)");
    }

    #[test]
    fn parser_test_3() {
        let mut parser = Parser::new("((x + 10) + (20 + 30))");
        let tree = parser.parse();
        assert_eq!(
            tree,
            Tree::Addition(
                Box::new(Tree::Addition(
                    Box::new(Tree::VariableX),
                    Box::new(Tree::Value(10)),
                )),
                Box::new(Tree::Addition(
                    Box::new(Tree::Value(20)),
                    Box::new(Tree::Value(30)),
                )),
            )
        );
        assert_eq!(format!("{tree}"), "((x + 10) + (20 + 30))");
    }

    #[test]
    fn parser_test_4() {
        let mut parser = Parser::new("((20 + 30) + (x + 10))");
        let tree = parser.parse();
        assert_eq!(
            tree,
            Tree::Addition(
                Box::new(Tree::Addition(
                    Box::new(Tree::Value(20)),
                    Box::new(Tree::Value(30)),
                )),
                Box::new(Tree::Addition(
                    Box::new(Tree::VariableX),
                    Box::new(Tree::Value(10)),
                )),
            )
        );
        assert_eq!(format!("{tree}"), "((20 + 30) + (x + 10))");
    }

    #[test]
    fn parser_test_5() {
        let mut parser = Parser::new("(((3 * x) * 4) * 5)");
        let tree = parser.parse();
        assert_eq!(
            tree,
            Tree::Multiplication(
                Box::new(Tree::Multiplication(
                    Box::new(Tree::Multiplication(
                        Box::new(Tree::Value(3)),
                        Box::new(Tree::VariableX),
                    )),
                    Box::new(Tree::Value(4)),
                )),
                Box::new(Tree::Value(5)),
            ),
        );
        assert_eq!(format!("{tree}"), "(((3 * x) * 4) * 5)");
    }

    #[test]
    fn parser_test_6() {
        let mut parser = Parser::new("(((3 / (x + 10)) * 4) * 5)");
        let tree = parser.parse();
        assert_eq!(
            tree,
            Tree::Multiplication(
                Box::new(Tree::Multiplication(
                    Box::new(Tree::Division(
                        Box::new(Tree::Value(3)),
                        Box::new(Tree::Addition(
                            Box::new(Tree::VariableX),
                            Box::new(Tree::Value(10)),
                        )),
                    )),
                    Box::new(Tree::Value(4)),
                )),
                Box::new(Tree::Value(5)),
            ),
        );
        assert_eq!(format!("{tree}"), "(((3 / (x + 10)) * 4) * 5)");
    }

    #[test]
    fn tokenizer_tests() {
        let mut tokenizer = Tokenizer::new("(x + 50) * 10 - 15".as_bytes());
        assert_eq!(tokenizer.next(), Token::ParenLeft);
        assert_eq!(tokenizer.next(), Token::VariableX);
        assert_eq!(tokenizer.next(), Token::Addition);
        assert_eq!(tokenizer.next(), Token::Value(50));
        assert_eq!(tokenizer.next(), Token::ParenRight);
        assert_eq!(tokenizer.next(), Token::Multiplication);
        assert_eq!(tokenizer.next(), Token::Value(10));
        assert_eq!(tokenizer.next(), Token::Subtraction);
        assert_eq!(tokenizer.next(), Token::Value(15));
        assert_eq!(tokenizer.next(), Token::Eof);
        assert_eq!(tokenizer.next(), Token::Eof);
    }

    #[test]
    fn tokenizer_peek_tests() {
        let mut tokenizer = Tokenizer::new("(x + 50) * 10 - 15".as_bytes());
        assert_eq!(tokenizer.peek(), Token::ParenLeft);
        assert_eq!(tokenizer.next(), Token::ParenLeft);
        assert_eq!(tokenizer.peek(), Token::VariableX);
        assert_eq!(tokenizer.next(), Token::VariableX);
        assert_eq!(tokenizer.peek(), Token::Addition);
        assert_eq!(tokenizer.next(), Token::Addition);
        assert_eq!(tokenizer.peek(), Token::Value(50));
        assert_eq!(tokenizer.next(), Token::Value(50));
        assert_eq!(tokenizer.peek(), Token::ParenRight);
        assert_eq!(tokenizer.next(), Token::ParenRight);
        assert_eq!(tokenizer.peek(), Token::Multiplication);
        assert_eq!(tokenizer.next(), Token::Multiplication);
        assert_eq!(tokenizer.peek(), Token::Value(10));
        assert_eq!(tokenizer.next(), Token::Value(10));
        assert_eq!(tokenizer.peek(), Token::Subtraction);
        assert_eq!(tokenizer.next(), Token::Subtraction);
        assert_eq!(tokenizer.peek(), Token::Value(15));
        assert_eq!(tokenizer.next(), Token::Value(15));
        assert_eq!(tokenizer.peek(), Token::Eof);
        assert_eq!(tokenizer.next(), Token::Eof);
        assert_eq!(tokenizer.peek(), Token::Eof);
        assert_eq!(tokenizer.next(), Token::Eof);
    }
}
