use crate::error::{ParseError, ParseErrorKind};
use std::str::CharIndices;

pub type Spanned<Token, Loc, Error> = Result<(Loc, Token, Loc), Error>;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Token<'input> {
    Comment(&'input str),
    Quote(&'input str),
    Key(&'input str),
    Boolean(bool),
    Integer(i64),
    Float(f64),
    AssignOp,
    ParenOpen,
    ParenClose,
    CurlyOpen,
    CurlyClose,
    Define(&'input str, &'input str, Option<&'input str>),
    BlockKeyword(&'input str),
    ValueString(&'input str),
    EOL,
    EOF,
    // Variable
}

pub struct Lexer<'input> {
    chars: std::iter::Peekable<CharIndices<'input>>,
    input: &'input str,
    current: Option<(usize, char)>,
    start_of_line: bool,
    found_assign_op: bool,
    found_define_op: bool,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        let mut chars = input.char_indices().peekable();
        let current = chars.next();
        Lexer {
            chars,
            input,
            current,
            start_of_line: true,
            found_assign_op: false,
            found_define_op: false,
        }
    }

    #[inline]
    fn handle_new_line(&mut self) {
        self.start_of_line = true;
        self.found_assign_op = false;
        self.found_define_op = false;
    }

    #[inline]
    fn scan_identifier(
        &mut self,
        start_index: usize,
        _: char,
    ) -> Option<Spanned<Token<'input>, usize, ParseError>> {
        self.start_of_line = false;
        self.current = self.chars.next();
        let (identifier, end_index) = loop {
            if let Some((j, c)) = self.current {
                if c.is_alphanumeric() || c == '-' || c == '_' {
                    self.current = self.chars.next();
                } else {
                    // @TODO: Need to figure out if the next char will get
                    // parsed when the next is called again..
                    break (&self.input[start_index..j], j);
                }
            } else {
                // TODO: This should report an error.. Unexpected end of file
                break (&self.input[start_index..], self.input.len());
            }
        };

        // Handle any keywords
        if identifier.eq_ignore_ascii_case("define") {
            if let Some((j, ':')) = self.current {
                self.found_define_op = true;
                self.current = self.chars.next();
                return Some(Ok((
                    start_index,
                    Token::Define(&self.input[start_index..j], "", None),
                    j,
                )));
            } else {
                // TODO: Error
            }
        } else if identifier.eq_ignore_ascii_case("processconfig") {
            return Some(Ok((
                start_index,
                Token::BlockKeyword(&self.input[start_index..end_index]),
                end_index,
            )));
        }

        Some(Ok((
            start_index,
            Token::Key(&self.input[start_index..end_index]),
            end_index,
        )))
    }

    #[inline]
    fn scan_quote(
        &mut self,
        i: usize,
        quote: char,
    ) -> Option<Spanned<Token<'input>, usize, ParseError>> {
        self.current = self.chars.next();
        while let Some((j, c)) = self.current {
            self.current = self.chars.next();
            if c == '\\' {
                // Take an extra char for escape characters
                if let Some((_, _)) = self.current {
                    self.current = self.chars.next()
                }
            } else if c == quote {
                return Some(Ok((i, Token::Quote(&self.input[i + 1..j]), j)));
            } else if c == '\n' {
                self.handle_new_line();
                // The original mission format didn't allow multi-line strings
                // so we'll do the same here.
                // @TODO: Report an error "missing trailing `'`
                return Some(Err(ParseError::new_missing_trailing(quote, j)));
            }
        }
        Some(Err(ParseError::new_missing_trailing(
            quote,
            self.input.len(),
        )))
    }

    #[inline]
    fn scan_comment(&mut self, i: usize) -> Option<Spanned<Token<'input>, usize, ParseError>> {
        self.current = self.chars.next();

        if let Some((_, '/')) = self.current {
            self.current = self.chars.next();
        } else {
            return None; // Should never get here
        }
        let mut skip_start = 2;

        // Remove white space before the comment
        while let Some((_, c)) = self.current {
            match c {
                ' ' | '\r' | '\t' => {
                    self.current = self.chars.next();
                    skip_start += 1;
                }
                _ => break,
            }
        }

        while let Some((j, c)) = self.current {
            match c {
                '\n' => {
                    self.handle_new_line();
                    return Some(Ok((i, Token::Comment(&self.input[i + skip_start..j]), j)));
                }
                _ => {}
            }
            self.current = self.chars.next();
        }
        let j = self.input.len();
        Some(Ok((i, Token::Comment(&self.input[i + skip_start..j]), j)))
    }

    #[inline]
    fn scan_char(
        &mut self,
        i: usize,
        token: Token<'input>,
    ) -> Option<Spanned<Token<'input>, usize, ParseError>> {
        self.current = self.chars.next();
        Some(Ok((i, token, i + 1)))
    }

    #[inline]
    fn scan_value(
        &mut self,
        start_index: usize,
    ) -> Option<Spanned<Token<'input>, usize, ParseError>> {
        self.current = self.chars.next();

        let (s, end_index) = loop {
            match self.current {
                None => break (&self.input[start_index..], self.input.len()),
                Some((j, c)) => match c {
                    // Skip Escape characters
                    '\\' => {
                        self.current = self.chars.next();
                        println!("Skipping escape");
                        // Take an extra char for escape characters
                        if let Some((_, cc)) = self.current {
                            println!("Skipping escape: {}", cc);
                            self.current = self.chars.next()
                        }
                    }
                    '"' | '\'' => {
                        let result = self.scan_quote(j, c);
                        if let Some(Err(e)) = result {
                            return Some(Err(e));
                        }
                    }
                    '/' => {
                        if let Some((_, '/')) = self.chars.peek() {
                            break (&self.input[start_index..j], j);
                        } else {
                            self.current = self.chars.next();
                        }
                    }
                    '\n' => {
                        break (&self.input[start_index..j], j);
                    }
                    _ => {
                        self.current = self.chars.next();
                    }
                },
            }
        };
        Some(Ok((start_index, Token::ValueString(s), end_index)))
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, usize, ParseError>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.current {
                None => return None, // End of file
                Some((i, c)) => match c {
                    ' ' | '\r' | '\t' => {} // Skip whitespace
                    '\n' => {
                        self.handle_new_line();
                        self.current = self.chars.next();
                        // After a new line, consume all of the next new lines
                        // and spaces
                        while let Some((_, c)) = self.current {
                            match c {
                                ' ' | '\r' | '\t' | '\n' => self.current = self.chars.next(),
                                _ => break,
                            }
                        }

                        return Some(Ok((i, Token::EOL, i)));
                    }
                    c if (self.start_of_line || self.found_define_op) && c.is_alphanumeric() => {
                        self.start_of_line = false;
                        return self.scan_identifier(i, c);
                    }
                    '/' => {
                        if let Some((_, '/')) = self.chars.peek() {
                            return self.scan_comment(i);
                        }
                    }
                    c if self.found_assign_op
                        && (c.is_alphanumeric() || c.is_ascii_punctuation()) =>
                    {
                        // TODO: This needs to consume everything until the
                        // end of the line
                        return self.scan_value(i);
                    }

                    '{' => return self.scan_char(i, Token::CurlyOpen),
                    '}' => return self.scan_char(i, Token::CurlyClose),
                    '(' => return self.scan_char(i, Token::ParenOpen),
                    ')' => return self.scan_char(i, Token::ParenClose),
                    '=' => {
                        self.found_assign_op = true;
                        return self.scan_char(i, Token::AssignOp);
                    }
                    c => return Some(Err(ParseError::new_unexpected_symbol(c, i))),
                },
            }
            self.current = self.chars.next();
        }
    }
}

// ----------------------------------------------------------------------------
// Tests
#[cfg(test)]
mod tests {

    use crate::{
        error::ParseError,
        lexer::{Lexer, Token},
    };

    #[test]
    pub fn test_scan_quote() {
        let input = r#"  "// This is a quote""#;
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert_eq!(
            (2_usize, Token::Quote("// This is a quote"), 21_usize),
            iter.unwrap().unwrap()
        );

        let input = r#"  '// This is a quote'"#;
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert_eq!(
            (2_usize, Token::Quote("// This is a quote"), 21_usize),
            iter.unwrap().unwrap()
        );

        let input = "  '// Check multi-line string'\n'Another quote'";
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert_eq!(
            (
                2_usize,
                Token::Quote("// Check multi-line string"),
                29_usize
            ),
            iter.unwrap().unwrap()
        );

        let iter = lexer.next();
        assert_eq!((30_usize, Token::EOL, 30_usize), iter.unwrap().unwrap());
        let iter = lexer.next();
        assert_eq!(
            (31_usize, Token::Quote("Another quote"), 45_usize),
            iter.unwrap().unwrap()
        );

        // Test when the quote is the last line
        let input = "  \"// This is a quote";
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert!(iter.is_some());
        assert!(iter.unwrap().is_err());

        if let Err(e) = iter.unwrap() {
            assert_eq!(e, ParseError::new_missing_trailing('"', input.len()))
        }

        // Test when the quote is there is a new line before the next quote
        let input = "  \"// This is a quote\n";
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert!(iter.is_some());
        assert!(iter.unwrap().is_err());
        if let Err(e) = iter.unwrap() {
            assert_eq!(e, ParseError::new_missing_trailing('"', input.len() - 1))
        }

        // Test when the quote is the last line
        let input = "  '// This is a quote";
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert!(iter.is_some());
        assert!(iter.unwrap().is_err());

        if let Err(e) = iter.unwrap() {
            assert_eq!(e, ParseError::new_missing_trailing('\'', input.len()))
        }

        // Test when the quote is there is a new line before the next quote
        let input = "  '// This is a quote\n";
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert!(iter.is_some());
        assert!(iter.unwrap().is_err());
        if let Err(e) = iter.unwrap() {
            assert_eq!(e, ParseError::new_missing_trailing('\'', input.len() - 1))
        }
    }

    #[test]
    pub fn test_scan_comment() {
        let input = r#"  // This is a "comment""#;
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert_eq!(
            (2_usize, Token::Comment("This is a \"comment\""), 24_usize),
            iter.unwrap().unwrap()
        );

        let input = r#" ProcessConfig = MyApp // This is a "comment""#;
        let mut lexer = Lexer::new(input);

        let mut i = 0;
        let expected_tokens = vec![
            Token::BlockKeyword("ProcessConfig"),
            Token::AssignOp,
            Token::ValueString("MyApp"),
            Token::Comment("This is a \"comment\""),
        ];
        check_tokens(&mut lexer, expected_tokens);
    }

    #[test]
    pub fn test_scan_value() {
        let input = r#"TestValue = This is a Test "Comment // Test" \"// Actual Comment"#;
        let mut lexer = Lexer::new(input);
        let expected_tokens = vec![
            Token::Key("TestValue"),
            Token::AssignOp,
            Token::ValueString(r#"This is a Test "Comment // Test" \""#),
            Token::Comment("Actual Comment"),
        ];
        check_tokens(&mut lexer, expected_tokens);
    }

    fn check_tokens(lexer: &mut Lexer, expected_tokens: Vec<Token>) {
        let mut i = 0;
        while let Some(Ok((_, token, _))) = lexer.next() {
            println!("Token: {:?}", token);
            if i < expected_tokens.len() {
                assert_eq!(token, expected_tokens[i]);
                i += 1;
            } else {
                break;
            }
        }
        assert_eq!(i, expected_tokens.len());
    }
}
