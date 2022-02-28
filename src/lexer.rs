use crate::error::MoosParseError;

use core::cmp::max;
use core::str::CharIndices;

pub type Spanned<Token, Loc, Error> = Result<(Loc, Token, Loc), Error>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Location {
    pub line: usize,
    pub index: usize,
}

impl Location {
    pub fn new(line: usize, index: usize) -> Self {
        Location { line, index }
    }
}

impl Default for Location {
    fn default() -> Self {
        Self { line: 0, index: 0 }
    }
}

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
    DefineKeyword,
    BlockKeyword(&'input str),
    ValueString(&'input str),
    Variable(&'input str),
    EOL,
    EOF,
    // Variable
}

pub struct Lexer<'input> {
    chars: std::iter::Peekable<CharIndices<'input>>,
    input: &'input str,
    current: Option<(usize, char)>,
    line_number: usize,
    char_count: usize,
    start_of_line: bool,
    found_assign_op: bool,
    found_define_op: bool,
    found_block_keyword: bool,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        let mut chars = input.char_indices().peekable();
        let current = chars.next();
        Lexer {
            chars,
            input,
            current,
            line_number: 0,
            char_count: 0,
            start_of_line: true,
            found_assign_op: false,
            found_define_op: false,
            found_block_keyword: false,
        }
    }
    #[inline]
    pub(crate) fn get_location(&self, index: usize) -> Location {
        Location::new(self.line_number, max(index - self.char_count, 0))
    }

    #[inline]
    fn handle_new_line(&mut self) {
        self.start_of_line = true;
        self.found_assign_op = false;
        self.found_define_op = false;
        self.found_block_keyword = false;
    }

    #[inline]
    fn scan_identifier(
        &mut self,
        start_index: usize,
        _: char,
    ) -> Option<Spanned<Token<'input>, Location, MoosParseError>> {
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
                    self.get_location(start_index),
                    Token::DefineKeyword,
                    self.get_location(j),
                )));
            } else {
                // TODO: Error
            }
        } else if identifier.eq_ignore_ascii_case("processconfig") {
            self.found_block_keyword = true;
            return Some(Ok((
                self.get_location(start_index),
                Token::BlockKeyword(&self.input[start_index..end_index]),
                self.get_location(end_index),
            )));
        }

        Some(Ok((
            self.get_location(start_index),
            Token::Key(&self.input[start_index..end_index]),
            self.get_location(end_index),
        )))
    }

    #[inline]
    fn scan_quote(
        &mut self,
        i: usize,
        quote: char,
    ) -> Option<Spanned<Token<'input>, Location, MoosParseError>> {
        self.current = self.chars.next();
        while let Some((j, c)) = self.current {
            self.current = self.chars.next();
            if c == '\\' {
                // Take an extra char for escape characters
                if let Some((_, _)) = self.current {
                    self.current = self.chars.next()
                }
            } else if c == quote {
                return Some(Ok((
                    self.get_location(i),
                    Token::Quote(&self.input[i + 1..j]),
                    self.get_location(j),
                )));
            } else if c == '\n' {
                self.handle_new_line();
                // The original mission format didn't allow multi-line strings
                // so we'll do the same here.
                // @TODO: Report an error "missing trailing `'`
                return Some(Err(MoosParseError::new_missing_trailing(
                    quote,
                    self.get_location(j),
                )));
            }
        }
        Some(Err(MoosParseError::new_missing_trailing(
            quote,
            self.get_location(self.input.len()),
        )))
    }

    #[inline]
    fn scan_comment(
        &mut self,
        start_index: usize,
    ) -> Option<Spanned<Token<'input>, Location, MoosParseError>> {
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
        // TODO: Need to figure out if this is better look at user iter.position

        // Or continue with:
        while let Some((j, c)) = self.current {
            match c {
                '\n' => {
                    self.handle_new_line();
                    return Some(Ok((
                        self.get_location(start_index),
                        Token::Comment(&self.input[start_index + skip_start..j]),
                        self.get_location(j),
                    )));
                }
                _ => {}
            }
            self.current = self.chars.next();
        }
        let j = self.input.len();
        Some(Ok((
            self.get_location(start_index),
            Token::Comment(&self.input[start_index + skip_start..j]),
            self.get_location(j),
        )))
    }

    #[inline]
    fn scan_char(
        &mut self,
        i: usize,
        token: Token<'input>,
    ) -> Option<Spanned<Token<'input>, Location, MoosParseError>> {
        self.current = self.chars.next();
        Some(Ok((self.get_location(i), token, self.get_location(i + 1))))
    }

    #[inline]
    fn scan_value(
        &mut self,
        start_index: usize,
    ) -> Option<Spanned<Token<'input>, Location, MoosParseError>> {
        let (s, end_index) = loop {
            match self.current {
                None => break (&self.input[start_index..], self.input.len()),
                Some((j, c)) => match c {
                    // Skip Escape characters
                    '\\' => {
                        // TODO: Should we skip escaped new lines? The
                        // original parser did not
                        self.current = self.chars.next();
                        println!("Skipping escape");
                        // Take an extra char for escape characters
                        if let Some((_, cc)) = self.current {
                            println!("Skipping escape: {}", cc);
                            self.current = self.chars.next()
                        }
                    }

                    // Handle Quotes
                    '"' | '\'' => {
                        let result = self.scan_quote(j, c);
                        if let Some(Err(e)) = result {
                            return Some(Err(e));
                        }
                        // TODO: Check if the next token is the end of line.
                        // this also needs to skip white space
                    }
                    // Handle Comments
                    '/' => {
                        if let Some((_, '/')) = self.chars.peek() {
                            break (&self.input[start_index..j], j);
                        } else {
                            self.current = self.chars.next();
                        }
                    }
                    // Handle New Lines
                    '\n' => {
                        break (&self.input[start_index..j], j);
                    }
                    c if self.found_block_keyword && c == '{' => {
                        self.handle_new_line();
                        break (&self.input[start_index..j], j);
                    }
                    _ => {
                        self.current = self.chars.next();
                    }
                },
            }
        };
        let s = s.trim_end();
        Some(Ok((
            self.get_location(start_index),
            Token::ValueString(s),
            self.get_location(start_index + s.len()),
        )))
    }

    fn scan_variable(
        &mut self,
        i: usize,
    ) -> Option<Spanned<Token<'input>, Location, MoosParseError>> {
        self.current = self.chars.next();

        if let Some((_, '{')) = self.current {
            self.current = self.chars.next();
        } else {
            return None; // Should never get here
        }

        while let Some((j, c)) = self.current {
            self.current = self.chars.next();
            if c == '\\' {
                // Take an extra char for escape characters
                if let Some((_, _)) = self.current {
                    self.current = self.chars.next()
                }
            } else if c == '}' {
                return Some(Ok((
                    self.get_location(i),
                    Token::Variable(&self.input[i + 2..j]),
                    self.get_location(j),
                )));
            } else if c == '\n' {
                self.handle_new_line();
                // The original mission format didn't allow multi-line strings
                // so we'll do the same here.
                // @TODO: Report an error "missing trailing `'`
                return Some(Err(MoosParseError::new_missing_trailing(
                    '}',
                    self.get_location(j),
                )));
            }
        }
        Some(Err(MoosParseError::new_missing_trailing(
            '}',
            self.get_location(self.input.len()),
        )))
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, Location, MoosParseError>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next_c = self.chars.peek().unwrap_or(&(0, '\0')).1;
            match self.current {
                None => return None, // End of file
                Some((i, c)) => match c {
                    // Skip whitespace
                    ' ' | '\r' | '\t' => {}
                    // Skip Escape characters
                    '\\' => {
                        self.current = self.chars.next();
                        // Take an extra char for escape characters
                        if let Some((_, _)) = self.current {
                            self.current = self.chars.next()
                        }
                    }
                    // New Lines
                    '\n' => {
                        self.handle_new_line();
                        self.current = self.chars.next();
                        // After a new line, consume all of the next spaces
                        while let Some((_, c)) = self.current {
                            match c {
                                ' ' | '\r' | '\t' => self.current = self.chars.next(),
                                _ => break,
                            }
                        }
                        let result =
                            Some(Ok((self.get_location(i), Token::EOL, self.get_location(i))));

                        // Store the current line count and char count
                        self.line_number += 1;
                        self.char_count = i + 1;
                        return result;
                    }
                    c if (self.start_of_line || self.found_define_op) && c.is_alphanumeric() => {
                        self.start_of_line = false;
                        return self.scan_identifier(i, c);
                    }
                    c if c == '/' && next_c == '/' => {
                        return self.scan_comment(i);
                    }
                    '"' | '\'' => {
                        return self.scan_quote(i, c);
                    }
                    c if self.found_assign_op
                        && (c.is_alphanumeric() || c.is_ascii_punctuation()) =>
                    {
                        // TODO: This needs to consume everything until the
                        // end of the line
                        return self.scan_value(i);
                    }
                    c if c == '$' && next_c == '{' => {
                        self.start_of_line = false;
                        return self.scan_variable(i);
                    }
                    '{' => return self.scan_char(i, Token::CurlyOpen),
                    '}' => return self.scan_char(i, Token::CurlyClose),
                    '(' => return self.scan_char(i, Token::ParenOpen),
                    ')' => return self.scan_char(i, Token::ParenClose),
                    '=' => {
                        self.found_assign_op = true;
                        return self.scan_char(i, Token::AssignOp);
                    }
                    c => {
                        return Some(Err(MoosParseError::new_unexpected_symbol(
                            c,
                            self.get_location(i),
                        )))
                    }
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
        error::MoosParseError,
        lexer::{Lexer, Location, Token},
    };

    #[test]
    pub fn test_scan_quote() {
        let input = r#" v =  "// This is a quote""#;
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Key("v"),
            Token::AssignOp,
            Token::Quote("// This is a quote"),
        ];

        check_tokens(&mut lexer, expected_tokens);
        // assert_eq!(
        //     (2_usize, Token::Quote("// This is a quote"), 21_usize),
        //     iter.unwrap().unwrap()
        // );

        let input = r#"  '// This is a quote'"#;
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert_eq!(
            (
                Location::new(0, 2),
                Token::Quote("// This is a quote"),
                Location::new(0, 21)
            ),
            iter.unwrap().unwrap()
        );

        let input = "  '// Check multi-line string'\n'Another quote'";
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert_eq!(
            (
                Location::new(0, 2),
                Token::Quote("// Check multi-line string"),
                Location::new(0, 29),
            ),
            iter.unwrap().unwrap()
        );

        let iter = lexer.next();
        assert_eq!(
            (Location::new(0, 30), Token::EOL, Location::new(0, 30)),
            iter.unwrap().unwrap()
        );
        let iter = lexer.next();
        assert_eq!(
            (
                Location::new(1, 0),
                Token::Quote("Another quote"),
                Location::new(1, 14),
            ),
            iter.unwrap().unwrap()
        );

        // Test when the quote is the last line
        let input = "  \"// This is a quote";
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert!(iter.is_some());
        assert!(iter.unwrap().is_err());

        if let Err(e) = iter.unwrap() {
            assert_eq!(
                e,
                MoosParseError::new_missing_trailing('"', Location::new(0, input.len()))
            )
        }

        // Test when the quote is there is a new line before the next quote
        let input = "  \"// This is a quote\n";
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert!(iter.is_some());
        assert!(iter.unwrap().is_err());
        if let Err(e) = iter.unwrap() {
            assert_eq!(
                e,
                MoosParseError::new_missing_trailing('"', Location::new(0, input.len() - 1))
            )
        }

        // Test when the quote is the last line
        let input = "  '// This is a quote";
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert!(iter.is_some());
        assert!(iter.unwrap().is_err());

        if let Err(e) = iter.unwrap() {
            assert_eq!(
                e,
                MoosParseError::new_missing_trailing('\'', Location::new(0, input.len()))
            )
        }

        // Test when the quote is there is a new line before the next quote
        let input = "  '// This is a quote\n";
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert!(iter.is_some());
        assert!(iter.unwrap().is_err());
        if let Err(e) = iter.unwrap() {
            assert_eq!(
                e,
                MoosParseError::new_missing_trailing('\'', Location::new(0, input.len() - 1))
            )
        }
    }

    #[test]
    pub fn test_scan_comment() {
        let input = r#"  // This is a "comment""#;
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert_eq!(
            (
                Location::new(0, 2),
                Token::Comment("This is a \"comment\""),
                Location::new(0, 24),
            ),
            iter.unwrap().unwrap()
        );

        let input = r#" ProcessConfig = MyApp // This is a "comment""#;
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::BlockKeyword("ProcessConfig"),
            Token::AssignOp,
            Token::ValueString("MyApp"),
            Token::Comment("This is a \"comment\""),
        ];
        check_tokens(&mut lexer, expected_tokens);

        // TODO: Currently values allow multi-line strings if you end the line
        // with a backslash to escape the new line

        let input = r#"
        name1 = value1 // This is a "comment"
        name2 = value2 // Second Comment
        name3 = this\
        is \
        a test\
        of a multi-line string"#;
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::EOL,
            Token::Key("name1"),
            Token::AssignOp,
            Token::ValueString("value1"),
            Token::Comment("This is a \"comment\""),
            Token::EOL,
            Token::Key("name2"),
            Token::AssignOp,
            Token::ValueString("value2"),
            Token::Comment("Second Comment"),
        ];
        check_tokens(&mut lexer, expected_tokens);
        for t in lexer {
            println!("Token: {:?}", t);
        }
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

    // TODO: Remove this test
    #[test]
    fn test_iterator() {
        let input = "name   \\=   value = real value";
        let mut chars = input.chars();
        let iter = chars.next();

        let mut prev = '\0';
        let pos = chars.position(|c| {
            (c == '=' && prev != '\\') || {
                prev = c;
                false
            }
        });

        let iter = chars.skip(10).next();

        println!("Pos: {:?}", pos);
        println!("Char: {:?}", iter);
    }
}
