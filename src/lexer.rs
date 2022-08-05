use crate::error::MoosParseError;

use core::cmp::max;
use core::str;
use core::str::{CharIndices, ParseBoolError};
use std::ascii::AsciiExt;
use std::collections::VecDeque;
use std::num::{ParseFloatError, ParseIntError};

pub type Spanned<Token, Loc, Error> = Result<(Loc, Token, Loc), Error>;
pub type TokenQueue<'input> = VecDeque<Spanned<Token<'input>, Location, MoosParseError<'input>>>;

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
    PartialQuote(&'input str, char),
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
    PartialVariable(&'input str, char),
    MacroDefine,
    MacroInclude,
    MacroIfDef,
    MacroIfNotDef,
    MacroElseIfDef,
    MacroElse,
    MacroEndIf,
    UnknownMacro(&'input str),
    OrOperator,
    AndOperator,
    /// End of Line
    EOL,
    /// End of File
    EOF,
}

pub trait TokenListener {
    fn handle_token(&mut self, token: &Token, start_loc: &Location, end_loc: &Location);
}

pub struct Lexer<'input, 'listen> {
    token_listeners: Vec<&'listen mut dyn TokenListener>,
    chars: std::iter::Peekable<CharIndices<'input>>,
    input: &'input str,
    current: Option<(usize, char)>,
    line_number: usize,
    char_count: usize,
    start_of_line: bool,
    found_assign_op: bool,
    found_define_op: bool,
    found_block_keyword: bool,
    token_queue: TokenQueue<'input>,
}

impl<'input, 'listen> Lexer<'input, 'listen> {
    pub fn new(input: &'input str) -> Self {
        let mut chars = input.char_indices().peekable();
        let current = chars.next();
        Lexer {
            token_listeners: vec![],
            chars,
            input,
            current,
            line_number: 0,
            char_count: 0,
            start_of_line: true,
            found_assign_op: false,
            found_define_op: false,
            found_block_keyword: false,
            token_queue: TokenQueue::new(),
        }
    }

    pub fn add_listener(&mut self, token_listener: &'listen mut dyn TokenListener) {
        self.token_listeners.push(token_listener);
    }

    pub fn clear_listeners(&mut self) {
        self.token_listeners.clear();
    }

    #[inline]
    pub(crate) fn get_location(&self, index: usize) -> Location {
        Location::new(self.line_number, max(index - self.char_count, 0))
    }

    /// Find the next whitespace after the specified `start_index`
    fn find_next_whitespace(&mut self, input: &str, start_index: usize) -> (usize, char) {
        // Handle special cases
        if let Some(v) = input[start_index..]
            .char_indices()
            .find(|&(_index, c)| c == ' ' || c == '\t' || c == '\n')
        {
            (v.0 + start_index, v.1)
        } else {
            (input.len(), '\0')
        }
    }

    /// Advance the iterator to the next white space. This method is typically
    /// used in conjunction with `find_next_whitespace`.
    fn advance_to_whitespace(&mut self, next_whitespace: (usize, char)) {
        let current = loop {
            if let Some((ii, cc)) = self.chars.next() {
                if ii == next_whitespace.0 {
                    break Some((ii, cc));
                }
            } else {
                break None;
            }
        };

        if next_whitespace.1 != '\n' {
            self.current = self.chars.next();
        } else {
            self.current = current;
            self.handle_new_line();
        }
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
    ) -> Option<Spanned<Token<'input>, Location, MoosParseError<'input>>> {
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
                // TODO: Error - This might be handled in the parser
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
    ) -> Option<Spanned<Token<'input>, Location, MoosParseError<'input>>> {
        self.current = self.chars.next();

        while let Some((j, c)) = self.current {
            // Only move the iterator if we haven't reached a new line
            if c != '\n' {
                self.current = self.chars.next();
            }
            if c == '\\' {
                // Take an extra char for escape characters
                if let Some((_, _)) = self.current {
                    self.current = self.chars.next()
                }
                continue;
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
                return Some(Ok((
                    self.get_location(i),
                    Token::PartialQuote(&self.input[i + 1..j], quote),
                    self.get_location(j),
                )));
            }
        }
        Some(Ok((
            self.get_location(i),
            Token::PartialQuote(&self.input[i + 1..], quote),
            self.get_location(self.input.len()),
        )))
    }

    #[inline]
    fn scan_comment(
        &mut self,
        start_index: usize,
    ) -> Option<Spanned<Token<'input>, Location, MoosParseError<'input>>> {
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
    ) -> Option<Spanned<Token<'input>, Location, MoosParseError<'input>>> {
        self.current = self.chars.next();
        Some(Ok((self.get_location(i), token, self.get_location(i + 1))))
    }

    #[inline]
    fn scan_value(
        &mut self,
        start_index: usize,
    ) -> Option<Spanned<Token<'input>, Location, MoosParseError<'input>>> {
        let (s, _end_index) = loop {
            match self.current {
                None => break (&self.input[start_index..], self.input.len()),
                Some((j, c)) => match c {
                    // Skip Escape characters
                    '\\' => {
                        // TODO: Should we skip escaped new lines? The
                        // original parser did not
                        self.current = self.chars.next();
                        log::trace!("Skipping escape");
                        // Take an extra char for escape characters
                        if let Some((_, cc)) = self.current {
                            log::trace!("Skipping escape: {}", cc);
                            self.current = self.chars.next()
                        }
                    }

                    // NOTE: I don't think we care about quotes here. If a
                    // quote mark is in the middle of a value string, just
                    // treat the whole thing as a string...
                    //
                    // // Handle Quotes
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
                        // TODO: Need to verify this does NOT consume EOL
                        break (&self.input[start_index..j], j);
                    }
                    c if self.found_block_keyword && c == '{' => {
                        self.handle_new_line();
                        break (&self.input[start_index..j], j);
                    }
                    _ => {
                        // Handle special cases
                        let next_whitespace = self.find_next_whitespace(self.input, start_index);

                        if let Ok(value) =
                            Self::scan_integer(&self.input[start_index..next_whitespace.0])
                        {
                            self.advance_to_whitespace(next_whitespace);
                            return Some(Ok((
                                self.get_location(start_index),
                                Token::Integer(value),
                                self.get_location(next_whitespace.0),
                            )));
                        } else if let Ok(value) =
                            Self::scan_float(&self.input[start_index..next_whitespace.0])
                        {
                            self.advance_to_whitespace(next_whitespace);
                            return Some(Ok((
                                self.get_location(start_index),
                                Token::Float(value),
                                self.get_location(next_whitespace.0),
                            )));
                        } else if let Ok(value) =
                            Self::scan_bool(&self.input[start_index..next_whitespace.0])
                        {
                            self.advance_to_whitespace(next_whitespace);
                            return Some(Ok((
                                self.get_location(start_index),
                                Token::Boolean(value),
                                self.get_location(next_whitespace.0),
                            )));
                        }

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

    // TODO: This does not handle cases where the variable is in the
    // middle of a string. E.G:
    // test = targ_${VEHICLE}.bhv
    fn scan_variable(
        &mut self,
        i: usize,
    ) -> Option<Spanned<Token<'input>, Location, MoosParseError<'input>>> {
        self.current = self.chars.next();

        if let Some((_, '{')) = self.current {
            self.current = self.chars.next();
        } else {
            return None; // Should never get here
        }

        while let Some((j, c)) = self.current {
            // Only move the iterator if we haven't reached a new line
            if c != '\n' {
                self.current = self.chars.next();
            }
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
                return Some(Ok((
                    self.get_location(i),
                    Token::PartialVariable(&self.input[i + 2..j], '}'),
                    self.get_location(j),
                )));
            }
        }
        Some(Ok((
            self.get_location(i),
            Token::PartialVariable(&self.input[i + 2..], '}'),
            self.get_location(self.input.len()),
        )))
    }

    fn handle_partial_string(
        &mut self,
        remaining: &'input str,
        tok_start: &mut usize,
        index: usize,
        offset: usize,
        allow_primitives_count: usize,
    ) {
        println!("start: {:?}  -  end: {:?}", *tok_start, index);
        if *tok_start != index {
            let string = &remaining[*tok_start..index].trim();

            let token = if allow_primitives_count == 1 {
                if let Ok(value) = Self::scan_integer(string) {
                    Token::Integer(value)
                } else if let Ok(value) = Self::scan_float(string) {
                    Token::Float(value)
                } else if let Ok(value) = Self::scan_bool(string) {
                    Token::Boolean(value)
                } else {
                    // If all else fails, add it as a string
                    Token::ValueString(string)
                }
            } else {
                Token::ValueString(string)
            };

            self.token_queue.push_back(Ok((
                self.get_location(offset + *tok_start), // TODO: Need to calculate the correct offset
                token,
                self.get_location(offset + index - 1),
            )));

            *tok_start = index;
        };
    }

    #[inline]
    fn scan_macro(
        &mut self,
        i: usize,
    ) -> Option<Spanned<Token<'input>, Location, MoosParseError<'input>>> {
        self.current = self.chars.next();

        let line = if let Some((line, _rem)) = self.input[i..].split_once('\n') {
            line
        } else {
            &self.input[i..]
        };

        println!("LINE: {:?}", line);

        let mut macro_index = 1;
        // Skip spaces between the # and macro keyword
        for (ii, cc) in line[macro_index..].char_indices() {
            match cc {
                ' ' | '\r' | '\t' => {}
                _ => {
                    macro_index = ii + 1;
                    break;
                }
            }
        }

        let next_whitespace = self.find_next_whitespace(line, macro_index);

        if line[macro_index..next_whitespace.0].is_empty() {
            return None;
        }

        // [endif|else] - [comment]
        // include [quote|string] [comment]
        // define [key|variable] [value|variable] [comment]
        // [ifdef|elseifdef] [condition] [|| &&] [condition]
        // ifndef [key|variable] [comment]

        let token = match line[macro_index..next_whitespace.0]
            .to_ascii_lowercase()
            .as_str()
        {
            "define" => Token::MacroDefine,
            "else" => Token::MacroElse,
            "elseifdef" => Token::MacroElseIfDef,
            "endif" => Token::MacroEndIf,
            "ifdef" => Token::MacroIfDef,
            "ifndef" => Token::MacroIfNotDef,
            "include" => Token::MacroInclude,
            _ => Token::UnknownMacro(&line[macro_index..next_whitespace.0]),
        };

        let found_define_keyword = if let Token::MacroDefine = token {
            true
        } else {
            false
        };

        // Consume the whole line
        for _n in 1..line.len() {
            self.current = self.chars.next();
        }

        self.token_queue.push_back(Ok((
            self.get_location(i),
            token,
            self.get_location(i + next_whitespace.0),
        )));

        let remaining = &line[next_whitespace.0..];
        let mut remaining_chars = remaining.char_indices().peekable();
        let mut iter = remaining_chars.next();

        // Need to scan for quotes, comments, &&, ||, Variables, numbers

        let mut tok_start = iter.unwrap_or((0, '\0')).0;

        let offset = next_whitespace.0 + i;

        let mut allow_primitives_count = 0_usize;

        'scan_line: loop {
            let next_c = remaining_chars.peek().unwrap_or(&(0, '\0')).1;
            match iter {
                None => {
                    // End of line - Check if we've parsed a word
                    // Push out a new word token
                    let word = &remaining[tok_start..];
                    if !word.is_empty() {
                        println!("End of line word: {:?}", word);
                        self.handle_partial_string(
                            remaining,
                            &mut tok_start,
                            remaining.len(),
                            offset,
                            allow_primitives_count,
                        );
                    }
                    break 'scan_line;
                }
                Some((ii, cc)) => match cc {
                    // Skip whitespace
                    cc if (!found_define_keyword || allow_primitives_count == 0) && cc == ' '
                        || cc == '\r'
                        || cc == '\t' =>
                    {
                        // Push out a new word token
                        let word = &remaining[tok_start..ii].trim();
                        if !word.is_empty() {
                            println!("Token word: {:?}", word);
                            self.handle_partial_string(
                                remaining,
                                &mut tok_start,
                                ii,
                                offset,
                                allow_primitives_count,
                            );
                            allow_primitives_count += 1;
                        }

                        // Need to skip additional spaces without creating
                        // words
                        loop {
                            let next_c = remaining_chars.peek().unwrap_or(&(0, '\0')).1;
                            match next_c {
                                '\0' => break 'scan_line,
                                ' ' | '\r' | '\t' => {}
                                _ => break,
                            }
                            iter = remaining_chars.next();
                        }
                        if let Some((iii, _ccc)) = iter {
                            tok_start = iii + 1;
                        }
                    }
                    // // Skip Escape characters
                    // '\\' => {
                    //     iter = remaining_chars.next();
                    //     // Take an extra char for escape characters
                    //     if let Some((_, _)) = iter {
                    //         iter = remaining_chars.next()
                    //     }
                    // }
                    c if c == '/' && next_c == '/' => {
                        self.handle_partial_string(
                            remaining,
                            &mut tok_start,
                            ii,
                            offset,
                            allow_primitives_count,
                        );
                        let comment = remaining[ii + 2..].trim();
                        println!("Comment: {:?}", comment);

                        self.token_queue.push_back(Ok((
                            self.get_location(offset + ii),
                            Token::Comment(comment),
                            self.get_location(offset + remaining.len()),
                        )));

                        break 'scan_line;
                    }
                    '"' => {
                        self.handle_partial_string(
                            remaining,
                            &mut tok_start,
                            ii,
                            offset,
                            allow_primitives_count,
                        );

                        iter = remaining_chars.next();
                        // Scan for quotes
                        '_quote_loop: loop {
                            match iter {
                                None => {
                                    // Partial quote

                                    let quote = &remaining[tok_start + 1..];
                                    println!("found partial quote: {:?}", quote);

                                    self.token_queue.push_back(Ok((
                                        self.get_location(offset + ii),
                                        Token::PartialQuote(quote, '"'),
                                        self.get_location(offset + remaining.len()),
                                    )));

                                    break 'scan_line;
                                }
                                Some((iii, ccc)) => match ccc {
                                    '"' => {
                                        iter = remaining_chars.next();
                                        let quote = &remaining[tok_start + 1..iii];
                                        println!("found quote: {:?}", quote);
                                        // Found end of quote

                                        self.token_queue.push_back(Ok((
                                            self.get_location(offset + ii),
                                            Token::Quote(quote),
                                            self.get_location(offset + iii),
                                        )));

                                        tok_start = iii + 1;
                                        allow_primitives_count += 1;
                                        continue 'scan_line;
                                    }
                                    _ => iter = remaining_chars.next(),
                                },
                            }
                        }
                    }
                    c if c == '|' && next_c == '|' => {
                        self.handle_partial_string(
                            remaining,
                            &mut tok_start,
                            ii,
                            offset,
                            allow_primitives_count,
                        );
                        // Or operator
                        println!("Or operator");

                        self.token_queue.push_back(Ok((
                            self.get_location(offset + tok_start), // TODO: Need to calculate the correct offset
                            Token::OrOperator,
                            self.get_location(offset + tok_start + 1),
                        )));

                        iter = remaining_chars.next();
                        allow_primitives_count = 0;
                        tok_start += 2;
                    }
                    c if c == '&' && next_c == '&' => {
                        self.handle_partial_string(
                            remaining,
                            &mut tok_start,
                            ii,
                            offset,
                            allow_primitives_count,
                        );
                        // And operator
                        println!("And operator");

                        self.token_queue.push_back(Ok((
                            self.get_location(offset + tok_start), // TODO: Need to calculate the correct offset
                            Token::AndOperator,
                            self.get_location(offset + tok_start + 1),
                        )));

                        iter = remaining_chars.next();
                        allow_primitives_count = 0;
                        tok_start += 2;
                    }
                    c if c == '$' && next_c == '{' => {
                        // Environment variable
                    }
                    c if c == '$' && next_c == '(' => {
                        // Plug variable
                    }

                    _ => {}
                },
            }
            iter = remaining_chars.next();
        }

        if let Some(token) = self.token_queue.pop_front() {
            return Some(token);
        } else {
            None
        }
    }

    #[inline]
    fn scan_ifdef(&mut self, rest: &str) {}

    #[inline]
    fn scan_include(
        &mut self,
        i: usize,
    ) -> Option<Spanned<Token<'input>, Location, MoosParseError<'input>>> {
        None
    }

    /// Scan a string for an integer. This method handles regular integers
    /// as well as integers encoded as hex, binary, or octal.
    fn scan_integer(s: &str) -> Result<i64, ParseIntError> {
        let mut chars = s.chars().peekable();

        if s.len() > 2 && chars.nth(0).unwrap_or('\0') == '0' {
            match chars.peek() {
                Some('x') | Some('X') => return i64::from_str_radix(&s[2..], 16),
                Some('b') | Some('B') => return i64::from_str_radix(&s[2..], 2),
                Some('o') | Some('O') => return i64::from_str_radix(&s[2..], 8),
                _ => {}
            }
        }
        s.parse::<i64>()
    }

    /// Scan a string for a float.
    fn scan_float(s: &str) -> Result<f64, ParseFloatError> {
        if s.eq_ignore_ascii_case("nan") {
            println!("scan_float: {}", s);
            Ok(f64::NAN)
        } else {
            s.parse::<f64>()
        }
    }

    // Scan a string for a boolean.
    fn scan_bool(s: &str) -> Result<bool, ()> {
        if s.eq_ignore_ascii_case("true") {
            Ok(true)
        } else if s.eq_ignore_ascii_case("false") {
            Ok(false)
        } else {
            Err(())
        }
    }

    fn _next(&mut self) -> Option<Spanned<Token<'input>, Location, MoosParseError<'input>>> {
        if let Some(token) = self.token_queue.pop_front() {
            return Some(token);
        }

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
                    // c if c == '#' && next_c.is_alphanumeric() => {
                    '#' => {
                        self.start_of_line = false;
                        // found_define_op should only be true the once
                        self.found_define_op = false;
                        return self.scan_macro(i);
                    }
                    c if (self.start_of_line || self.found_define_op) && c.is_alphanumeric() => {
                        self.start_of_line = false;
                        // found_define_op should only be true the once
                        self.found_define_op = false;
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
                    _ => {
                        // Handle special cases
                        let next_whitespace = self.input[i..]
                            .char_indices()
                            .find(|&(_index, c)| c == ' ' || c == '\t' || c == '\n')
                            .unwrap_or((self.input.len() - 1, '\0'))
                            .0
                            + i;

                        if let Ok(value) = Self::scan_integer(&self.input[i..next_whitespace]) {
                            // Iterate forward to the next_whitespace
                            while let Some((ii, cc)) = self.chars.next() {
                                if ii == next_whitespace {
                                    break;
                                }
                            }
                            self.current = self.chars.next();
                            return Some(Ok((
                                self.get_location(i),
                                Token::Integer(value),
                                self.get_location(next_whitespace),
                            )));
                        } else if let Ok(value) = Self::scan_float(&self.input[i..next_whitespace])
                        {
                            // Iterate forward to the next_whitespace
                            while let Some((ii, cc)) = self.chars.next() {
                                if ii == next_whitespace {
                                    break;
                                }
                            }
                            self.current = self.chars.next();
                            return Some(Ok((
                                self.get_location(i),
                                Token::Float(value),
                                self.get_location(next_whitespace),
                            )));
                        } else if let Ok(value) = Self::scan_bool(&self.input[i..next_whitespace]) {
                            // Iterate forward to the next_whitespace
                            while let Some((ii, cc)) = self.chars.next() {
                                if ii == next_whitespace {
                                    break;
                                }
                            }
                            self.current = self.chars.next();
                            return Some(Ok((
                                self.get_location(i),
                                Token::Boolean(value),
                                self.get_location(next_whitespace),
                            )));
                        }

                        // TODO: An error here results in the rest of the file from getting parsed.

                        // TODO: Not sure we should be returning an error here
                        // This should probably just return a the entire string
                        return Some(Err(MoosParseError::new_unexpected_symbol(
                            c,
                            self.get_location(i),
                        )));
                    }
                },
            }
            self.current = self.chars.next();
        }
    }
}

impl<'input, 'listen> Iterator for Lexer<'input, 'listen> {
    type Item = Spanned<Token<'input>, Location, MoosParseError<'input>>;
    fn next(&mut self) -> Option<Self::Item> {
        let rtn = self._next();

        for listener in &mut self.token_listeners {
            if let Some(Ok((start_loc, token, end_loc))) = rtn {
                listener.handle_token(&token, &start_loc, &end_loc);
            }
        }
        return rtn;
    }
}
// ----------------------------------------------------------------------------
// Tests
#[cfg(test)]
mod tests {
    use crate::{
        error::MoosParseError,
        lexer::{Lexer, Location, Token, TokenListener},
    };
    use log;

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
        let input = "  \"// This is a partial quote";
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert!(iter.is_some());
        // This is no longer an error. It should be a partial quote
        assert!(!iter.unwrap().is_err());

        println!("Partial Quote: {:?}", iter.unwrap().unwrap());
        assert_eq!(
            (
                Location::new(0, 2),
                Token::PartialQuote("// This is a partial quote", '"'),
                Location::new(0, input.len()),
            ),
            iter.unwrap().unwrap()
        );

        // Test when the quote is there is a new line before the next quote
        let input = "  \"// This is a partial quote\n";
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert!(iter.is_some());
        assert!(!iter.unwrap().is_err());

        assert_eq!(
            (
                Location::new(0, 2),
                Token::PartialQuote("// This is a partial quote", '"'),
                Location::new(0, input.len() - 1),
            ),
            iter.unwrap().unwrap()
        );

        let iter = lexer.next();
        assert_eq!(
            (Location::new(0, 29), Token::EOL, Location::new(0, 29)),
            iter.unwrap().unwrap()
        );

        println!("After test: {:?}", iter.unwrap().unwrap());

        // Test when the quote is the last line
        let input = "  '// This is a partial quote";
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert!(iter.is_some());
        assert!(!iter.unwrap().is_err());

        assert_eq!(
            (
                Location::new(0, 2),
                Token::PartialQuote("// This is a partial quote", '\''),
                Location::new(0, input.len()),
            ),
            iter.unwrap().unwrap()
        );

        // Test when the quote is there is a new line before the next quote
        let input = "  '// This is a partial quote\n";
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert!(iter.is_some());
        assert!(!iter.unwrap().is_err());

        assert_eq!(
            (
                Location::new(0, 2),
                Token::PartialQuote("// This is a partial quote", '\''),
                Location::new(0, input.len() - 1),
            ),
            iter.unwrap().unwrap()
        );
    }

    #[test]
    pub fn test_scan_variable() {
        let input = "${MY_VAR}";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![Token::Variable("MY_VAR")];

        check_tokens(&mut lexer, expected_tokens);

        let input = "${MY_VAR}\n";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![Token::Variable("MY_VAR"), Token::EOL];

        check_tokens(&mut lexer, expected_tokens);

        let input = "${THIS_is_a_VARIABLE}\n";
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert_eq!(
            (
                Location::new(0, 0),
                Token::Variable("THIS_is_a_VARIABLE"),
                Location::new(0, 20)
            ),
            iter.unwrap().unwrap()
        );
        let iter = lexer.next();
        assert_eq!(
            (
                Location::new(0, input.len() - 1),
                Token::EOL,
                Location::new(0, input.len() - 1)
            ),
            iter.unwrap().unwrap()
        );
        // Test Partial Variables
        let input = "${MY_VAR";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![Token::PartialVariable("MY_VAR", '}')];

        check_tokens(&mut lexer, expected_tokens);

        let input = "${MY_VAR\n";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![Token::PartialVariable("MY_VAR", '}'), Token::EOL];

        check_tokens(&mut lexer, expected_tokens);

        let input = "${THIS_is_a_VARIABLE\n";
        let mut lexer = Lexer::new(input);
        let iter = lexer.next();
        assert_eq!(
            (
                Location::new(0, 0),
                Token::PartialVariable("THIS_is_a_VARIABLE", '}'),
                Location::new(0, 20)
            ),
            iter.unwrap().unwrap()
        );
        let iter = lexer.next();
        assert_eq!(
            (
                Location::new(0, input.len() - 1),
                Token::EOL,
                Location::new(0, input.len() - 1)
            ),
            iter.unwrap().unwrap()
        );
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

    #[test]
    fn test_scan_integer() {
        // Regular Integer
        assert_eq!(Lexer::scan_integer("12345"), Ok(12345));
        // Another Integer
        assert_eq!(Lexer::scan_integer("-12345"), Ok(-12345));

        // Hex Integer
        assert_eq!(Lexer::scan_integer("0xffff"), Ok(65535));
        assert_eq!(Lexer::scan_integer("0Xffff"), Ok(65535));
        assert_eq!(Lexer::scan_integer("0xFFFF"), Ok(65535));
        assert_eq!(Lexer::scan_integer("0XFFFF"), Ok(65535));

        // Binary Integer
        assert_eq!(Lexer::scan_integer("0b11111111"), Ok(255));
        assert_eq!(Lexer::scan_integer("0B11111111"), Ok(255));

        // Octal
        assert_eq!(Lexer::scan_integer("0o10"), Ok(8));
        assert_eq!(Lexer::scan_integer("0O10"), Ok(8));

        assert_eq!(Lexer::scan_integer("102d"), "102d".parse::<i64>());
        assert!(Lexer::scan_integer("102d").is_err());
    }

    #[test]
    fn test_scan_float() {
        let approx_eq = |lhs: f64, rhs: f64, delta: f64| -> bool {
            if lhs.is_finite() && rhs.is_finite() {
                (lhs - rhs).abs() <= delta
            } else if lhs.is_nan() && rhs.is_nan() {
                true
            } else {
                lhs == rhs
            }
        };
        assert!(approx_eq(
            Lexer::scan_float("12341.0").unwrap(),
            12341.0,
            0.0001
        ));
        assert!(approx_eq(
            Lexer::scan_float("-12341.0").unwrap(),
            -12341.0,
            0.0001
        ));
        assert!(approx_eq(
            Lexer::scan_float("2.23e3").unwrap(),
            2230.0,
            0.0001
        ));

        assert!(approx_eq(
            Lexer::scan_float("-inf").unwrap(),
            f64::NEG_INFINITY,
            0.0001
        ));
        assert!(approx_eq(
            Lexer::scan_float("inf").unwrap(),
            f64::INFINITY,
            0.0001
        ));
        assert!(approx_eq(
            Lexer::scan_float("nan").unwrap(),
            f64::NAN,
            0.0001
        ));
    }

    #[test]
    fn test_scan_bool() {
        assert_eq!(Lexer::scan_bool("true"), Ok(true));
        assert_eq!(Lexer::scan_bool("True"), Ok(true));
        assert_eq!(Lexer::scan_bool("TRUE"), Ok(true));

        assert_eq!(Lexer::scan_bool("false"), Ok(false));
        assert_eq!(Lexer::scan_bool("False"), Ok(false));
        assert_eq!(Lexer::scan_bool("FALSE"), Ok(false));
    }

    #[test]
    fn test_primitives() {
        let input = r#"
        // This is a test float
        a = 12345.0
        b = 12345

        // Another Float
        c = -12341.0
        d = -12341

        // Scientific Notation
        e = 2.23e3
        f = +1.0
        g = -inf
        h = true
        i = False
        j = TRUE
        k = trues
        l = "true"
        m = 'FALSE'
        "#;

        let mut lexer = Lexer::new(input);
        let expected_tokens = vec![
            Token::EOL,
            Token::Comment("This is a test float"),
            Token::EOL,
            Token::Key("a"),
            Token::AssignOp,
            Token::Float(12345.0),
            Token::EOL,
            Token::Key("b"),
            Token::AssignOp,
            Token::Integer(12345),
            Token::EOL,
            Token::EOL,
            Token::Comment("Another Float"),
            Token::EOL,
            Token::Key("c"),
            Token::AssignOp,
            Token::Float(-12341.0),
            Token::EOL,
            Token::Key("d"),
            Token::AssignOp,
            Token::Integer(-12341),
            Token::EOL,
            Token::EOL,
            Token::Comment("Scientific Notation"),
            Token::EOL,
            Token::Key("e"),
            Token::AssignOp,
            Token::Float(2230.0),
            Token::EOL,
            Token::Key("f"),
            Token::AssignOp,
            Token::Float(1.0),
            Token::EOL,
            Token::Key("g"),
            Token::AssignOp,
            Token::Float(f64::NEG_INFINITY),
            Token::EOL,
            Token::Key("h"),
            Token::AssignOp,
            Token::Boolean(true),
            Token::EOL,
            Token::Key("i"),
            Token::AssignOp,
            Token::Boolean(false),
            Token::EOL,
            Token::Key("j"),
            Token::AssignOp,
            Token::Boolean(true),
            Token::EOL,
            Token::Key("k"),
            Token::AssignOp,
            Token::ValueString("trues"),
            Token::EOL,
            Token::Key("l"),
            Token::AssignOp,
            Token::Quote("true"),
            Token::EOL,
            Token::Key("m"),
            Token::AssignOp,
            Token::Quote("FALSE"),
            Token::EOL,
        ];
        check_tokens(&mut lexer, expected_tokens);
    }

    #[test]
    fn test_scan_macro() {
        let input = r#"
        #include asdf 
        #include "Test.plug"
        #define VALUE00  This is a test
        #define VALUE01 This is a test // Comment after define

        #ifdef VALUE1 12 // Test Comment  
        #else // Comment
        #define VALUE2 "this is a quote"
        #include "filepath.txt"
        #else // Comment
        #ifdef VALUE3 12 || VALUE4 123
        #endif // Comments
        #endfi // Unknown macro
        "#;

        let mut lexer = Lexer::new(input);
        while let Some(Ok((_, token, _))) = lexer.next() {
            println!("test_scan_macro Token: {:?}", token);
        }

        let mut lexer = Lexer::new(input);
        let expected_tokens = vec![
            Token::EOL,
            Token::MacroInclude,
            Token::ValueString("asdf"),
            Token::EOL,
            Token::MacroInclude,
            Token::Quote("Test.plug"),
            Token::EOL,
            Token::MacroDefine,
            Token::ValueString("VALUE00"),
            Token::ValueString("This is a test"),
            Token::EOL,
            Token::MacroDefine,
            Token::ValueString("VALUE01"),
            Token::ValueString("This is a test"),
            Token::Comment("Comment after define"),
            Token::EOL,
            Token::EOL,
            Token::MacroIfDef,
            Token::ValueString("VALUE1"),
            Token::Integer(12),
            Token::Comment("Test Comment"),
            Token::EOL,
            // TODO: Need to finish added test cases for macros
        ];
        check_tokens(&mut lexer, expected_tokens);
    }

    #[test]
    fn test_listener() {
        use crate::moos;
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        struct SemanticToken {
            token_type: i32,
            modifier: i32,
            start_loc: Location,
            end_loc: Location,
        }

        struct TokenCollector {
            tokens: Vec<SemanticToken>,
        }

        impl TokenListener for TokenCollector {
            fn handle_token(&mut self, token: &Token, start_loc: &Location, end_loc: &Location) {
                match token {
                    Token::Comment(_comment) => {
                        self.tokens.push(SemanticToken {
                            token_type: 0,
                            modifier: 1,
                            start_loc: *start_loc,
                            end_loc: *end_loc,
                        });
                    }
                    Token::BlockKeyword(keyword) => {
                        self.tokens.push(SemanticToken {
                            token_type: 3,
                            modifier: 5,
                            start_loc: *start_loc,
                            end_loc: *end_loc,
                        });
                    }
                    _ => {
                        log::debug!("Unhandled token: {:?}", token)
                    }
                }
            }
        }

        let input = r#"
        //------------------------------------------
        // uMemWatch config block

        ProcessConfig = uMemWatch
        {
          AppTick   = $(POP) // Test
          CommsTick = 4

          absolute_time_gap = 1   // In Seconds, Default is 4
          log_path = "/home/user/tmp"

          watch_only = pHelmIvP,pMarineViewer
        }
        "#;

        let mut token_collector = TokenCollector { tokens: vec![] };

        let mut lexer = Lexer::new(input);
        lexer.add_listener(&mut token_collector);

        while let Some(Ok((_, token, _))) = lexer.next() {
            println!("Parser Token: {:?}", token);
        }

        lexer = Lexer::new(input);
        let mut errors = Vec::new();
        let result = moos::LinesParser::new().parse(&mut errors, input, lexer);
        assert!(result.is_ok());
        println!("Tokens: ");
        for t in &token_collector.tokens {
            println!("  {:?}", t);
        }
    }
}
