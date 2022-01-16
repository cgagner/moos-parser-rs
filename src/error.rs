#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ParseError {
    kind: ParseErrorKind,
    loc_start: usize,
    loc_end: usize,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, loc_start: usize, loc_end: usize) -> ParseError {
        ParseError {
            kind,
            loc_start,
            loc_end,
        }
    }
    pub fn new_missing_trailing(c: char, loc_end: usize) -> ParseError {
        ParseError {
            kind: ParseErrorKind::MissingTrailing(c),
            loc_start: loc_end,
            loc_end: loc_end,
        }
    }
    pub fn new_unexpected_symbol(c: char, loc_end: usize) -> ParseError {
        ParseError {
            kind: ParseErrorKind::UnexpectedSymbol(c),
            loc_start: loc_end,
            loc_end: loc_end,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ParseErrorKind {
    MissingTrailing(char),
    InvalidConfigBlock,
    UnexpectedSymbol(char),
}
