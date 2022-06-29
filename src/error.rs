use crate::lexer::Location;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct MoosParseError {
    pub kind: MoosParseErrorKind,
    pub loc_start: Location,
    pub loc_end: Location,
}

impl MoosParseError {
    pub fn new(kind: MoosParseErrorKind, loc_start: Location, loc_end: Location) -> MoosParseError {
        MoosParseError {
            kind,
            loc_start,
            loc_end,
        }
    }
    pub fn new_missing_trailing(c: char, loc_end: Location) -> MoosParseError {
        MoosParseError {
            kind: MoosParseErrorKind::MissingTrailing(c),
            loc_start: loc_end,
            loc_end,
        }
    }
    pub fn new_unexpected_symbol(c: char, loc_end: Location) -> MoosParseError {
        MoosParseError {
            kind: MoosParseErrorKind::UnexpectedSymbol(c),
            loc_start: loc_end,
            loc_end,
        }
    }
    pub fn new_missing_new_line(loc_start: Location, loc_end: Location) -> MoosParseError {
        MoosParseError {
            kind: MoosParseErrorKind::MissingNewLine,
            loc_start: loc_start,
            loc_end,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum MoosParseErrorKind {
    MissingTrailing(char),
    MissingNewLine,
    InvalidConfigBlock,
    UnexpectedSymbol(char),
}
