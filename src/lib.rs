#[macro_use]
extern crate lalrpop_util;

mod error;
mod helpers;
mod lexer;
mod parser;

lalrpop_mod!(
    #[allow(clippy::all, dead_code, unused_imports, unused_mut)]
    pub moos
); // syntesized by LALRPOP
