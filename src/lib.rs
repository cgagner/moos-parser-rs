#[macro_use]
extern crate lalrpop_util;

mod error;
mod helpers;
mod lexer;
mod parser;

lalrpop_mod!(pub moos); // syntesized by LALRPOP

use crate::lexer::Lexer;
use std::str::FromStr;

#[test]
fn calculator1() {
    // assert!(moos::TermParser::new().parse("22").is_ok());
    // assert!(moos::TermParser::new().parse("( 22 )").is_ok());
    // assert!(moos::TermParser::new().parse("((((22))))").is_ok());
    // assert!(moos::TermParser::new().parse("((22)").is_err());
    // assert!(moos::TermParser::new().parse("(22e-2)").is_ok());
    assert!(!','.is_alphanumeric());
}

// fn main() {
//     // let input = r#" define: test=1 // Test Comment1"#;
//     // let lexer = Lexer::new(input);
//     // if let Ok(result) = moos::MoosParser::new().parse(input, lexer) {
//     //     println!("{:?}", result);
//     // }

//     // let input = r#" ProcessConfig = MyApp // Test Comment2"#;
//     // //let input = r#" ProcessConfig = MyApp"#;
//     // let lexer = Lexer::new(input);
//     // if let Ok(result) = moos::MoosParser::new().parse(input, lexer) {
//     //     println!("{:?}", result);
//     // }

//     // let input = r#"a!@#$%^&*;:,<.>/?|test 'This is a \'test\' '"#;
//     // let lexer = Lexer::new(input);
//     // if let Ok(result) = moos::MoosParser::new().parse(input, lexer) {
//     //     println!("{:?}", result);
//     // }

//     // let input = r#"  "This is a \"test\" " "#;
//     // let lexer = Lexer::new(input);
//     // if let Ok(result) = moos::MoosParser::new().parse(input, lexer) {
//     //     println!("{:?}", result);
//     // }

//     // let input = r#"  // This is a "comment""#;
//     // let lexer = Lexer::new(input);
//     // if let Ok(result) = moos::MoosParser::new().parse(input, lexer) {
//     //     println!("{:?}", result);
//     // }

//     // let input = r#"  "// This is a quote""#;
//     // let lexer = Lexer::new(input);
//     // if let Ok(result) = moos::MoosParser::new().parse(input, lexer) {
//     //     println!("{:?}", result);
//     // }

//     // let input = r#"  '// This is a quote'"#;
//     // let lexer = Lexer::new(input);
//     // if let Ok(result) = moos::MoosParser::new().parse(input, lexer) {
//     //     println!("{:?}", result);
//     // }

//     // let input = "  '// Check multi-line string\nAnother quote'";
//     // let lexer = Lexer::new(input);
//     // if let Ok(result) = moos::MoosParser::new().parse(input, lexer) {
//     //     println!("{:?}", result);
//     // }

//     let s: &str = r#"" this is a test ""#;
//     println!("Hello, world!, '{}'", s);

//     println!(
//         "Hello, world!, {}",
//         i32::from_str("22".trim_end_matches("L")).unwrap()
//     );
// }
