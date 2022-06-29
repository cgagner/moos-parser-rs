use lalrpop_util::lalrpop_mod;
use lalrpop_util::ErrorRecovery;

lalrpop_mod!(
    #[allow(clippy::all, dead_code, unused_imports, unused_mut)]
    pub moos
); // synthesized by LALRPOP

#[derive(Debug)]
pub enum Value<'input> {
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(&'input str),
}

#[derive(Debug)]
pub enum Line<'input> {
    Comment(&'input str),
    Define(&'input str, Value<'input>, Option<&'input str>),
    BlockBegin(&'input str, Option<&'input str>),
    BlockEnd(Option<&'input str>),
    // TODO: Need to update the assignment to take a variant for the value
    Assignment(&'input str, Value<'input>, Option<&'input str>),
    Integer(i64),
    Error,
    EndOfLine,
}

// ----------------------------------------------------------------------------
// Tests
#[cfg(test)]
mod tests {

    use crate::lexer::Lexer;

    lalrpop_mod!(
        #[allow(clippy::all, dead_code, unused_imports, unused_mut)]
        pub moos
    ); // syntesized by LALRPOP

    #[test]
    fn test_block_newline_fail() {
        let input = r#"
        // Antler configuration  block
        ProcessConfig = ANTLER {
            MSBetweenLaunches = 200
        }
        "#;

        let mut lexer = Lexer::new(input);

        while let Some(Ok((_, token, _))) = lexer.next() {
            println!("Parser Token: {:?}", token);
        }

        lexer = Lexer::new(input);
        let mut errors = Vec::new();
        let result = moos::LinesParser::new().parse(&mut errors, input, lexer);
        println!("Result: {:?}", result);

        // // This test should fail
        // assert!(result.is_err());
        // if let Err(e) = result {
        //     assert_eq!(
        //         lalrpop_util::ParseError::User {
        //             error: crate::error::MoosParseError::new_missing_new_line(
        //                 crate::lexer::Location::new(2, 31),
        //                 crate::lexer::Location::new(2, 32),
        //             ),
        //         },
        //         e,
        //     )
        // }
    }

    #[test]
    fn test_block_newline_pass() {
        let input = r#"
        // Antler configuration  block
        ProcessConfig = ANTLER 
        {
            MSBetweenLaunches = 200
        }
        "#;

        let mut lexer = Lexer::new(input);

        while let Some(Ok((_, token, _))) = lexer.next() {
            println!("Parser Token: {:?}", token);
        }

        lexer = Lexer::new(input);
        let mut errors = Vec::new();
        let result = moos::LinesParser::new().parse(&mut errors, input, lexer);
        println!("Result: {:?}", result);

        // This test should fail
        assert!(result.is_ok());
    }

    #[test]
    fn test_line_parser() {
        let input = r#"
        // Test Mission File
        ServerHost   = localhost
        ServerPort   = 9000
        Community    = alpha






        ${TEST}      = 12
        MOOSTimeWarp = 1


        // MIT Sailing Pavilion
        LatOrigin  = 42.35846207515723
        LongOrigin = -71.08774014042629

        //------------------------------------------
        // Antler configuration  block
        ProcessConfig = ANTLER
        {
          MSBetweenLaunches = 200
          ExecutablePath = system // System path
          Run = MOOSDB          @ NewConsole =
          Run = pLogger         @ NewConsole = true
          Run = uSimMarine      @ NewConsole = false
          Run = pMarinePID      @ NewConsole = false
          Run = pHelmIvP        @ NewConsole = false
          Run = pMarineViewer	@ NewConsole = false
          Run = uProcessWatch	@ NewConsole = false
          Run = pNodeReporter	@ NewConsole = false
          Run = uMemWatch       @ NewConsole = false
        }

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

        let mut lexer = Lexer::new(input);

        while let Some(Ok((_, token, _))) = lexer.next() {
            println!("Parser Token: {:?}", token);
        }

        lexer = Lexer::new(input);
        let mut errors = Vec::new();
        let result = moos::LinesParser::new().parse(&mut errors, input, lexer);
        println!("Result: {:?}", result);
    }
}
