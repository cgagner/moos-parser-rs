use lalrpop_util::{lalrpop_mod, ParseError};

lalrpop_mod!(
    #[allow(clippy::all, dead_code, unused_imports, unused_mut)]
    pub moos
); // syntesized by LALRPOP

#[derive(Debug)]
pub enum Line<'input> {
    Comment(&'input str),
    Define(&'input str, &'input str, Option<&'input str>),
    BlockBegin(&'input str, Option<&'input str>),
    BlockEnd(Option<&'input str>),
    Assignment(&'input str, &'input str, Option<&'input str>),
    EmptyLine,
}

// ----------------------------------------------------------------------------
// Tests
#[cfg(test)]
mod tests {

    use crate::{
        error::ParseError,
        lexer::{Lexer, Token},
    };

    lalrpop_mod!(pub moos); // syntesized by LALRPOP

    #[test]
    fn test_line_parser() {
        let input = r#"
        // Test Mission File
        ServerHost   = localhost
        ServerPort   = 9000
        Community    = alpha






        //${TEST}      = 12
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

          watch_only = pHelmIvP,pMarineViewer
        }
        "#;

        let mut lexer = Lexer::new(input);

        while let Some(Ok((_, token, _))) = lexer.next() {
            println!("Parser Token: {:?}", token);
        }

        let result = moos::LinesParser::new().parse(input, lexer);
        println!("Result: {:?}", result);
    }
}
