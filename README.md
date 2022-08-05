# moos-parser-rs

Parser for MOOS-IvP mission files for Rust.


## TODO

- [ ] Switch to scanning the whole line - Makes things easier.
- [ ] The lexer currently returns errors. This prevents the parser from recovering. See
      lalrpop start_machine.rs line 611.
- [ ] If macros are used for blocks (e.g. ProcessConfig) we need to check
      that each branch has a process config. That may also screw up the
      vs-code plugin since there may be different process configs.
- [ ] Remove the scan_identifier method and return a constant ValueString
- [ ] Replace the Key/Identifier from moos.lalrpop with ValueString
- [ ] Need to handle variables in the middle of a string
- [ ] Variables can also appear in a comment
- [ ] Support "#define $(FOO) BAR" as well as "#define FOO BAR"


## nsplug Questions

1. Is it intended that `nsplug` doesn't support comments after macros:
      ```text
      #ifdef VALUE 12 // Test Comment
      ```
1. Why does `ifndef` without a variable result in an error?
1. Should spaces in between `#` and a macro be an error? E.G. `#   ifdef`
1. Do variables in quotes get replaced? E.G. `value = "${VAR}"`
1. It looks like `%(VAL)` replaces with the uppercase version of VAL where `$(VAL)`
   just replaces VAL verbatim.
