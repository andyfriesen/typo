
mod lex;

#[test]
fn empty() {
    let src = ~"";
    let ls = ::lex::lex(src);
    assert_eq!(0, ls.lexemes.len());
}

#[test]
fn lex_keyword() {
    let src = ~"module";

    let ls = ::lex::lex(src);
    assert_eq!(1, ls.lexemes.len());
    assert_eq!(lex::Keyword(~lex::Module), *ls.lexemes[0].tok);
}

#[test]
fn lex_two_keywords() {
    let ls = ::lex::lex(~"module return");
    assert_eq!(2, ls.lexemes.len());
    assert_eq!(lex::Keyword(~lex::Module), *ls.lexemes[0].tok);
    assert_eq!(lex::Keyword(~lex::Return), *ls.lexemes[1].tok);
}

#[test]
fn operators() {
    let ls = lex::lex(~">>= ||!");
    assert_eq!(3, ls.lexemes.len());
    assert_eq!(lex::Operator(~lex::ShiftRightEquals), *ls.lexemes[0].tok);
    assert_eq!(lex::Operator(~lex::DoublePipe), *ls.lexemes[1].tok);
    assert_eq!(lex::Operator(~lex::Bang), *ls.lexemes[2].tok);
}
