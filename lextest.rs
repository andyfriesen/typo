
mod lex;

fn getTokens(l:&lex::LexerState) -> ~[~lex::Token] {
    l.lexemes.iter().map(|lexeme| lexeme.tok.clone()).to_owned_vec()
}

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
    assert_eq!(getTokens(ls), ~[~lex::Keyword(lex::Module)]);
}

#[test]
fn lex_two_keywords() {
    let ls = ::lex::lex(~"module return");
    assert_eq!(getTokens(ls), ~[~lex::Keyword(lex::Module), ~lex::Keyword(lex::Return)]);
}

#[test]
fn operators() {
    let ls = lex::lex(~">>=||!");
    assert_eq!(
        getTokens(ls),
        ~[ ~lex::Operator(lex::ShiftRightEquals)
         , ~lex::Operator(lex::DoublePipe)
         , ~lex::Operator(lex::Bang)
         ]
    );
}

#[test]
fn identifiers() {
    let ls = lex::lex(~"module foo");
    assert_eq!(
        getTokens(ls),
        ~[ ~lex::Keyword(lex::Module)
         , ~lex::Identifier(~"foo")
         ]
    );
}

#[test]
fn stringliteral() {
    let ls = lex::lex(~"\"abc\"");
    assert_eq!(
        getTokens(ls),
        ~[ ~lex::StringLiteral(~"abc") ]
    );
}

#[test]
fn twoStringLiterals() {
    let ls = lex::lex(~"\"abc\"\"def\"");
    assert_eq!(
        getTokens(ls),
        ~[ ~lex::StringLiteral(~"abc")
         , ~lex::StringLiteral(~"def")
         ]
    );
}

#[test]
fn numbers() {
    let ls = lex::lex(~"12345");
    assert_eq!(
        getTokens(ls),
        ~[ ~lex::NumericLiteral(~"12345") ]
    );
}

#[test]
fn decimal_float() {
    let ls = lex::lex(~"3.14159");
    assert_eq!(
        getTokens(ls),
        ~[ ~lex::NumericLiteral(~"3.14159") ]
    );
}
