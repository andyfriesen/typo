mod parse;

#[test]
fn join() {
    let p = parse::JoinParser { p1: ~parse::ParseIdentifier
                              , p2: ~parse::ParseKeyword
                              };
}
