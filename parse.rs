
struct Lexeme;

struct ParseResult<T>;

trait Parser<T> {
    fn run(&self) -> ParseResult<T>;
}

struct ParseIdentifier;

impl Parser<Lexeme> for ParseIdentifier {
    fn run(&self) -> ParseResult<Lexeme> {
        ParseResult
    }
}

struct JoinParser<A, B> {
    p1 : ~Parser<A>,
    p2 : ~Parser<B>
}

impl<A, B> Parser<(A, B)> for JoinParser<A, B> {
    fn run(&self) -> ParseResult<(A, B)> { ParseResult }
}

#[test]
fn join() {
    let p = JoinParser { p1: ~ParseIdentifier
                       , p2: ~ParseIdentifier
                       };
}
