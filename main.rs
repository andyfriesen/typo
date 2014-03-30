
use std::io::File;
use std::str::from_utf8;

mod lex;

fn main() {
    let bytes = File::open(&Path::new("ast.ts")).read_to_end();
    let src = from_utf8(bytes).into_owned();
    //let src = ~"module";

    let mut ls = lex::lex(src);

    for l in ls.lexemes.iter() {
        println!("{:s}", l.to_str());
    }
}
