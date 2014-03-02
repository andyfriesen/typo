
use std::str::CharRange;

#[deriving(ToStr)]
pub enum Keyword {
    Any,
    Boolean,
    Break,
    Case,
    Catch,
    Class,
    Constructor,
    Continue,
    Delete,
    Enum,
    Export,
    Extends,
    False,
    For,
    Function,
    If,
    In,
    InstanceOf,
    Interface,
    Let,
    Module,
    New,
    Null,
    Number,
    Private,
    Protected,
    Public,
    Return,
    Static,
    String,
    Super,
    Switch,
    This,
    Throw,
    True,
    Try,
    TypeOf,
    Undefined,
    Var,
    Void,
    While,
    With,
}

static keywords : &'static[(&'static str, Keyword)] = &[
    ("any",          Any),
    ("boolean",      Boolean),
    ("break",        Break),
    ("case",         Case),
    ("catch",        Catch),
    ("class",        Class),
    ("constructor",  Constructor),
    ("continue",     Continue),
    ("delete",       Delete),
    ("enum",         Enum),
    ("export",       Export),
    ("extends",      Extends),
    ("false",        False),
    ("for",          For),
    ("function",     Function),
    ("if",           If),
    ("in",           In),
    ("instanceof",   InstanceOf),
    ("interface",    Interface),
    ("let",          Let),
    ("module",       Module),
    ("new",          New),
    ("null",         Null),
    ("number",       Number),
    ("private",      Private),
    ("protected",    Protected),
    ("public",       Public),
    ("return",       Return),
    ("static",       Static),
    ("string",       String),
    ("super",        Super),
    ("switch",       Switch),
    ("this",         This),
    ("throw",        Throw),
    ("true",         True),
    ("try",          Try),
    ("typeof",       TypeOf),
    ("undefined",    Undefined),
    ("var",          Var),
    ("void",         Void),
    ("while",        While),
    ("with",         With),
];

#[deriving(ToStr)]
enum Operator {
    Ampersand,
    AmpersandEquals,
    Bang,
    Caret,
    CaretEquals,
    CloseBrace,
    CloseBracket,
    CloseParen,
    Colon,
    Comma,
    Dot,
    DoubleAmpersand,
    DoublePipe,
    Equals,
    FatRightArrow, // =>
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Minus,
    MinusEquals,
    MinusMinus,
    NotEqual,
    OpenBrace,
    OpenBracket,
    OpenParen,
    Percent,
    PercentEquals,
    Pipe,
    PipeEquals,
    Plus,
    PlusEquals,
    PlusPlus,
    QuestionMark,
    ShiftLeft,
    ShiftLeftEquals,
    ShiftRight,
    ShiftRightEquals,
    SignedShiftRight,
    SignedShiftRightEquals,
    Slash,
    SlashEquals,
    Star,
    StarEquals,
    StrictEqual,
    StrictNotEqual,
    Tilde,
}

static operators : &'static[(&'static str, Operator)] = &[
    (">>>=",  SignedShiftRightEquals),
    ("<<=",   ShiftLeftEquals),
    (">>=",   ShiftRightEquals),
    (">>>",   SignedShiftRight),
    ("===",   StrictEqual),
    ("!==",   StrictNotEqual),
    ("/=",    SlashEquals),
    ("*=",    StarEquals),
    (">>",    ShiftRight),
    ("&=",    AmpersandEquals),
    ("^=",    CaretEquals),
    ("&&",    DoubleAmpersand),
    ("||",    DoublePipe),
    ("=>",    FatRightArrow),
    (">=",    GreaterEqual),
    ("<=",    LessEqual),
    ("-=",    MinusEquals),
    ("--",    MinusMinus),
    ("!=",    NotEqual),
    ("%=",    PercentEquals),
    ("|=",    PipeEquals),
    ("+=",    PlusEquals),
    ("++",    PlusPlus),
    ("<<",    ShiftLeft),
    ("~",     Tilde),
    ("&",     Ampersand),
    ("!",     Bang),
    ("^",     Caret),
    ("}",     CloseBrace),
    (")",     CloseBracket),
    (")",     CloseParen),
    (":",     Colon),
    (",",     Comma),
    (".",     Dot),
    ("=",     Equals),
    (">",     Greater),
    ("<",     Less),
    ("-",     Minus),
    ("{",     OpenBrace),
    ("(",     OpenBracket),
    ("(",     OpenParen),
    ("%",     Percent),
    ("|",     Pipe),
    ("+",     Plus),
    ("?",     QuestionMark),
    ("/",     Slash),
    ("*",     Star),
];

#[deriving(ToStr)]
enum Token {
    Keyword(~Keyword),
    Operator(~Operator),
    Identifier(~str)
}

#[deriving(ToStr)]
struct Lexeme {
    tok: ~Token,
    lineNumber: uint
}

#[deriving(ToStr)]
pub struct LexerState {
    src: ~str,
    lexemes: ~[Lexeme],
    pos: uint,
    lineNo: uint
}

impl LexerState {
    fn eatWhitespace(&mut self) {
        while self.pos < self.src.len() && isWhite(self.here()) {
            if '\n' == self.here() {
                self.lineNo += 1;
                println!("lineNo = {}", self.lineNo);
            }

            self.next();
        }
    }

    fn here(&self) -> char {
        let CharRange {ch, ..} = self.src.char_range_at(self.pos);
        return ch;
    }

    fn next(&mut self) {
        let CharRange {next, ..} = self.src.char_range_at(self.pos);
        self.pos = next;
    }

    fn len(&self) -> uint {
        self.src.len()
    }

    fn eof(&self) -> bool {
        self.pos >= self.len()
    }

    fn matchOperator(&mut self, op: &str, tok: Operator) -> bool {
        if self.pos + op.len() > self.len() {
            return false;
        }

        if op == self.src.slice(self.pos, op.len()) {
            self.append(~Operator(~tok));
            self.pos += op.len();
            return true;
        }

        return false;
    }

    fn append(&mut self, t:~Token) {
        self.lexemes.push(Lexeme { tok: t, lineNumber: self.lineNo });
    }

    fn syntaxError(&self) {
        error!("Syntax error on line {}", self.lineNo);
        fail!();
    }

    pub fn lex(&mut self) {
        while !self.eof() {
            self.eatWhitespace();

            if self.eatLineComment() { continue; }
            if self.lexWord() { continue; }
            if self.lexOperator() { continue; }

            self.syntaxError();
        }
    }

    fn eatLineComment(&mut self) -> bool {
        if "//" == self.src.slice(self.pos, self.pos + 2) {
            self.pos += 2;
            while self.here() != '\n' {
                self.next();
            }

            return true;
        }
        return false;
    }

    fn lexOperator(&mut self) -> bool {
        for &(s, op) in operators.iter() {
            if self.matchOperator(s, op) {
                return true;
            }
        }
        return false;
    }

    fn lexWord(&mut self) -> bool {
        let mw = self.peekWord();
        match mw {
            None => return false,
            Some ((newpos, s)) => {

                match toKeyword(s) {
                    None => self.append(~Identifier(s.to_owned())),
                    Some(kw) => self.append(~Keyword(~kw))
                }

                self.pos = newpos;
                return true;
            }
        }
    }

    fn peekWord(&self) -> Option<(uint, ~str)> {
        let mut pos = self.pos;
        if !isLetter(self.here()) {
            return None;
        }

        loop {
            let CharRange {next, ..} = self.src.char_range_at(pos);
            pos = next;

            let here = self.here();
            if !isLetter(here) && !isNumber(here) {
                break;
            }
        }

        let r = self.src.slice(self.pos, pos).into_owned();
        return Some((pos, r));
    }
}

fn isWhite(c:char) -> bool {
    match c {
        ' ' => true,
        '\t'  => true,
        '\n' => true,
        '\r' => true,
        _  => false
    }
}

fn isLetter(c:char) -> bool {
    (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z') ||
    c == '_'
}

fn isNumber(c:char) -> bool {
    c >= '0' && c <= '9'
}

fn toKeyword(s:&str) -> Option<Keyword> {
    for &(skw, kw) in keywords.iter() {
        if skw == s {
            return Some(kw);
        }
    }
    return None;
}
