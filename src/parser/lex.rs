use crate::parser::core::{pmatch, Parser};

use super::core::{Input, Span};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
    LetKeyword,
    RecKeyword,
    InKeyword,
    FunKeyword,
    IfKeyword,
    ThenKeyword,
    ElseKeyword,
    TrueKeyword,
    FalseKeyword,

    LParen,
    RParen,
    LCurly,
    RCurly,
    Eq,
    Semi,
    Comma,
    Colon,
    Arrow,
    Plus,
    Minus,
    Star,
    Slash,
    Dot,

    Ident,
    Num,
    Op,
    EndLine,

    ErrToken,
}

use TokenKind::*;

const KEYWORD: [(&str, TokenKind); 9] = [
    ("let", LetKeyword),
    ("rec", RecKeyword),
    ("in", InKeyword),
    ("fun", FunKeyword),
    ("if", IfKeyword),
    ("then", ThenKeyword),
    ("else", ElseKeyword),
    ("true", TrueKeyword),
    ("false", FalseKeyword),
];

const OP: [(&str, TokenKind); 14] = [
    ("(", LParen),
    (")", RParen),
    ("{", LCurly),
    ("}", RCurly),
    ("=", Eq),
    (";", Semi),
    (",", Comma),
    (":", Colon),
    ("->", Arrow),
    ("+", Plus),
    ("-", Minus),
    ("*", Star),
    ("/", Slash),
    (".", Dot),
];

pub type TokenNode<'a> = Span<'a, (&'a str, TokenKind)>;

pub fn lex<'a>(input: Span<'a, &'a str>) -> Option<(Input<'a>, Vec<TokenNode<'a>>)> {
    let ident = pmatch(|c: char| c.is_alphabetic() || c == '_')
        .with(pmatch(|c: char| c.is_alphanumeric() || c == '_').option())
        .map(|(head, tail)| {
            let tail_len = tail.map(|t| t.len()).unwrap_or(0);
            let ident = unsafe {head.data.get_unchecked(..head.len() as usize + tail_len as usize)};
            let kind = if let Some((_, k)) = KEYWORD.into_iter().find(|(k, _)| ident == *k) {
                k
            } else {
                Ident
            };
            Span {
                data: (ident, kind),
                start_offset: head.start_offset,
                end_offset: head.start_offset + head.len() + tail_len,
                path: head.path,
            }
        });
    let num = pmatch(|c: char| c.is_ascii_digit())
        .map(|x| x.map(|y| (y, Num)));
    let op = pmatch(|c: char| ('!'..='/').contains(&c) || (':'..='@').contains(&c) || ('['..='`').contains(&c) || ('{'..='~').contains(&c))
        .map(|x| {
            let token = if let Some((_, k)) = OP.into_iter().find(|(k, _)| x.data == *k) {
                k
            } else {
                Op
            };
            x.map(move |y| (y, token))
        });
    let endline = pmatch("\n").map(|x| x.map(|y| (y, EndLine)));
    let err_token = pmatch(|c: char| !c.is_ascii_whitespace())
        .map(|x| x.map(|y| (y, ErrToken)));
    fn ws<'a, A, P: Parser<Span<'a, &'a str>, A>>(p: P) -> impl Parser<Span<'a, &'a str>, A> {
        //let whitespace = pmatch(|c: char| c == ' ' || c == '\t' || c == '\r').option();
        let whitespace = pmatch(|c: char| c.is_whitespace()).option();
        p.with(whitespace).map(|(a, _)| a)
    }
    //let whitespace = pmatch(|c: char| c == ' ' || c == '\t' || c == '\r').option();
    let whitespace = pmatch(|c: char| c.is_whitespace()).option();
    whitespace.with(
        ws(ident).or(ws(num)).or(ws(op))//.or(ws(endline))
            .or(ws(err_token)).many0()
    ).map(|(_, token)| token).parse(input)
}

#[test]
fn test() {
    use std::path::PathBuf;
    let input = r#"let id = fun x -> x
let twice = fun f -> fun x -> f (f x)

let object1 = { x = 42; y = id }
let object2 = { x = 17; y = false }
let pick_an_object = fun b ->
  if b then object1 else object2

let rec produce = fun arg ->
  { head = arg; tail = produce (succ arg) }

let rec consume = fun strm ->
  add strm.head (consume strm.tail)

let codata = produce 42
let res = consume codata"#;
    let path = PathBuf::from("test.txt");
    let ret = lex(Span {
        data: input,
        start_offset: 0,
        end_offset: input.len() as u32,
        path: &path,
    }).unwrap();
    for x in ret.1 {
        println!("{}", x.data.0)
    }
}
