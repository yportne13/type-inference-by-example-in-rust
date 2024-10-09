use core::{maybe, Maybe, Parser, Span};

use lex::{TokenKind, TokenNode};

pub mod core;
pub mod lex;

#[derive(Clone, Copy, Debug)]
pub enum Expect {
    Ident,
    Eq,
    In,
    Then,
    Else,
    Arrow,
    Term,
}

#[derive(Clone, Debug)]
pub enum Term<'a> {
    Lit(Span<'a, i32>),
    Bool(Span<'a, bool>),
    Var(Span<'a, String>),
    //Paren(Box<Term>),
    Lam {
        ident: Span<'a, String>,
        term: Box<Term<'a>>,
    },
    App(Box<Term<'a>>, Box<Term<'a>>),
    Rcd {
        left: Span<'a, ()>,
        data: Vec<(Span<'a, String>, Span<'a, ()>, Term<'a>)>,
        right: Span<'a, ()>,
    },
    Sel(Box<Term<'a>>, Span<'a, String>),
    Ite {
        cond: Box<Term<'a>>,
        rhs1: Box<Term<'a>>,
        rhs2: Box<Term<'a>>,
    },
}

pub enum Type<'a> {
    Top,
    Bot,
    Union {
        lhs: Box<Type<'a>>,
        rhs: Box<Type<'a>>,
    },
    Inter {
        lhs: Box<Type<'a>>,
        rhs: Box<Type<'a>>,
    },
    Fun {
        arg: Box<Type<'a>>,
        ret: Box<Type<'a>>,
    },
    Record {
        fields: Vec<(String, Type<'a>)>
    },
    Recursive {
        uv: TypeVariable<'a>,
        body: Box<Type<'a>>,
    },
    Primitive {
        name: Span<'a, String>,
    },
    Variable(TypeVariable<'a>),
}

pub struct TypeVariable<'a> {
    name_hint: Span<'a, String>,
    hash: Span<'a, i32>,
}

fn kw<'a, 'b: 'a>(p: TokenKind) -> impl Parser<&'a [TokenNode<'b>], Span<'a, ()>> {
    move |input: &'a [TokenNode<'b>]| {
        match input.first() {
            Some(x) if x.data.1 == p => {
                input.get(1..).map(|i| (
                    i,
                    x.map(|_| ())
                ))
            }
            _ => None
        }
    }
}

fn string<'a>(p: TokenKind) -> impl Parser<&'a [TokenNode<'a>], Span<'a, String>> {
    move |input: &'a [TokenNode<'a>]| {
        match input.first() {
            Some(x) if x.data.1 == p => {
                input.get(1..).map(|i| (
                    i,
                    x.map(|s| s.0.to_owned())
                ))
            }
            _ => None
        }
    }
}

fn term<'a>(input: &'a [TokenNode<'a>]) -> Option<(&'a [TokenNode<'a>], Term<'a>)> {
    fun.or(ite).or(apps).parse(input)
}

fn const_or_var<'a>(input: &'a [TokenNode<'a>]) -> Option<(&'a [TokenNode<'a>], Term<'a>)> {
    match input.first() {
        Some(x) if x.data.1 == TokenKind::Num => {
            input.get(1..).map(|i| (
                i,
                Term::Lit(x.map(|d| d.0.parse().unwrap()))
            ))
        },
        Some(x) if x.data.1 == TokenKind::TrueKeyword => {
            input.get(1..).map(|i| (
                i,
                Term::Bool(x.map(|_| true))
            ))
        },
        Some(x) if x.data.1 == TokenKind::FalseKeyword => {
            input.get(1..).map(|i| (
                i,
                Term::Bool(x.map(|_| false))
            ))
        },
        Some(x) if x.data.1 == TokenKind::Ident => {
            input.get(1..).map(|i| (
                i,
                Term::Var(x.map(|d| d.0.to_owned()))
            ))
        },
        _ => None,
    }
}

fn parens<'a>(input: &'a [TokenNode<'a>]) -> Option<(&'a [TokenNode<'a>], Term<'a>)> {
    kw(TokenKind::LParen).with(term).with(kw(TokenKind::RParen))
        .map(|x| x.0.1).parse(input)
}

fn subterm_no_sel<'a>(input: &'a [TokenNode<'a>]) -> Option<(&'a [TokenNode<'a>], Term<'a>)> {
    parens.or(record).or(const_or_var).parse(input)
}

fn subterm<'a>(input: &'a [TokenNode<'a>]) -> Option<(&'a [TokenNode<'a>], Term<'a>)> {
    let (input, lhs) = subterm_no_sel(input)?;
    let (input, rhs) = (
        kw(TokenKind::Dot).with(string(TokenKind::Ident))
            .map(|x| x.1)
    ).many0().parse(input)?;
    Some((
        input,
        rhs.into_iter()
            .fold(lhs, |acc, ident| Term::Sel(Box::new(acc), ident))
    ))
}

fn record<'a>(input: &'a [TokenNode<'a>]) -> Option<(&'a [TokenNode<'a>], Term<'a>)> {
    let (input, l) = kw(TokenKind::LCurly).parse(input)?;
    let (input, data) =
        string(TokenKind::Ident).with(kw(TokenKind::Eq)).with(term)
        .map(|x| (x.0.0, x.0.1, x.1))
        .many0_sep(kw(TokenKind::Semi))
        .parse(input)?;
    let (input, r) = kw(TokenKind::RCurly).parse(input)?;
    Some((input, Term::Rcd {
        left: l,
        data,
        right: r,
    }))
}

fn fun<'a>(input: &'a [TokenNode<'a>]) -> Option<(&'a [TokenNode<'a>], Term<'a>)> {
    let (input, fun) = kw(TokenKind::FunKeyword).parse(input)?;
    let (input, ident) = string(TokenKind::Ident).parse(input)?;
    let (input, arrow) = kw(TokenKind::Arrow).parse(input)?;
    let (input, rhs) = term.parse(input)?;
    Some((input, Term::Lam {
        ident,
        term: Box::new(rhs),
    }))
}

fn ite<'a>(input: &'a [TokenNode<'a>]) -> Option<(&'a [TokenNode<'a>], Term<'a>)> {
    let (input, if_kw) = kw(TokenKind::IfKeyword).parse(input)?;
    let (input, cond) = term.parse(input)?;
    let (input, then_kw) = kw(TokenKind::ThenKeyword).parse(input)?;
    let (input, rhs1) = term.parse(input)?;
    let (input, else_kw) = kw(TokenKind::ElseKeyword).parse(input)?;
    let (input, rhs2) = term.parse(input)?;
    Some((input, Term::Ite {
        cond: Box::new(cond),
        rhs1: Box::new(rhs1),
        rhs2: Box::new(rhs2),
    }))
}

fn apps<'a>(input: &'a [TokenNode<'a>]) -> Option<(&'a [TokenNode<'a>], Term<'a>)> {
    subterm.many1()
        .map(|x|
            x.into_iter()
                .reduce(|a, b| Term::App(Box::new(a), Box::new(b)))
                .unwrap()
        ).parse(input)
}

fn toplvl<'a>(input: &'a [TokenNode<'a>]) -> Option<(&'a [TokenNode<'a>], (
    Span<'a, String>,
    Box<Term<'a>>))> {
    let (input, keyword) = kw(TokenKind::LetKeyword).parse(input)?;
    let (input, name) = string(TokenKind::Ident).parse(input)?;
    let (input, eq) = kw(TokenKind::Eq).parse(input)?;
    let (input, term1) = term.parse(input)?;
    Some((
        input,
        (
            name,
            Box::new(term1),
        )
    ))
}

fn prgm<'a>(input: &'a [TokenNode<'a>]) -> Option<(&'a [TokenNode<'a>], Vec<(Span<'a, String>, Box<Term<'a>>)>)> {
    toplvl.many0().parse(input)
}

#[test]
fn test() {
    use std::path::PathBuf;
    use crate::parser::lex::lex;
    /*let input = r#"let id = fun x -> x
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
let res = consume codata"#;*/
    /*let input = r#"let id = fun x -> x
let ret = id id"#;*/
    //let input = "let apply = fun f -> fun x -> f x";
    let input = r#"let apply = fun f -> fun x -> f x
let id = fun x -> x
let ret = (id apply) id apply Int"#;
    let path = PathBuf::from("test.txt");
    let ret = lex(Span {
        data: input,
        start_offset: 0,
        end_offset: input.len() as u32,
        path: &path,
    }).unwrap();
    let prgm = prgm(&ret.1).unwrap();
    println!("{:?}", prgm.0);
    for x in prgm.1 {
        println!("{:?}", x)
    }
}

/*#[derive(Clone, Copy, Debug)]
pub enum TreeKind {
    TermLit,
    TermVar,
    TermParen,
    Lam,
    App,
    TermRcd,
    Sel,
    Let,
}

pub struct TreeNode<'a> {
    kind: TreeKind,
    nodes: Vec<Child<'a>>,
}

pub enum Child<'a> {
  Token(TokenNode<'a>),
  Tree(TreeNode<'a>),
  Nil,
}

type Error<'a> = Span<'a, TokenKind>;

fn rec<'a, 'b: 'a>(p: TokenKind) -> impl Parser<&'a [TokenNode<'b>], (Child<'b>, Vec<Error<'b>>)> {
    move |input: &'a [TokenNode<'b>]| {
        match input.first() {
            Some(x) if x.data.1 == p => {
                input.get(1..).map(|i| (
                    i,
                    (Child::Token(*x), vec![])
                ))
            }
            Some(x) => {
                Some((
                    input,
                    (Child::Nil, vec![x.map(|_| p)])
                ))
            },
            _ => None
        }
    }
}

fn unrec<'a, 'b: 'a>(p: TokenKind) -> impl Parser<&'a [TokenNode<'b>], (Child<'b>, Vec<Error<'b>>)> {
    move |input: &'a [TokenNode<'b>]| {
        match input.first() {
            Some(x) if x.data.1 == p => {
                input.get(1..).map(|i| (i, (Child::Token(*x), vec![])))
            },
            _ => None,
        }
    }
}

use lex::TokenKind::*;
use TreeKind::*;

macro_rules! parse {
    ($func_name:ident : $return_type:tt = $($base_unit:expr)=>+) => {
        fn $func_name<'a, 'b: 'a>(input: &'a [TokenNode<'b>]) -> Option<(&'a [TokenNode<'b>], (Child<'b>, Vec<Error<'b>>))> {
            // 解析基本单元并生成相应的代码
            parse_impl!(input, $return_type, $($base_unit)=>+)
        }
    };
    ($func_name:ident : $return_type:tt = $($base_unit:expr);+) => {
        fn $func_name<'a, 'b: 'a>(input: &'a [TokenNode<'b>]) -> Option<(&'a [TokenNode<'b>], (Child<'b>, Vec<Error<'b>>))> {
            // 解析基本单元并生成相应的代码
            parse_or_impl!(input, $return_type, $($base_unit);+)
        }
    };
}

macro_rules! parse_impl {
    ($input:tt, $return_type:tt, $head:expr) => {{
        let (input, o) = $head.parse($input)?;
        {
            Some((input, (Child::Tree(TreeNode {
                kind: $return_type,
                nodes: vec![o.0]
            }), o.1)))
        }
    }};
    ($input:tt, $return_type:tt, $head:expr => $($base_unit:expr)=>+) => {{
        let (input, o) = $head.parse($input)?;
        {
            parse_impl!(input, $return_type, $($base_unit)=>+, [o.0] | o.1)
        }
    }};
    ($input:tt, $return_type:tt, $head:expr => $($base_unit:expr)=>+, [$($parsed:tt)*] | $($parsed1:tt)*) => {{
        let (input, o) = $head.parse($input)?;
        {
            parse_impl!(input, $return_type, $($base_unit)=>+, [$($parsed)*, o.0] | $($parsed1)*, o.1)
        }
    }};
    ($input:tt, $return_type:tt, $head:expr, [$($parsed:tt)*] | $($parsed1:tt)*) => {{
        let (input, o) = $head.parse($input)?;
        {
            Some((input, (Child::Tree(TreeNode {
                kind: $return_type,
                nodes: vec![$($parsed)*, o.0]
            }), [$($parsed1)*, o.1].concat())))
        }
    }};
}

macro_rules! parse_or_impl {
    ($input:tt, $return_type:tt, $head:expr) => {{
        let (input, o) = $head.parse($input)?;
        Some((input, (Child::Tree(TreeNode {
            kind: $return_type,
            nodes: vec![o.0]
        }), o.1)))
    }};
    ($input:tt, $return_type:tt, $head:expr ; $($base_unit:expr);+) => {
        if let Some((input, o)) = $head.parse($input) {
            Some((input, (Child::Tree(TreeNode {
                kind: $return_type,
                nodes: vec![o.0]
            }), o.1)))
        } else {
            parse_or_impl!($input, $return_type, $($base_unit);+)
        }
    };
}

fn term<'a, 'b: 'a>(input: &'a [TokenNode<'b>]) -> Option<(&'a [TokenNode<'b>], (Child<'b>, Vec<Error<'b>>))> {
    None
}

parse!(const_: TermLit = unrec(Num));

parse!{variable: TermVar = unrec(Ident) ; unrec(TrueKeyword) ; unrec(FalseKeyword)}

parse!(parens: TermParen = unrec(LParen) => term => rec(RParen));

//subtermNoSel

//subterm

parse!(record_item: RcdItem = unrec(Ident) => rec(Eq) => term);

//parse!(record: TermRcd = unrec(LCurly) =>  => rec(RCurly));
*/