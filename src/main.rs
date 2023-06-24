use std::fmt;

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;
use makeit::{Buildable, Builder};

type Span = SimpleSpan<usize>;
type Spanned<T> = (T, Span);

type ParserError<'tokens> = extra::Err<Rich<'tokens, Token>>;

#[derive(Builder)]
struct EvalError {
    span: Span,
    msg: String,
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r\n]+")]
enum Token {
    Error,

    #[regex(r"[+-]?([0-9]*[.])?[0-9]+", |lex| lex.slice().parse().ok())]
    Float(f64),

    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Float(s) => write!(f, "{}", s),
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::Error => write!(f, "<error>"),
        }
    }
}

#[derive(Debug)]
enum TokenTree {
    Token(Token),
    Parens(Vec<Spanned<Self>>),
}

fn parser<'tokens, Input>() -> impl Parser<'tokens, Input, Spanned<TokenTree>, ParserError<'tokens>>
where
    Input: ValueInput<'tokens, Token = Token, Span = SimpleSpan>,
{
    recursive(|token_tree| {
        let atom = select! {
            Token::Float(x) => TokenTree::Token(Token::Float(x)),
            Token::Add => TokenTree::Token(Token::Add),
            Token::Sub => TokenTree::Token(Token::Sub),
            Token::Mul => TokenTree::Token(Token::Mul),
            Token::Div => TokenTree::Token(Token::Div),
        };

        let list = token_tree
            .repeated()
            .collect()
            .map(TokenTree::Parens)
            .delimited_by(just(Token::LParen), just(Token::RParen));

        atom.or(list).map_with_span(|expr, span| (expr, span))
    })
}

impl TokenTree {
    fn eval(expr: &Spanned<Self>) -> Result<f64, EvalError> {
        let error = EvalError::builder().set_span(expr.1);

        match &expr.0 {
            Self::Token(Token::Float(x)) => Ok(*x),
            Self::Token(x) => Err(error
                .set_msg(format!("Cannot evaluate operator {:?}", x))
                .build()),
            Self::Parens(list) => match &list[..] {
                [(Self::Token(Token::Add), _), tail @ ..] => tail.iter().map(TokenTree::eval).sum(),
                [(Self::Token(Token::Mul), _), tail @ ..] => {
                    tail.iter().map(TokenTree::eval).product()
                }
                [(Self::Token(Token::Sub), _), init, tail @ ..] => Ok(TokenTree::eval(init)?
                    - tail.iter().map(TokenTree::eval).sum::<Result<f64, _>>()?),
                [(Self::Token(Token::Div), _), init, tail @ ..] => Ok(TokenTree::eval(init)?
                    / tail
                        .iter()
                        .map(TokenTree::eval)
                        .product::<Result<f64, _>>()?),
                _ => Err(error.set_msg("Cannot evaluate list".to_owned()).build()),
            },
        }
    }
}

fn run_program(src: &str) {
    println!("input program {}", src);

    let mut errors = Vec::new();

    let token_iter = Token::lexer(src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::Error, span.into()),
    });

    let token_stream = Stream::from_iter(token_iter).spanned((src.len()..src.len()).into());

    let (parser_output, parser_errors) = parser().parse(token_stream).into_output_errors();

    errors.extend(parser_errors.into_iter());

    if let Some(token_tree) = parser_output {
        println!("parsed program {:#?}", token_tree.0);

        if errors.len() == 0 {
            match TokenTree::eval(&token_tree) {
                Ok(val) => println!("Return value: {}", val),
                Err(e) => errors.push(Rich::custom(e.span, e.msg)),
            }
        }
    }

    for err in errors {
        Report::build(ReportKind::Error, (), err.span().start)
            .with_message(err.to_string())
            .with_label(
                Label::new(err.span().into_range())
                    .with_message(err.reason().to_string())
                    .with_color(Color::Red),
            )
            .with_labels(err.contexts().map(|(label, span)| {
                Label::new(span.into_range())
                    .with_message(format!("while parsing this {}", label))
                    .with_color(Color::Yellow)
            }))
            .finish()
            .eprint(Source::from(src))
            .unwrap();
    }
}

const SOURCES: [&str; 3] = [
    r"
    (-
        (* (+ 4 7.3) 7)
        (/ 5 3)
    )
",
    r"
    (-
        (* (+ 4 7.3) 7)
        (/ 5 3
    )
",
    r"
    (-
        (* (+ 4 7A3) 7)
        (/ 5 3)
    )
",
];

fn main() {
    for (i, src) in SOURCES.iter().enumerate() {
        run_program(src);

        if i < SOURCES.len() - 1 {
            println!("\n--\n");
        }
    }
}
