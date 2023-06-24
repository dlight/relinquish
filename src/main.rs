//! An example of using logos with chumsky to parse sexprs
//! Run it with the following command:
//! cargo run --example logos

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;
use std::fmt;

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
    Parens(Vec<Self>),
}

// This function signature looks complicated, but don't fear! We're just saying that this function is generic over
// inputs that:
//     - Can have tokens pulled out of them by-value, by cloning (`ValueInput`)
//     - Gives us access to slices of the original input (`SliceInput`)
//     - Produces tokens of type `Token`, the type we defined above (`Token = Token<'a>`)
//     - Produces spans of type `SimpleSpan`, a built-in span type provided by chumsky (`Span = SimpleSpan`)
// The function then returns a parser that:
//     - Has an input type of type `I`, the one we declared as a type parameter
//     - Produces an `SExpr` as its output
//     - Uses `Rich`, a built-in error type provided by chumsky, for error generation
fn parser<'a, I>() -> impl Parser<'a, I, TokenTree, extra::Err<Rich<'a, Token>>>
where
    I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
    recursive(|sexpr| {
        let atom = select! {
            Token::Float(x) => TokenTree::Token(Token::Float(x)),
            Token::Add => TokenTree::Token(Token::Add),
            Token::Sub => TokenTree::Token(Token::Sub),
            Token::Mul => TokenTree::Token(Token::Mul),
            Token::Div => TokenTree::Token(Token::Div),
        };

        let list = sexpr
            .repeated()
            .collect()
            .map(TokenTree::Parens)
            .delimited_by(just(Token::LParen), just(Token::RParen));

        atom.or(list)
    })
}

impl TokenTree {
    // Recursively evaluate an s-expression
    fn eval(&self) -> Result<f64, String> {
        match self {
            Self::Token(Token::Float(x)) => Ok(*x),
            Self::Token(x) => Err(format!("Cannot evaluate operator {:?}", x)),
            Self::Parens(list) => match &list[..] {
                [Self::Token(Token::Add), tail @ ..] => tail.iter().map(TokenTree::eval).sum(),
                [Self::Token(Token::Mul), tail @ ..] => tail.iter().map(TokenTree::eval).product(),
                [Self::Token(Token::Sub), init, tail @ ..] => {
                    Ok(init.eval()? - tail.iter().map(TokenTree::eval).sum::<Result<f64, _>>()?)
                }
                [Self::Token(Token::Div), init, tail @ ..] => Ok(init.eval()?
                    / tail
                        .iter()
                        .map(TokenTree::eval)
                        .product::<Result<f64, _>>()?),
                _ => Err("Cannot evaluate list".to_owned()),
            },
        }
    }
}

const SRC: &str = r"
    (-
        (* (+ 4 7.3) 7)
        (/ 5 3)
    )
";

fn main() {
    println!("{}", SRC);

    // Create a logos lexer over the source code
    let token_iter = Token::lexer(SRC)
        .spanned()
        // Convert logos errors into tokens. We want parsing to be recoverable and not fail at the lexing stage, so
        // we have a dedicated `Token::Error` variant that represents a token error that was previously encountered
        .map(|(tok, span)| match tok {
            // Turn the `Range<usize>` spans logos gives us into chumsky's `SimpleSpan` via `Into`, because it's easier
            // to work with
            Ok(tok) => (tok, span.into()),
            Err(()) => (Token::Error, span.into()),
        });

    // Turn the token iterator into a stream that chumsky can use for things like backtracking
    let token_stream = Stream::from_iter(token_iter)
        // Tell chumsky to split the (Token, SimpleSpan) stream into its parts so that it can handle the spans for us
        // This involves giving chumsky an 'end of input' span: we just use a zero-width span at the end of the string
        .spanned((SRC.len()..SRC.len()).into());

    // Parse the token stream with our chumsky parser
    match parser().parse(token_stream).into_result() {
        // If parsing was successful, attempt to evaluate the s-expression
        Ok(sexpr) => match dbg!(sexpr).eval() {
            Ok(out) => println!("Result = {}", out),
            Err(err) => println!("Runtime error: {}", err),
        },
        // If parsing was unsuccessful, generate a nice user-friendly diagnostic with ariadne. You could also use
        // codespan, or whatever other diagnostic library you care about. You could even just display-print the errors
        // with Rust's built-in `Display` trait, but it's a little crude
        Err(errs) => {
            for err in errs {
                Report::build(ReportKind::Error, (), err.span().start)
                    .with_code(3)
                    .with_message(err.to_string())
                    .with_label(
                        Label::new(err.span().into_range())
                            .with_message(err.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(SRC))
                    .unwrap();
            }
        }
    }
}
