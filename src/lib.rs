use std::{
    borrow::Cow,
    cell::RefCell,
    fmt::{self, Display},
    io::{self, Cursor},
    string::FromUtf8Error,
};

use comrak::{
    nodes::{Ast, AstNode, LineColumn, NodeHeading, NodeValue},
    Arena, ComrakOptions,
};
use html5ever::{
    tendril::StrTendril,
    tokenizer::{
        BufferQueue, TagKind, Token, TokenSink, TokenSinkResult, Tokenizer, TokenizerOpts,
    },
};

#[derive(Debug)]
pub enum Error {
    IOError(io::Error),
    Utf8Error(FromUtf8Error),
    Other(Cow<'static, str>),
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::IOError(e) => e.fmt(f),
            Error::Utf8Error(e) => e.fmt(f),
            Error::Other(e) => e.fmt(f),
        }
    }
}

impl From<Cow<'static, str>> for Error {
    fn from(s: Cow<'static, str>) -> Self {
        Self::Other(s)
    }
}

impl From<&'static str> for Error {
    fn from(s: &'static str) -> Self {
        Self::Other(s.into())
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Self::IOError(err)
    }
}

impl From<FromUtf8Error> for Error {
    fn from(err: FromUtf8Error) -> Self {
        Self::Utf8Error(err)
    }
}

pub struct Sink<'a> {
    error: Option<Error>,
    arena: &'a Arena<AstNode<'a>>,
    stack: Vec<&'a AstNode<'a>>,
}

impl<'a> Sink<'a> {
    #[inline]
    fn cur(&mut self) -> Result<&mut &'a AstNode<'a>, Error> {
        self.stack
            .last_mut()
            .ok_or_else(move || "stack is empty".into())
    }
}

impl<'a> Sink<'a> {
    pub fn new(arena: &'a Arena<AstNode<'a>>) -> Self {
        let root = arena.alloc(AstNode::new(RefCell::new(Ast::new(
            NodeValue::Document,
            LineColumn { line: 0, column: 0 },
        ))));

        Self {
            error: None,
            arena,
            stack: vec![root],
        }
    }
}

#[inline]
fn node<'a>(value: NodeValue, line: usize) -> AstNode<'a> {
    AstNode::new(RefCell::new(Ast::new(
        value,
        LineColumn { line, column: 0 },
    )))
}

#[inline]
fn heading<'a>(level: u8, line: usize) -> AstNode<'a> {
    node(
        NodeValue::Heading(NodeHeading {
            level,
            setext: false,
        }),
        line,
    )
}

fn create_node<'a>(name: &str, line: usize) -> Option<AstNode<'a>> {
    match name {
        "h1" => Some(heading(1, line)),
        "h2" => Some(heading(2, line)),
        "h3" => Some(heading(3, line)),
        "h4" => Some(heading(4, line)),
        "h5" => Some(heading(5, line)),
        "h6" => Some(heading(6, line)),
        "p" => Some(node(NodeValue::Paragraph, line)),
        _ => None,
    }
}

impl<'a> TokenSink for Sink<'a> {
    type Handle = ();

    fn process_token(&mut self, token: Token, line: u64) -> TokenSinkResult<Self::Handle> {
        let f = || {
            match token {
                Token::DoctypeToken(_) => todo!(),
                Token::TagToken(tag) => match tag.kind {
                    TagKind::StartTag => {
                        if let Some(node) = create_node(tag.name.as_ref(), line as usize) {
                            self.stack.push(self.arena.alloc(node));
                        }
                    }
                    TagKind::EndTag => {
                        let node = self.stack.pop().unwrap();
                        let parent = self.cur()?;

                        parent.append(node);
                    }
                },
                Token::CommentToken(_) => todo!(),
                Token::CharacterTokens(s) => {
                    let node = self.stack.last_mut().unwrap();

                    node.append(self.arena.alloc(AstNode::new(RefCell::new(Ast::new(
                        NodeValue::Text(s.to_string()),
                        LineColumn { line: 0, column: 0 },
                    )))));
                }
                Token::NullCharacterToken => todo!(),
                Token::EOFToken => {}
                Token::ParseError(_) => todo!(),
            }

            Ok::<_, Error>(())
        };

        if let Err(e) = f() {
            self.error = Some(e);
            self.end();
        }

        TokenSinkResult::Continue
    }
}

pub fn render(input: String) -> Result<String, Error> {
    let chunk = StrTendril::from(input);
    let mut input = BufferQueue::new();
    input.push_back(chunk.try_reinterpret().map_err(|_| "buffer invalid")?);

    let arena = Arena::new();
    let sink = Sink::new(&arena);

    let mut tok = Tokenizer::new(sink, TokenizerOpts::default());
    let _ = tok.feed(&mut input);
    tok.end();

    let root = tok.sink.stack.pop().ok_or("stack is empty")?;
    assert!(tok.sink.stack.is_empty());

    if let Some(e) = tok.sink.error {
        return Err(e);
    }

    let mut writer = Cursor::new(vec![]);
    comrak::format_commonmark(root, &ComrakOptions::default(), &mut writer)?;

    Ok(String::from_utf8(writer.into_inner())?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_render() {
        let input = String::from("<h1>hello world</h1>");
        let output = render(input);

        assert_eq!(output.as_deref().ok(), Some("# hello world\n"));
    }
}
