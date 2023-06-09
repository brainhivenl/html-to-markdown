//! # HTML to CommonMark
//!
//! Convert HTML to markdown (CommonMark). Uses [html5ever](https://crates.io/crates/html5ever) for parsing HTML and [comrak](https://crates.io/crates/comrak) for generating markdown output. It generates a comrak AST based on the HTML input and then converts it to markdown using `comrak::format_commonmark`.
//!
//! ## Usage
//!
//! ```rust
//! fn example() {
//!     let html = "<h1>Hello World</h1>".to_string();
//!     let markdown = html_to_markdown::render(html)
//!         .expect("failed to convert HTML to markdown");
//!
//!     println!("{}", markdown); // # Hello World
//! }
//! ```
//!
//! ## Features
//!
//! The following HTML elements are supported (other elements will be stripped):
//!
//! - `h1`
//! - `h2`
//! - `h3`
//! - `h4`
//! - `h5`
//! - `h6`
//! - `p`
//! - `ul`
//! - `ol`
//! - `li`
//! - `em`/`i`
//! - `strong`/`b`
//! - `a`
//! - `img`

use std::{cell::RefCell, io::Cursor};

use comrak::{
    nodes::{Ast, AstNode, LineColumn, ListType, NodeHeading, NodeLink, NodeList, NodeValue},
    Arena, ComrakOptions,
};
use html5ever::{
    tendril::StrTendril,
    tokenizer::{
        BufferQueue, TagKind, Token, TokenSink, TokenSinkResult, Tokenizer, TokenizerOpts,
    },
    Attribute,
};

mod attributes;
mod error;

use attributes::AttributeList;
pub use error::Error;

struct Sink<'a> {
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

#[inline]
fn create_node<'a>(
    arena: &'a Arena<AstNode<'a>>,
    name: &str,
    attrs: &[Attribute],
    line: usize,
) -> Option<&'a AstNode<'a>> {
    Some(match name {
        "a" => arena.alloc(node(
            NodeValue::Link(NodeLink {
                url: attrs.get_or_default("href"),
                title: attrs.get_or_default("title"),
            }),
            1,
        )),
        "h1" => arena.alloc(heading(1, line)),
        "h2" => arena.alloc(heading(2, line)),
        "h3" => arena.alloc(heading(3, line)),
        "h4" => arena.alloc(heading(4, line)),
        "h5" => arena.alloc(heading(5, line)),
        "h6" => arena.alloc(heading(6, line)),
        "p" => arena.alloc(node(NodeValue::Paragraph, line)),
        "ul" => arena.alloc(node(
            NodeValue::List(NodeList {
                list_type: ListType::Bullet,
                bullet_char: b'-',
                ..NodeList::default()
            }),
            line,
        )),
        "ol" => arena.alloc(node(
            NodeValue::List(NodeList {
                list_type: ListType::Ordered,
                start: 1,
                ..NodeList::default()
            }),
            line,
        )),
        "li" => arena.alloc(node(NodeValue::Item(NodeList::default()), line)),
        "b" | "strong" => arena.alloc(node(NodeValue::Strong, line)),
        "i" | "em" => arena.alloc(node(NodeValue::Emph, line)),
        "img" => {
            let image = arena.alloc(node(
                NodeValue::Image(NodeLink {
                    url: attrs.get_or_default("src"),
                    title: attrs.get_or_default("title"),
                }),
                line,
            ));

            if let Some(alt) = AttributeList::get(&attrs, "alt") {
                let text_node = arena.alloc(node(NodeValue::Text(alt.to_string()), line));
                image.append(text_node);
            }

            image
        }
        _ => return None,
    })
}

#[inline]
fn is_self_closing(name: &str) -> bool {
    matches!(
        name,
        "area"
            | "base"
            | "br"
            | "col"
            | "embed"
            | "hr"
            | "img"
            | "input"
            | "keygen"
            | "link"
            | "meta"
            | "param"
            | "source"
            | "track"
            | "wbr"
    )
}

#[inline]
fn valid_elem(name: &str) -> bool {
    matches!(
        name,
        "h1" | "h2"
            | "h3"
            | "h4"
            | "h5"
            | "h6"
            | "p"
            | "ul"
            | "li"
            | "ol"
            | "a"
            | "b"
            | "strong"
            | "i"
            | "em"
            | "img"
    )
}

impl<'a> TokenSink for Sink<'a> {
    type Handle = ();

    fn process_token(&mut self, token: Token, line: u64) -> TokenSinkResult<Self::Handle> {
        let f = || {
            match token {
                Token::DoctypeToken(_) => {}
                Token::TagToken(tag) => match tag.kind {
                    TagKind::StartTag => {
                        if let Some(node) =
                            create_node(self.arena, &tag.name, &tag.attrs, line as usize)
                        {
                            if tag.self_closing || is_self_closing(&tag.name) {
                                let parent = self.cur()?;
                                parent.append(node);
                            } else {
                                self.stack.push(node);
                            }
                        }
                    }
                    TagKind::EndTag if !valid_elem(&tag.name) || is_self_closing(&tag.name) => {}
                    TagKind::EndTag => {
                        let node = self.stack.pop().unwrap();
                        let parent = self.cur()?;

                        parent.append(node);
                    }
                },
                Token::CommentToken(_) => {}
                Token::CharacterTokens(s) => {
                    self.cur()?
                        .append(self.arena.alloc(node(NodeValue::Text(s.to_string()), 0)));
                }
                Token::NullCharacterToken => {}
                Token::EOFToken => {}
                Token::ParseError(err) => return Err(Error::ParseError(err)),
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

/// Parse an HTML document into a [comrak](https://crates.io/crates/comrak) AST
pub fn parse_document<'a>(
    arena: &'a Arena<AstNode<'a>>,
    input: String,
) -> Result<&'a AstNode<'a>, Error> {
    let chunk = StrTendril::from(input);
    let mut input = BufferQueue::new();
    input.push_back(chunk.try_reinterpret().map_err(|_| "buffer invalid")?);

    let mut tok = Tokenizer::new(Sink::new(arena), TokenizerOpts::default());
    let _ = tok.feed(&mut input);
    tok.end();

    let root = tok.sink.stack.pop().ok_or("stack is empty")?;
    assert!(tok.sink.stack.is_empty());

    if let Some(e) = tok.sink.error {
        return Err(e);
    }

    Ok(root)
}

/// Convert a HTML document into markdown (CommonMark)
pub fn render(input: String) -> Result<String, Error> {
    let arena = Arena::new();
    let root = parse_document(&arena, input)?;

    let mut writer = Cursor::new(vec![]);
    comrak::format_commonmark(root, &ComrakOptions::default(), &mut writer)?;

    Ok(String::from_utf8(writer.into_inner())?)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_render(input: impl Into<String>, expected: impl AsRef<str>) {
        let output = render(input.into()).unwrap();
        assert_eq!(output.as_str(), expected.as_ref());
    }

    #[test]
    fn test_headings() {
        let levels = 1..6;

        for level in levels {
            assert_render(
                format!("<h{level}>hello world</h{level}>"),
                format!("{} hello world\n", "#".repeat(level)),
            );
        }
    }

    #[test]
    fn test_paragraph() {
        assert_render("<p>hello world</p>", "hello world\n");
    }

    #[test]
    fn test_wrapped() {
        assert_render("<div><p>hello world</p></div>", "hello world\n");
    }

    #[test]
    fn test_unordered_list() {
        assert_render(
            "<ul><li>first item</li><li>second item</li></ul>",
            "- first item\n- second item\n",
        );
    }

    #[test]
    fn test_ordered_list() {
        assert_render(
            "<ol><li>first item</li><li>second item</li></ol>",
            "1.  first item\n2.  second item\n",
        );
    }

    #[test]
    fn test_link() {
        assert_render(
            "<a href=\"https://example.com\">example</a>",
            "[example](https://example.com)\n",
        );
    }

    #[test]
    fn test_strong() {
        assert_render("<strong>hello world</strong>", "**hello world**\n");
        assert_render("<b>hello world</b>", "**hello world**\n");
    }

    #[test]
    fn test_emphasis() {
        assert_render("<em>hello world</em>", "*hello world*\n");
        assert_render("<i>hello world</i>", "*hello world*\n");
    }

    #[test]
    fn test_img() {
        assert_render("<img src=\"test.jpg\" />", "![](test.jpg)\n");
        assert_render(
            "<img src=\"test.jpg\" title=\"this is a test\" />",
            "![](test.jpg \"this is a test\")\n",
        );
        assert_render(
            "<img src=\"test.jpg\" title=\"this is a test\" alt=\"alt test\" />",
            "![alt test](test.jpg \"this is a test\")\n",
        );
    }

    #[test]
    fn test_img_not_self_closing_issue() {
        assert_render("<img src=\"test.jpg\">", "![](test.jpg)\n");
        assert_render("<img src=\"test.jpg\"></img>", "![](test.jpg)\n");
    }
}
