use std::{borrow::Cow, cell::RefCell};

use comrak::{
    nodes::{Ast, AstNode, LineColumn, ListType, NodeHeading, NodeLink, NodeList, NodeValue},
    Arena,
};
use html5ever::{
    tokenizer::{TagKind, Token, TokenSink, TokenSinkResult},
    Attribute, LocalName,
};

use crate::attributes::AttributeList;
use crate::error::Error;

pub(crate) struct Sink<'a> {
    strict: bool,
    arena: &'a Arena<AstNode<'a>>,
    error: Option<Error>,
    node_stack: Vec<&'a AstNode<'a>>,
    elem_stack: Vec<LocalName>,
}

impl<'a> Sink<'a> {
    #[inline]
    fn cur(&mut self) -> Result<&mut &'a AstNode<'a>, Error> {
        self.node_stack
            .last_mut()
            .ok_or_else(move || "stack is empty".into())
    }
}

impl<'a> Sink<'a> {
    pub fn new(arena: &'a Arena<AstNode<'a>>, strict: bool) -> Self {
        let root = arena.alloc(AstNode::new(RefCell::new(Ast::new(
            NodeValue::Document,
            LineColumn { line: 0, column: 0 },
        ))));

        Self {
            error: None,
            strict,
            arena,
            node_stack: vec![root],
            elem_stack: vec![],
        }
    }

    pub fn finish(mut self) -> Result<&'a AstNode<'a>, Error> {
        if let Some(e) = self.error {
            return Err(e);
        }

        let root = self.node_stack.pop().ok_or("stack is empty")?;

        if !self.node_stack.is_empty() {
            return Err(Error::ParseError(Cow::Borrowed(
                "stack is not empty, probably due to invalid HTML",
            )));
        }

        Ok(root)
    }
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
                                self.node_stack.push(node);
                                self.elem_stack.push(tag.name);
                            }
                        }
                    }
                    TagKind::EndTag if !valid_elem(&tag.name) || is_self_closing(&tag.name) => {}
                    TagKind::EndTag => {
                        if self.elem_stack.pop().as_ref() == Some(&tag.name) {
                            let node = self.node_stack.pop().unwrap();
                            let parent = self.cur()?;

                            parent.append(node);
                        } else if self.strict {
                            return Err(Error::ParseError(Cow::Owned(format!(
                                "unexpected closing tag: </{}>",
                                tag.name
                            ))));
                        }
                    }
                },
                Token::CommentToken(_) => {}
                Token::CharacterTokens(s) => {
                    self.cur()?.append(
                        self.arena
                            .alloc(new_node(NodeValue::Text(s.to_string()), 0)),
                    );
                }
                Token::NullCharacterToken => {}
                Token::EOFToken => {}
                Token::ParseError(err) => {
                    if self.strict {
                        return Err(Error::ParseError(err));
                    }
                }
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

#[inline]
pub fn new_node<'a>(value: NodeValue, line: usize) -> AstNode<'a> {
    AstNode::new(RefCell::new(Ast::new(
        value,
        LineColumn { line, column: 0 },
    )))
}

#[inline]
fn heading<'a>(level: u8, line: usize) -> AstNode<'a> {
    new_node(
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
        "a" => arena.alloc(new_node(
            NodeValue::Link(NodeLink {
                url: attrs.get_or_default("href"),
                title: attrs.get_or_default("title"),
            }),
            line,
        )),
        "br" => arena.alloc(new_node(NodeValue::LineBreak, line)),
        "h1" => arena.alloc(heading(1, line)),
        "h2" => arena.alloc(heading(2, line)),
        "h3" => arena.alloc(heading(3, line)),
        "h4" => arena.alloc(heading(4, line)),
        "h5" => arena.alloc(heading(5, line)),
        "h6" => arena.alloc(heading(6, line)),
        "p" => arena.alloc(new_node(NodeValue::Paragraph, line)),
        "ul" => arena.alloc(new_node(
            NodeValue::List(NodeList {
                list_type: ListType::Bullet,
                bullet_char: b'-',
                ..NodeList::default()
            }),
            line,
        )),
        "ol" => arena.alloc(new_node(
            NodeValue::List(NodeList {
                list_type: ListType::Ordered,
                start: 1,
                ..NodeList::default()
            }),
            line,
        )),
        "li" => arena.alloc(new_node(NodeValue::Item(NodeList::default()), line)),
        "b" | "strong" => arena.alloc(new_node(NodeValue::Strong, line)),
        "i" | "em" => arena.alloc(new_node(NodeValue::Emph, line)),
        "del" => arena.alloc(new_node(NodeValue::Strikethrough, line)),
        "img" => {
            let image = arena.alloc(new_node(
                NodeValue::Image(NodeLink {
                    url: attrs.get_or_default("src"),
                    title: attrs.get_or_default("title"),
                }),
                line,
            ));

            if let Some(alt) = AttributeList::get(&attrs, "alt") {
                let text_node = arena.alloc(new_node(NodeValue::Text(alt.to_string()), line));
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
            | "del"
    )
}
