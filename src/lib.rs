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

use std::io::Cursor;

use comrak::{nodes::AstNode, Arena, ComrakOptions};
use html5ever::{
    tendril::StrTendril,
    tokenizer::{BufferQueue, Tokenizer, TokenizerOpts},
};

mod attributes;
mod error;
mod sink;

pub use error::Error;
use sink::Sink;

/// Options for rendering
pub struct RenderOptions {
    /// When disabled, ParseErrors will be ignored
    pub strict: bool,
}

impl Default for RenderOptions {
    fn default() -> Self {
        Self { strict: true }
    }
}

/// Parse an HTML document into a [comrak](https://crates.io/crates/comrak) AST
pub fn parse_document<'a>(
    arena: &'a Arena<AstNode<'a>>,
    input: String,
) -> Result<&'a AstNode<'a>, Error> {
    parse_document_with_options(RenderOptions::default(), arena, input)
}

/// Parse an HTML document into a [comrak](https://crates.io/crates/comrak) AST
pub fn parse_document_with_options<'a>(
    opts: RenderOptions,
    arena: &'a Arena<AstNode<'a>>,
    input: String,
) -> Result<&'a AstNode<'a>, Error> {
    let chunk = StrTendril::from(input);
    let mut input = BufferQueue::new();
    input.push_back(chunk.try_reinterpret().map_err(|_| "buffer invalid")?);

    let mut tok = Tokenizer::new(Sink::new(arena, opts.strict), TokenizerOpts::default());
    let _ = tok.feed(&mut input);
    tok.end();

    tok.sink.finish()
}

/// Convert a HTML document into markdown (CommonMark)
pub fn render(input: String) -> Result<String, Error> {
    render_with_options(RenderOptions::default(), input)
}

/// Convert a HTML document into markdown (CommonMark) with options
pub fn render_with_options(opts: RenderOptions, input: String) -> Result<String, Error> {
    let arena = Arena::new();
    let root = parse_document_with_options(opts, &arena, input)?;

    let mut writer = Cursor::new(vec![]);
    comrak::format_commonmark(root, &ComrakOptions::default(), &mut writer)?;

    Ok(String::from_utf8(writer.into_inner())?)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_render(opts: RenderOptions, input: impl Into<String>, expected: impl AsRef<str>) {
        let output = render_with_options(opts, input.into()).unwrap();
        assert_eq!(output.as_str(), expected.as_ref());
    }

    #[test]
    fn test_headings() {
        let levels = 1..6;

        for level in levels {
            assert_render(
                RenderOptions::default(),
                format!("<h{level}>hello world</h{level}>"),
                format!("{} hello world\n", "#".repeat(level)),
            );
        }
    }

    #[test]
    fn test_paragraph() {
        assert_render(
            RenderOptions::default(),
            "<p>hello world</p>",
            "hello world\n",
        );
    }

    #[test]
    fn test_wrapped() {
        assert_render(
            RenderOptions::default(),
            "<div><p>hello world</p></div>",
            "hello world\n",
        );
    }

    #[test]
    fn test_unordered_list() {
        assert_render(
            RenderOptions::default(),
            "<ul><li>first item</li><li>second item</li></ul>",
            "- first item\n- second item\n",
        );
    }

    #[test]
    fn test_ordered_list() {
        assert_render(
            RenderOptions::default(),
            "<ol><li>first item</li><li>second item</li></ol>",
            "1.  first item\n2.  second item\n",
        );
    }

    #[test]
    fn test_link() {
        assert_render(
            RenderOptions::default(),
            "<a href=\"https://example.com\">example</a>",
            "[example](https://example.com)\n",
        );
    }

    #[test]
    fn test_strong() {
        assert_render(
            RenderOptions::default(),
            "<strong>hello world</strong>",
            "**hello world**\n",
        );
        assert_render(
            RenderOptions::default(),
            "<b>hello world</b>",
            "**hello world**\n",
        );
    }

    #[test]
    fn test_emphasis() {
        assert_render(
            RenderOptions::default(),
            "<em>hello world</em>",
            "*hello world*\n",
        );
        assert_render(
            RenderOptions::default(),
            "<i>hello world</i>",
            "*hello world*\n",
        );
    }

    #[test]
    fn test_img() {
        assert_render(
            RenderOptions::default(),
            "<img src=\"test.jpg\" />",
            "![](test.jpg)\n",
        );
        assert_render(
            RenderOptions::default(),
            "<img src=\"test.jpg\" title=\"this is a test\" />",
            "![](test.jpg \"this is a test\")\n",
        );
        assert_render(
            RenderOptions::default(),
            "<img src=\"test.jpg\" title=\"this is a test\" alt=\"alt test\" />",
            "![alt test](test.jpg \"this is a test\")\n",
        );
    }

    #[test]
    fn test_img_not_self_closing_issue() {
        assert_render(
            RenderOptions::default(),
            "<img src=\"test.jpg\">",
            "![](test.jpg)\n",
        );
        assert_render(
            RenderOptions::default(),
            "<img src=\"test.jpg\"></img>",
            "![](test.jpg)\n",
        );
    }

    #[test]
    fn test_parse_invalid_html() {
        assert_render(
            RenderOptions { strict: false },
            "<img src=\"test.jpg\" '=\"\">",
            "![](test.jpg)\n",
        );

        assert_render(
            RenderOptions { strict: false },
            "<strong>hello</strong></a>",
            "**hello**\n",
        );
    }
}
