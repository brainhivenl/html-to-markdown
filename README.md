# HTML to CommonMark

Convert HTML to markdown (CommonMark). Uses [html5ever](https://crates.io/crates/html5ever) for parsing HTML and [comrak](https://crates.io/crates/comrak) for generating markdown output. It generates a comrak AST based on the HTML input and then converts it to markdown using `comrak::format_commonmark`.

## Usage

```rust
fn main() {
    let html = "<h1>Hello World</h1>".to_string();
    let markdown = html_to_markdown::render(html)
        .expect("failed to convert HTML to markdown");

    println!("{}", markdown); // # Hello World
}
```

## Features

The following HTML elements are supported (other elements will be stripped):

- `h1`
- `h2`
- `h3`
- `h4`
- `h5`
- `h6`
- `p`
- `ul`
- `ol`
- `li`
- `em`/`i`
- `strong`/`b`
- `a`
- `img`
