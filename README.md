[![Workflow Status](https://github.com/arviceblot/bbcode-tagger/workflows/main/badge.svg)](https://github.com/arviceblot/bbcode-tagger/actions?query=workflow%3A%22main%22)
[![Coverage Status](https://codecov.io/gh/arviceblot/bbcode-tagger/branch/master/graph/badge.svg)](https://codecov.io/gh/arviceblot/bbcode-tagger)

# bbcode-tagger

A tree parser and tagger for BBCode formatted text.

Rather than being a BBCode to HTML converter, this project aims to do something less sane by parsing it into a tree structure.
The slightly sane reasoning for this is to build a generic structure that doesn't depend on a specific output format.
So an HTML formatter could be written for the tree structure rather than the string data itself.
Or the original purpose for this library, to display BBCode formatted text in [egui](https://docs.rs/egui/latest/egui/) for [eso-addon-manager](https://github.com/arviceblot/eso-addons).

## Usage
```rust
use bbcode_tagger::BBCode;

let parser = BBCode::default();
let tree = parser.parse(r"This is some [B]BBCODE![\B]");

println!("{}", tree);
```

The example code should produce an output like:

```shell
Nodes: 2
ID    : 0
Text  : This is some
Tag   : None
Value : None
Parent: None

  ID    : 1
  Text  : BBCODE!
  Tag   : Bold
  Value : None
  Parent: Some(0)
```

License: MIT
