//! A tree parser and tagger for BBCode formatted text.
//!
//! Rather than being a BBCode to HTML converter, this project aims to do something less sane by parsing it into a tree structure.
//! The slightly sane reasoning for this is to build a generic structure that doesn't depend on a specific output format.
//! So an HTML formatter could be written for the tree structure rather than the string data itself.
//! Or the original purpose for this library, to display BBCode formatted text in [egui](https://docs.rs/egui/latest/egui/) for [eso-addon-manager](https://github.com/arviceblot/eso-addons).
//!
//! # Usage
//! ```rust
//! use bbcode_tagger::BBCode;
//!
//! let parser = BBCode::default();
//! let tree = parser.parse(r"This is some [B]BBCODE![\B]");
//!
//! println!("{}", tree);
//! ```
//!
//! The example code should produce an output like:
//!
//! ```shell
//! Nodes: 2
//! ID    : 0
//! Text  : This is some
//! Tag   : None
//! Value : None
//! Parent: None
//!
//!   ID    : 1
//!   Text  : BBCODE!
//!   Tag   : Bold
//!   Value : None
//!   Parent: Some(0)
//! ```
#![warn(
    missing_docs,
    rust_2018_idioms,
    missing_debug_implementations,
    rustdoc::broken_intra_doc_links
)]
use core::fmt;
use regex::Regex;
use std::collections::HashMap;
use std::fmt::Display;

static RE_OPEN_TAG: &str = r#"^\[(?P<tag>[^/\]]+?\S*?)((?:[ \t]+\S+?)?="?(?P<val>[^\]\n]*?))?"?\]"#;
static RE_CLOSE_TAG: &str = r#"^\[/(?P<tag>[^/\]]+?\S*?)\]"#;
static RE_NEWLINE: &str = r#"^\r?\n"#;

/// BBCode tag type enum
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BBTag {
    /// No tag
    None,
    Bold,
    Italic,
    Underline,
    Strikethrough,
    FontSize,
    FontColor,
    Center,
    Left,
    Right,
    Superscript,
    Subscript,
    Blur,
    Quote,
    Spoiler,
    Link,
    Email,
    Image,
    ListOrdered,
    ListUnordered,
    ListItem,
    Code,
    Preformatted,
    Table,
    TableHeading,
    TableRow,
    TableCell,
    YouTube,
    /// Some other unhandled tag
    Unknown,
    // TODO: Handle some extra codes
    // - indent
}
impl From<&str> for BBTag {
    fn from(value: &str) -> BBTag {
        let binding = value.trim().to_lowercase();
        let trim_tag = binding.as_str();
        match trim_tag {
            "b" => BBTag::Bold,
            "i" => BBTag::Italic,
            "u" => BBTag::Underline,
            "s" => BBTag::Strikethrough,
            "size" => BBTag::FontSize,
            "color" => BBTag::FontColor,
            "center" => BBTag::Center,
            "left" => BBTag::Left,
            "right" => BBTag::Right,
            "sup" => BBTag::Superscript,
            "sub" => BBTag::Subscript,
            "blur" => BBTag::Blur,
            "email" => BBTag::Email,
            "quote" => BBTag::Quote,
            "spoiler" => BBTag::Spoiler,
            "url" => BBTag::Link,
            "img" => BBTag::Image,
            "ul" | "list" => BBTag::ListUnordered,
            "ol" => BBTag::ListOrdered,
            "li" | "*" => BBTag::ListItem,
            "code" | "highlight" => BBTag::Code,
            "pre" => BBTag::Preformatted,
            "table" => BBTag::Table,
            "tr" => BBTag::TableRow,
            "th" => BBTag::TableHeading,
            "td" => BBTag::TableCell,
            "youtube" => BBTag::YouTube,
            "" => BBTag::None,
            &_ => BBTag::Unknown,
        }
    }
}

/// Node in the BBTag Tree with associated data
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BBNode {
    /// Unformatted string text
    pub text: String,
    /// Associated tag
    pub tag: BBTag,
    /// Possible value related to tag (i.e. "4" in [SIZE=4])
    pub value: Option<String>,
    /// Parent node. Only root (id = 0) node should not have parent
    pub parent: Option<i32>,
    /// Child nodes
    pub children: Vec<i32>,
}
impl Default for BBNode {
    fn default() -> Self {
        Self {
            text: "".to_string(),
            tag: BBTag::None,
            value: None,
            parent: None,
            children: vec![],
        }
    }
}

impl BBNode {
    /// Create a new BBNode with Text and Tag
    pub fn new(text: &str, tag: BBTag) -> BBNode {
        BBNode {
            text: String::from(text),
            tag,
            value: None,
            parent: None,
            children: vec![],
        }
    }
}

/// Main data scructure for parsed BBCode, usually a root node and child nodes
#[derive(Clone, PartialEq, Eq)]
pub struct BBTree {
    /// Nodes stored in the tree
    pub nodes: HashMap<i32, BBNode>,
    id: i32,
}
impl Default for BBTree {
    fn default() -> Self {
        Self {
            nodes: HashMap::new(),
            id: -1,
        }
    }
}
impl Display for BBTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Nodes: {}", self.id)?;
        self.fmt_node(f, 0)
    }
}
impl fmt::Debug for BBTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Nodes: {}", self.id)?;
        self.fmt_node(f, 0)
    }
}

impl BBTree {
    /// Get a node by ID
    pub fn get_node(&self, i: i32) -> &BBNode {
        self.nodes.get(&i).unwrap()
    }
    /// Get a node as mutable by ID
    pub fn get_node_mut(&mut self, i: i32) -> &mut BBNode {
        self.nodes.get_mut(&i).unwrap()
    }
    /// Add a new node and return the new node ID
    pub fn add_node(&mut self, node: BBNode) -> i32 {
        self.id += 1;
        self.nodes.insert(self.id, node);
        self.id
    }
    /// Recursive (I know...) function to get the depth of a given node ID in the tree
    pub fn get_depth(&self, i: i32) -> usize {
        if self.get_node(i).parent.is_none() {
            return 0;
        }
        return 1 + self.get_depth(self.get_node(i).parent.unwrap());
    }
    fn fmt_node(&self, f: &mut std::fmt::Formatter<'_>, i: i32) -> std::fmt::Result {
        let indent = self.get_depth(i) * 2;
        let node = self.get_node(i);
        writeln!(f, "{:indent$}ID    : {}", "", i, indent = indent)?;
        writeln!(f, "{:indent$}Text  : {}", "", node.text, indent = indent)?;
        writeln!(f, "{:indent$}Tag   : {:?}", "", node.tag, indent = indent)?;
        writeln!(f, "{:indent$}Value : {:?}", "", node.value, indent = indent)?;
        writeln!(
            f,
            "{:indent$}Parent: {:?}",
            "",
            node.parent,
            indent = indent
        )?;
        writeln!(f)?;
        for child in node.children.iter() {
            self.fmt_node(f, *child)?;
        }
        Ok(())
    }
}

/// BBCode parser
#[derive(Debug)]
pub struct BBCode {
    open_matcher: Regex,
    close_matcher: Regex,
    newline_matcher: Regex,
}
impl Default for BBCode {
    fn default() -> Self {
        Self {
            open_matcher: Regex::new(RE_OPEN_TAG).unwrap(),
            close_matcher: Regex::new(RE_CLOSE_TAG).unwrap(),
            newline_matcher: Regex::new(RE_NEWLINE).unwrap(),
        }
    }
}

impl BBCode {
    /// Parse the given input into tagged BBCode tree
    pub fn parse(&self, input: &str) -> BBTree {
        // Slice through string until open or close tag match
        let mut slice = &input[0..];

        // set up initial tree with empty node
        // let curr_node = BBNode::new("", BBTag::None);
        let mut tree = BBTree::default();
        let mut curr_node = tree.add_node(BBNode::default());
        let mut closed_tag = false;

        while !slice.is_empty() {
            // special handling for [*] short code
            // check for newline while ListItem is open
            if let Some(captures) = self.newline_matcher.captures(slice) {
                if tree.get_node(curr_node).tag == BBTag::ListItem {
                    // we are in a ListItem, close list item
                    curr_node = tree.get_node(curr_node).parent.unwrap();

                    // move past newline
                    slice = &slice[captures.get(0).unwrap().as_str().len()..];
                    closed_tag = true;
                    continue;
                }
                if tree.get_node(curr_node).parent.is_some()
                    && tree.get_node(tree.get_node(curr_node).parent.unwrap()).tag
                        == BBTag::ListItem
                {
                    // parent is a list item
                    // close current and parent
                    curr_node = tree
                        .get_node(tree.get_node(curr_node).parent.unwrap())
                        .parent
                        .unwrap();
                    // move past newline
                    slice = &slice[captures.get(0).unwrap().as_str().len()..];
                    closed_tag = true;
                    continue;
                }
            }
            // check open
            if let Some(captures) = self.open_matcher.captures(slice) {
                // we have open tag, create child and go deeper
                // if current node has no tag, use it's parent as the parent,
                // instead of creating child of just text
                let tag = captures.name("tag").unwrap().as_str();
                let curr_node_obj = tree.get_node(curr_node);
                // do not attempt to get parent of root node
                if curr_node_obj.tag == BBTag::None && curr_node != 0 {
                    curr_node = curr_node_obj.parent.unwrap();
                }
                let mut node = BBNode {
                    tag: BBTag::from(tag),
                    parent: Some(curr_node),
                    ..Default::default()
                };
                if let Some(val) = captures.name("val") {
                    node.value = Some(val.as_str().to_string());
                }
                let new_id = tree.add_node(node);
                tree.get_node_mut(curr_node).children.push(new_id);
                curr_node = new_id;

                // increment slice past open tag
                slice = &slice[captures.get(0).unwrap().as_str().len()..];
                closed_tag = false;
                continue;
            } else if let Some(captures) = self.close_matcher.captures(slice) {
                // if close tag, check current. If same, end child node and go back up. Otherwise toss the tag and keep going.
                let tag = captures.name("tag").unwrap().as_str();
                let bbtag = BBTag::from(tag);
                let curr_node_obj = tree.get_node(curr_node);
                if curr_node_obj.tag == BBTag::None && !curr_node_obj.text.is_empty() {
                    // current tag is only text, check the parent and close current if has text and matching,
                    // then close parent
                    let parent = tree.get_node(curr_node_obj.parent.unwrap());
                    if parent.tag == bbtag {
                        curr_node = parent.parent.unwrap();
                        slice = &slice[captures.get(0).unwrap().as_str().len()..];
                        closed_tag = true;
                        continue;
                    }
                }
                if bbtag == tree.get_node(curr_node).tag {
                    // matching open and close tags
                    // we're done with this node
                    curr_node = tree.get_node(curr_node).parent.unwrap();
                    // increment slice past close tag
                    slice = &slice[captures.get(0).unwrap().as_str().len()..];
                    closed_tag = true;
                    continue;
                } else {
                    // not a matching close tag, toss the tag and keep going
                    slice = &slice[captures.get(0).unwrap().as_str().len()..];
                    closed_tag = false;
                    continue;
                }
            }

            // no tags, grab text and continue
            if let Some(ch) = slice.chars().next() {
                if closed_tag {
                    // we just closed a tag but have more text to get, create a new node
                    let node = BBNode {
                        parent: Some(curr_node),
                        ..Default::default()
                    };
                    let new_id = tree.add_node(node);
                    tree.get_node_mut(curr_node).children.push(new_id);
                    curr_node = new_id;
                }

                tree.get_node_mut(curr_node).text.push(ch);
                slice = &slice[ch.len_utf8()..];
                closed_tag = false;
            } else {
                // end of the line
                break;
            }
        }

        tree
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! bbtest_regex {
        ($($name:ident: $value:expr;)*) => {
            $(
                #[test]
                fn $name() {
                    let open_re = Regex::new(RE_OPEN_TAG).unwrap();
                    let (input, expected_tag, expected_val) = $value;

                    // check expected match
                    let captures = open_re.captures(input);
                    if expected_tag.is_empty() && expected_val.is_empty() {
                        assert!(captures.is_none());
                    } else {
                        let captures = captures.unwrap();
                        let tag = captures.name("tag").unwrap().as_str();
                        assert_eq!(expected_tag, tag);

                        if expected_val.is_empty() {
                            let val = captures.name("val");
                            assert!(val.is_none());
                        } else {
                            let val = captures.name("val").unwrap().as_str();
                            assert_eq!(expected_val, val);
                        }
                    }
                    // if value not None, check expected value
                    // let val = captures.name("val").unwrap().as_str();
                    // assert_eq!(expected_val, val);


                    // assert_eq!(bbcode.parse(input), expected);
                }
            )*
        }
    }

    macro_rules! test_lines {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected_tree) = $value;
                    let parser = BBCode::default();
                    let tree = parser.parse(input);
                    assert_eq!(expected_tree, tree);
                }
            )*
        }
    }

    test_lines! {
        test_one_tag: (
            "[b]bold text[/b]",
            BBTree {
                nodes: HashMap::from([
                    (0, BBNode {
                        text: "".to_string(),
                        tag: BBTag::None,
                        children: vec![1],
                        ..Default::default()
                    }),
                    (1, BBNode {
                        text: "bold text".to_string(),
                        tag: BBTag::Bold,
                        parent: Some(0),
                        ..Default::default()
                    })
                ]),
                id: 1,
            }),
        test_one_tag_value: (
            r#"[size="3"]big text[/size]"#,
            BBTree {
                nodes: HashMap::from([
                    (0, BBNode {
                        text: "".to_string(),
                        tag: BBTag::None,
                        children: vec![1],
                        ..Default::default()
                    }),
                    (1, BBNode {
                        text: "big text".to_string(),
                        tag: BBTag::FontSize,
                        value: Some("3".to_string()),
                        parent: Some(0),
                        ..Default::default()
                    })
                ]),
                id: 1,
            }),
        test_braces_not_tags: (
            "text [] is [i]a[/i] thing",
            BBTree {
                nodes: HashMap::from([
                    (0, BBNode {
                        text: "text [] is ".to_string(),
                        children: vec![1, 2],
                        ..Default::default()
                    }),
                    (1, BBNode {
                        text: "a".to_string(),
                        tag: BBTag::Italic,
                        parent: Some(0),
                        ..Default::default()
                    }),
                    (2, BBNode {
                        text: " thing".to_string(),
                        parent: Some(0),
                        ..Default::default()
                    })
                ]),
                id: 2
            }),
        test_post_text: (
            "[i]thing[/i] after",
            BBTree {
                nodes: HashMap::from([
                    (0, BBNode {
                        children: vec![1, 2],
                        ..Default::default()
                    }),
                    (1, BBNode {
                        text: "thing".to_string(),
                        tag: BBTag::Italic,
                        parent: Some(0),
                        ..Default::default()
                    }),
                    (2, BBNode {
                        text: " after".to_string(),
                        parent: Some(0),
                        ..Default::default()
                    })
                ]),
                id: 2
            }
        ),
        test_ul_list_items: (
            r#"[ul]
[*]item one
[*]item two
[/ul]"#,
            BBTree {
                nodes: HashMap::from([
                    (0, BBNode {
                        children: vec![1],
                        ..Default::default()
                    }),
                    (1, BBNode {
                        text: "\n".to_string(),
                        tag: BBTag::ListUnordered,
                        parent: Some(0),
                        children: vec![2, 3],
                        ..Default::default()
                    }),
                    (2, BBNode {
                        text: "item one".to_string(),
                        tag: BBTag::ListItem,
                        parent: Some(1),
                        ..Default::default()
                    }),
                    (3, BBNode {
                        text: "item two".to_string(),
                        tag: BBTag::ListItem,
                        parent: Some(1),
                        ..Default::default()
                    })
                ]),
                id: 3
            }
        ),
        test_ul_list_item_subtag: (
            r#"[ul]
[*][SIZE=4]wow[/SIZE]
[/ul]"#,
            BBTree {
                nodes: HashMap::from([
                    (0, BBNode {
                        children: vec![1],
                        ..Default::default()
                    }),
                    (1, BBNode {
                        text: "\n".to_string(),
                        tag: BBTag::ListUnordered,
                        parent: Some(0),
                        children: vec![2],
                        ..Default::default()
                    }),
                    (2, BBNode {
                        tag: BBTag::ListItem,
                        parent: Some(1),
                        children: vec![3],
                        ..Default::default()
                    }),
                    (3, BBNode {
                        tag: BBTag::FontSize,
                        value: Some("4".to_string()),
                        text: "wow".to_string(),
                        parent: Some(2),
                        ..Default::default()
                    })
                ]),
                id: 3
            }
        ),
        test_serveral: (
            r#"[COLOR=#E5E5E5][CENTER][SIZE=5][COLOR=#00ffff]This Is A Title[/COLOR][/SIZE][/CENTER]

[/COLOR][CENTER][SIZE=4][COLOR=#ff8c00]Now with new stuff![/COLOR][/SIZE][/CENTER]"#,
            BBTree {
                nodes: HashMap::from([
                    (0, BBNode {
                        children: vec![1, 6],
                        ..Default::default()
                    }),
                    (1, BBNode {
                        tag: BBTag::FontColor,
                        value: Some("#E5E5E5".to_string()),
                        parent: Some(0),
                        children: vec![2, 5],
                        ..Default::default()
                    }),
                    (2, BBNode {
                        tag: BBTag::Center,
                        parent: Some(1),
                        children: vec![3],
                        ..Default::default()
                    }),
                    (3, BBNode {
                        tag: BBTag::FontSize,
                        value: Some("5".to_string()),
                        parent: Some(2),
                        children: vec![4],
                        ..Default::default()
                    }),
                    (4, BBNode {
                        text: "This Is A Title".to_string(),
                        tag: BBTag::FontColor,
                        value: Some("#00ffff".to_string()),
                        parent: Some(3),
                        ..Default::default()
                    }),
                    (5, BBNode {
                        text: "\n\n".to_string(),
                        parent: Some(1),
                        ..Default::default()
                    }),
                    (6, BBNode {
                        tag: BBTag::Center,
                        parent: Some(0),
                        children: vec![7],
                        ..Default::default()
                    }),
                    (7, BBNode {
                        tag: BBTag::FontSize,
                        value: Some("4".to_string()),
                        parent: Some(6),
                        children: vec![8],
                        ..Default::default()
                    }),
                    (8, BBNode {
                        text: "Now with new stuff!".to_string(),
                        tag: BBTag::FontColor,
                        value: Some("#ff8c00".to_string()),
                        parent: Some(7),
                        ..Default::default()
                    })
                ]),
                id: 8
            }
        ),
        test_complex_large: (
            r#"[I][B][COLOR="Orange"]AddOn Name[/COLOR][/B][/I] is an addon

[COLOR="DeepSkyBlue"]Color text:[/COLOR]
[LIST]
[*][COLOR="Orange"]Colored list item1[/COLOR]
[LIST]
[*]sublist1 item
[*]item2:
[LIST]
[*]Sublist2 item [I]wow[/I] and [I]wooh[/I], is a thing
[/LIST]

Non listitem text in list.
[/LIST]


[*][COLOR="Orange"]List item2[/COLOR]
[/LIST]

[LIST]
[*][COLOR="Orange"]color list item:[/COLOR][LIST][*] inline sub list item
[/LIST][/LIST]
"#,
            BBTree {
                nodes: HashMap::from([
                    (0, BBNode {
                        children: vec![1, 4, 5, 6, 7, 23, 24],
                        ..Default::default()
                    }),
                    (1, BBNode {
                        tag: BBTag::Italic,
                        parent: Some(0),
                        children: vec![2],
                        ..Default::default()
                    }),
                    (2, BBNode {
                        tag: BBTag::Bold,
                        parent: Some(1),
                        children: vec![3],
                        ..Default::default()
                    }),
                    (3, BBNode {
                        text: "AddOn Name".to_string(),
                        tag: BBTag::FontColor,
                        value: Some("Orange".to_string()),
                        parent: Some(2),
                        ..Default::default()
                    }),
                    (4, BBNode {
                        text: " is an addon\n\n".to_string(),
                        parent: Some(0),
                        ..Default::default()
                    }),
                    (5, BBNode {
                        text: "Color text:".to_string(),
                        tag: BBTag::FontColor,
                        value: Some("DeepSkyBlue".to_string()),
                        parent: Some(0),
                        ..Default::default()
                    }),
                    (6, BBNode {
                        text: "\n".to_string(),
                        parent: Some(0),
                        ..Default::default()
                    }),
                    (7, BBNode {
                        text: "\n".to_string(),
                        tag: BBTag::ListUnordered,
                        parent: Some(0),
                        children: vec![8, 10, 20, 21],
                        ..Default::default()
                    }),
                    (8, BBNode {
                        tag: BBTag::ListItem,
                        parent: Some(7),
                        children: vec![9],
                        ..Default::default()
                    }),
                    (9, BBNode {
                        text: "Colored list item1".to_string(),
                        tag: BBTag::FontColor,
                        value: Some("Orange".to_string()),
                        parent: Some(8),
                        ..Default::default()
                    }),
                    (10, BBNode {
                        text: "\n".to_string(),
                        tag: BBTag::ListUnordered,
                        parent: Some(7),
                        children: vec![11, 12, 13, 19],
                        ..Default::default()
                    }),
                    (11, BBNode {
                        text: "sublist1 item".to_string(),
                        tag: BBTag::ListItem,
                        parent: Some(10),
                        ..Default::default()
                    }),
                    (12, BBNode {
                        text: "item2:".to_string(),
                        tag: BBTag::ListItem,
                        parent: Some(10),
                        ..Default::default()
                    }),
                    (13, BBNode {
                        text: "\n".to_string(),
                        tag: BBTag::ListUnordered,
                        parent: Some(10),
                        children: vec![14],
                        ..Default::default()
                    }),
                    (14, BBNode {
                        text: "Sublist2 item ".to_string(),
                        tag: BBTag::ListItem,
                        parent: Some(13),
                        children: vec![15, 16, 17, 18],
                        ..Default::default()
                    }),
                    (15, BBNode {
                        text: "wow".to_string(),
                        tag: BBTag::Italic,
                        parent: Some(14),
                        ..Default::default()
                    }),
                    (16, BBNode {
                        text: " and ".to_string(),
                        parent: Some(14),
                        ..Default::default()
                    }),
                    (17, BBNode {
                        text: "wooh".to_string(),
                        tag: BBTag::Italic,
                        parent: Some(14),
                        ..Default::default()
                    }),
                    (18, BBNode {
                        text: ", is a thing".to_string(),
                        parent: Some(14),
                        ..Default::default()
                    }),
                    (19, BBNode {
                        text: "\n\nNon listitem text in list.\n".to_string(),
                        parent: Some(10),
                        ..Default::default()
                    }),
                    (20, BBNode {
                        text: "\n\n\n".to_string(),
                        parent: Some(7),
                        ..Default::default()
                    }),
                    (21, BBNode {
                        tag: BBTag::ListItem,
                        parent: Some(7),
                        children: vec![22],
                        ..Default::default()
                    }),
                    (22, BBNode {
                        text: "List item2".to_string(),
                        tag: BBTag::FontColor,
                        value: Some("Orange".to_string()),
                        parent: Some(21),
                        ..Default::default()
                    }),
                    (23, BBNode {
                        text: "\n\n".to_string(),
                        parent: Some(0),
                        ..Default::default()
                    }),
                    (24, BBNode {
                        text: "\n".to_string(),
                        tag: BBTag::ListUnordered,
                        parent: Some(0),
                        children: vec![25],
                        ..Default::default()
                    }),
                    (25, BBNode {
                        tag: BBTag::ListItem,
                        parent: Some(24),
                        children: vec![26, 27],
                        ..Default::default()
                    }),
                    (26, BBNode {
                        text: "color list item:".to_string(),
                        tag: BBTag::FontColor,
                        value: Some("Orange".to_string()),
                        parent: Some(25),
                        ..Default::default()
                    }),
                    (27, BBNode {
                        tag: BBTag::ListUnordered,
                        parent: Some(25),
                        children: vec![28],
                        ..Default::default()
                    }),
                    (28, BBNode {
                        text: " inline sub list item".to_string(),
                        tag: BBTag::ListItem,
                        parent: Some(27),
                        ..Default::default()
                    })
                ]),
                id: 28
            }
        ),
    }

    #[test]
    fn build_re() {
        // should not fail
        let _open_re = Regex::new(RE_OPEN_TAG).unwrap();
        let _close_re = Regex::new(RE_CLOSE_TAG).unwrap();
    }

    #[test]
    fn bbcode_default() {
        // init should not fail with default regex
        let _bbcode = BBCode::default();
    }

    #[test]
    fn parse() {
        let parser = BBCode::default();
        // let result = parser.parse("[b]hello[/b]");
        let tree = parser.parse(r#"[SIZE="3"]Features:[/SIZE]

[LIST]
[*][B][URL=https://github.com/sirinsidiator/ESO-LibAddonMenu/wiki/Controls]Controls[/URL][/B] - LAM offers different control types to build elaborate settings menus
[*][B]Reset to Default[/B] - LAM can restore the settings to their default state with one key press
[*][B]Additional AddOn Info[/B] - Add a version label and URLs for website, donations, translations or feedback
[*][B]AddOn Search[/B] - Can't find the settings for your AddOn between the other hundred entries? No problem! Simply use the text search to quickly find what you are looking for
[*][B]Slash Commands[/B] - Provides a shortcut to open your settings menu from chat
[*][B]Tooltips[/B] - In case you need more space to explain what a control does, simply use a tooltip
[*][B]Warnings[/B] - If your setting causes some unexpected behaviour, you can simply slap a warning on them
[*][B]Dangerous Buttons[/B] - when flagged as such, a button will have red text and ask for confirmation before it runs any action
[*][B]Required UI Reload[/B] - For cases where settings have to reload the UI or should be stored to disk right away, LAM offers a user friendly way to ask for a UI reload.
[*]Support for all 5 official languages and 6 custom localisation projects
[/LIST]"#);
        println!("{}", tree);

        // assert_eq!("".to_string(), result.borrow().text);
        // assert_eq!(BBTag::None, result.borrow().tag);
        // assert_eq!(1, result.borrow().children.len());

        // let child = result.borrow_mut().children.pop().unwrap();
        // assert_eq!("hello".to_string(), child.borrow().text);
        // assert_eq!(BBTag::Bold, child.borrow().tag);
    }

    // test [*] short code
    // [list]
    // [*]item
    // [*]item2 yeah[
    //
    // [*]three
    // [*] second [i]things[/i]
    // [*] trick [/*]

    bbtest_regex! {
        empty: ("hello", "", "");
        bold: ("[b]hello[/b]", "b", "");
        no_tag: ("[]hello[/]", "", "");
        tag_and_val: ("[size=3]large[/size]", "size", "3");
        tag_and_val_quote: (r#"[size="3"]large[/size]"#, "size", "3");
        url: ("[url=https://www.com]some url[/url] ", "url", "https://www.com");
        multi_tag: (r#"[SIZE="2"][COLOR=#e5e5e5][B]Team:[/COLOR][/B] "#, "SIZE", "2");
    }

    #[test]
    fn test_node_eq() {
        let node1 = BBNode {
            text: "text1".to_string(),
            ..Default::default()
        };
        let node2 = BBNode {
            text: "text1".to_string(),
            ..Default::default()
        };
        assert_eq!(node1, node2);
    }

    #[test]
    fn test_node_ne_text() {
        let node1 = BBNode {
            text: "text1".to_string(),
            ..Default::default()
        };
        let node2 = BBNode {
            text: "text2".to_string(),
            ..Default::default()
        };
        assert_ne!(node1, node2);
    }

    #[test]
    fn test_node_ne_child() {
        let node1 = BBNode {
            text: "text1".to_string(),
            children: vec![1],
            ..Default::default()
        };
        let node2 = BBNode {
            text: "text1".to_string(),
            children: vec![2],
            ..Default::default()
        };
        assert_ne!(node1, node2);
    }
}
