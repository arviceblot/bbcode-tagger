use regex::Regex;
use std::collections::HashMap;
use std::fmt::Display;

static RE_OPEN_TAG: &str = r#"^\[(?P<tag>[^/\]]+?\S*?)((?:[ \t]+\S+?)?="?(?P<val>[^\]\n]*?))?"?\]"#;
static RE_CLOSE_TAG: &str = r#"^\[/(?P<tag>[^/\]]+?\S*?)\]"#;
static RE_NEWLINE: &str = r#"^\r?\n"#;

/// TODO: Handle some extra codes
/// - indent

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BBTag {
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
    Unknown,
}
impl BBTag {
    pub fn get_tag(tag: &str) -> BBTag {
        let binding = tag.trim().to_lowercase();
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

#[derive(Debug, Clone)]
pub struct BBNode {
    pub text: String,
    pub tag: BBTag,
    pub value: Option<String>,
    pub parent: Option<i32>,
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

#[derive(Clone)]
pub struct BBTree {
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
        self.fmt_node(f, 0)
    }
}

impl BBTree {
    // Get a node by ID
    pub fn get_node(&self, i: i32) -> &BBNode {
        self.nodes.get(&i).unwrap()
    }
    // Get a node as mutable by ID
    pub fn get_node_mut(&mut self, i: i32) -> &mut BBNode {
        self.nodes.get_mut(&i).unwrap()
    }
    // Add a new node and return the new node ID
    pub fn add_node(&mut self, node: BBNode) -> i32 {
        self.id += 1;
        self.nodes.insert(self.id, node);
        self.id
    }
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

#[allow(dead_code)]
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
    #[allow(dead_code)]
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
            let captures = self.newline_matcher.captures(slice);
            if captures.is_some() {
                if tree.get_node(curr_node).tag == BBTag::ListItem {
                    // close list item
                    curr_node = tree.get_node(curr_node).parent.unwrap();

                    // move past newline
                    slice = &slice[captures.unwrap().get(0).unwrap().as_str().len()..];
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
                    slice = &slice[captures.unwrap().get(0).unwrap().as_str().len()..];
                    closed_tag = true;
                    continue;
                }
            }
            // check open
            let captures = self.open_matcher.captures(slice);
            if let Some(captures) = captures {
                // we have open tag
                // create child and go deeper
                let tag = captures.name("tag").unwrap().as_str();
                let bbtag = BBTag::get_tag(tag);
                let mut node = BBNode::new("", bbtag);
                node.parent = Some(curr_node);
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
                let bbtag = BBTag::get_tag(tag);
                if bbtag == tree.get_node(curr_node).tag {
                    // matching open and close tags
                    // we're done with this node
                    curr_node = tree.get_node(curr_node).parent.unwrap();
                    // increment slice past close tag
                    slice = &slice[captures.get(0).unwrap().as_str().len()..];
                    closed_tag = true;
                    continue;
                }
            }

            // no tags, grab text and continue
            if let Some(ch) = slice.chars().next() {
                if closed_tag {
                    // we just closed a tag but have more text to get, create a new node
                    let mut node = BBNode::default();
                    node.parent = Some(curr_node);
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

    macro_rules! bbtest_all {
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

    bbtest_all! {
        empty: ("hello", "", "");
        bold: ("[b]hello[/b]", "b", "");
        no_tag: ("[]hello[/]", "", "");
        tag_and_val: ("[size=3]large[/size]", "size", "3");
        tag_and_val_quote: (r#"[size="3"]large[/size]"#, "size", "3");
        url: ("[url=https://www.com]some url[/url] ", "url", "https://www.com");
        multi_tag: (r#"[SIZE="2"][COLOR=#e5e5e5][B]Team:[/COLOR][/B] "#, "SIZE", "2");

    }
}
