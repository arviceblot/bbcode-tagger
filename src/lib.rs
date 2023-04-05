use regex::Regex;
use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

static RE_OPEN_TAG: &str = r#"^\[(?P<tag>[^/\]]+?\S*?)((?:[ \t]+\S+?)?="?(?P<val>[^\]\n]*?))?"?\]"#;
static RE_CLOSE_TAG: &str = r#"^\[/(?P<tag>[^/\]]+?\S*?)\]"#;

#[derive(Debug, PartialEq, Eq)]
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
    Quote,
    Spoiler,
    Link,
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
            "quote" => BBTag::Quote,
            "spoiler" => BBTag::Spoiler,
            "url" => BBTag::Link,
            "img" => BBTag::Image,
            "ul" => BBTag::ListUnordered,
            "list" => BBTag::ListUnordered,
            "ol" => BBTag::ListOrdered,
            "li" => BBTag::ListItem,
            "*" => BBTag::ListItem,
            "code" => BBTag::Code,
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

pub enum MatchType {
    Open,
    Close,
}

#[derive(Debug)]
pub struct BBNode {
    pub text: String,
    pub tag: BBTag,
    pub value: Option<String>,
    pub parent: Option<Rc<RefCell<BBNode>>>,
    pub children: Vec<Rc<RefCell<BBNode>>>,
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
impl Display for BBNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let indent = usize::try_from(self.get_depth() * 2).unwrap();
        writeln!(f, "{:indent$}Text  : {}", "", self.text, indent = indent)?;
        writeln!(f, "{:indent$}Tag   : {:?}", "", self.tag, indent = indent)?;
        writeln!(f, "{:indent$}Value : {:?}", "", self.value, indent = indent)?;
        writeln!(
            f,
            "{:indent$}Parent: {}",
            "",
            self.parent.is_some(),
            indent = indent
        )?;
        writeln!(f)?;
        for child in self.children.iter() {
            child.borrow().fmt(f)?;
        }
        Ok(())
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
    fn get_depth(&self) -> i32 {
        if self.parent.is_none() {
            return 0;
        }
        return 1 + self.parent.as_ref().unwrap().borrow().get_depth();
    }
}

#[allow(dead_code)]
struct BBCode {
    open_matcher: Regex,
    close_matcher: Regex,
}
impl Default for BBCode {
    fn default() -> Self {
        Self {
            open_matcher: Regex::new(RE_OPEN_TAG).unwrap(),
            close_matcher: Regex::new(RE_CLOSE_TAG).unwrap(),
        }
    }
}

impl BBCode {
    #[allow(dead_code)]
    pub fn parse(&self, input: &str) -> Rc<RefCell<BBNode>> {
        // Slice through string until open or close tag match
        let mut slice = &input[0..];

        // set up initial tree with empty node
        // let curr_node = BBNode::new("", BBTag::None);
        let root = Rc::new(RefCell::new(BBNode::default()));
        let mut curr_node = root.clone();
        let mut closed_tag = false;

        while !slice.is_empty() {
            // check open
            let captures = self.open_matcher.captures(slice);
            if let Some(captures) = captures {
                // we have open tag
                // create child and go deeper
                let tag = captures.name("tag").unwrap().as_str();
                let bbtag = BBTag::get_tag(tag);
                let node = Rc::new(RefCell::new(BBNode::new("", bbtag)));
                if let Some(val) = captures.name("val") {
                    node.borrow_mut().value = Some(val.as_str().to_string());
                }
                node.borrow_mut().parent = Some(curr_node.clone());
                curr_node.borrow_mut().children.push(node.clone());
                curr_node = node.clone();

                // increment slice past open tag
                slice = &slice[captures.get(0).unwrap().as_str().len()..];
                closed_tag = false;
                continue;
            } else if let Some(captures) = self.close_matcher.captures(slice) {
                // if close tag, check current. If same, end child node and go back up. Otherwise toss the tag and keep going.
                let tag = captures.name("tag").unwrap().as_str();
                let bbtag = BBTag::get_tag(tag);
                if bbtag == curr_node.borrow().tag {
                    // matching open and close tags
                    // we're done with this node
                    let new_curr = curr_node.borrow().parent.clone().unwrap();
                    curr_node = new_curr.clone();
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
                    let node = Rc::new(RefCell::new(BBNode::new("", BBTag::None)));
                    node.borrow_mut().parent = Some(curr_node.clone());
                    curr_node.borrow_mut().children.push(node.clone());
                    curr_node = node.clone();
                }

                curr_node.borrow_mut().text.push(ch);
                // curr_node.text.push(ch);
                slice = &slice[ch.len_utf8()..];
                closed_tag = false;
            } else {
                // end of the line
                break;
            }
        }
        // println!("{}", root.borrow());
        root
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
        let result = parser.parse(r#"[i]oh no[/i] KR Patch for [B][SIZE="4"][URL="https://www.esoui.com/downloads/info1245-TamrielTradeCentre.html"][]Tamriel Trade Centre[/][/URL][/SIZE][/B] or something"#);
        let item = result.take();
        println!("{}", item);

        // assert_eq!("".to_string(), result.borrow().text);
        // assert_eq!(BBTag::None, result.borrow().tag);
        // assert_eq!(1, result.borrow().children.len());

        // let child = result.borrow_mut().children.pop().unwrap();
        // assert_eq!("hello".to_string(), child.borrow().text);
        // assert_eq!(BBTag::Bold, child.borrow().tag);
    }

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
