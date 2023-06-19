#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Tag {
    // Operators
    Plus, PlusEqual,
    Minus, MinusEqual,
    Slash, SlashEqual,
    Asterisk, AsteriskEqual,
    
    Dot,
    DotDot, // Range
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Meta tokens
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Semicolon,
    Colon,
    Comma,

    // Literals
    Ident,
    String,
    Bool,
    Number,

    // Keywords
    Fn,
    If,
    Else,
    Return,
    While,
    For,
    Let,
    Break,
    Continue,

    UnexpectedEof,
    Invalid
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub(crate) tag: Tag,
    pub(crate) pos: u32,
    pub(crate) end: u32,
    pub(crate) line: u32
}

pub struct SourceToken<'s> {
    pub(crate) token: Token,
    pub(crate) value: &'s str
}

impl<'s> SourceToken<'s> {
    pub fn new(token: Token, value: &'s str) -> Self {
        Self { token, value }
    }
}

impl Token {
    pub fn new(tag: Tag, pos: u32, end: u32, line: u32) -> Self {
        Self {
            tag,
            pos,
            end,
            line
        }
    }
}