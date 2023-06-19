use crate::token::{Tag, Token, SourceToken};

/// A lexer that turns a string literal to a stream of tokens.
#[derive(Clone, Copy, Debug)]
pub struct Lexer<'a> {
    /// Reference to source
    pub(crate) src: &'a str,
    /// The cursor of the lexer - Always at the start of a char boundary
    pub(crate) offset: u32,
    line: u32,
}

impl<'a> Lexer<'a> {
    /// Creates a `Lexer` from a string literal
    /// 
    /// String literals greater than `u32::MAX` in size will cause a panic
    pub fn from(source: &'a str) -> Self {
        assert!(source.len() < u32::MAX as usize, "4GiB is the maximum source size");
        Self {
            src: source,
            offset: 0,
            line: 1,
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        if let Some((idx, ch)) = self.next_ch() {
            let mut tag = Tag::Invalid;
            match ch {
                '+' => tag = self.bi_tok('=', Tag::Plus, Tag::PlusEqual),
                '-' => tag = self.bi_tok('=', Tag::Minus, Tag::MinusEqual),
                '/' => tag = self.bi_tok('=', Tag::Slash, Tag::SlashEqual),
                '*' => tag = self.bi_tok('=', Tag::Asterisk, Tag::AsteriskEqual),

                '!' => tag = self.bi_tok('=', Tag::Bang, Tag::BangEqual),
                '=' => tag = self.bi_tok('=', Tag::Equal, Tag::EqualEqual),
                '>' => tag = self.bi_tok('=', Tag::Greater, Tag::GreaterEqual),
                '<' => tag = self.bi_tok('=', Tag::Less, Tag::LessEqual),
    
                '.' => tag = self.bi_tok('.', Tag::Dot, Tag::DotDot),

                ',' => tag = Tag::Comma,
                ';' => tag = Tag::Semicolon,
                ':' => tag = Tag::Colon,

                '[' => tag = Tag::LBracket,
                ']' => tag = Tag::RBracket,
                '{' => tag = Tag::LBrace,
                '}' => tag = Tag::RBrace,
                '(' => tag = Tag::LParen,
                ')' => tag = Tag::RParen,

                '"' => tag = self.lex_string(),
                '0'..='9' => tag = self.lex_number(),

                _ if ch.is_alphabetic() || ch == '_' => {
                    self.read_ident();
                    tag = Self::match_keyword(unsafe { self.slice(idx) });
                },

                _ => {}
            }
            Some(Token::new(tag, idx, self.offset, self.line))
        } else {
            None
        }
    }

    #[inline]
    fn skip_whitespace(&mut self) {
        loop {
            match self.peek_off() {
                Some('\n') => {
                    self.bump();
                    self.line += 1;
                }
                Some('\t' | '\r' | ' ') => {
                    self.bump();
                }
                _ => break,
            }
        }
    }

    #[inline]
    fn bi_tok(&mut self, next_ch: char, tag: Tag, matched_tag: Tag) -> Tag {
        match self.peek_off() {
            Some(ch) if ch == next_ch => {
                self.bump();
                matched_tag
            }
            _ => tag,
        }
    }

    /// Consumes a string literal
    /// 
    /// String literals have opening and closing quotes(`""`)
    /// The content of a string literal may be any sequence of 
    /// characters.
    /// 
    /// Quotes used in string literals must be escaped: `"  \"Hello\" "`
    fn lex_string(&mut self) -> Tag {
        loop {
            match self.peek_off() {
                Some('"') => {
                    self.bump();
                    return Tag::String;
                }
                Some('\n') => {
                    self.bump();
                    self.line += 1;
                }
                Some('\\') => {
                    self.bump();
                    if matches!(self.peek_off(), Some('\\' | '"')) {
                        self.bump();
                    }
                }
                None => return Tag::UnexpectedEof,
                _ => {
                    let _ = self.next_ch();
                }
            }
        }
    }

    /// Consumes a number 
    /// 
    /// `Regex` - /\d+\.\d+/
    fn lex_number(&mut self) -> Tag {
        loop {
            match self.peek_off() {
                Some('0'..='9') => self.bump(),
                Some('.') => {
                    self.bump();
                    loop {
                        match self.peek_off() {
                            Some('0'..='9') => self.bump(),
                            _ => break,
                        }
                    }
                    return Tag::Number;
                }
                _ => return Tag::Number,
            }
        }
    }

    /// Consumes an identifier-like section from the source
    /// 
    /// Identifiers are single tokens(no space between) starting with 
    /// any alphabetic unicode codepoint or `_` which may be followed by any alphanumeric 
    /// unicode codepoint
    fn read_ident(&mut self) {
        loop {
            match self.peek_off() {
                Some('0'..='9' | 'a'..='z' | 'A'..='Z' | '_') => self.bump(),
                Some(ch) if ch.is_ascii() => return ,
                Some(ch) if ch.is_alphanumeric() => {
                    self.next_ch();
                }
                _ => return ,
            }
        }
    }

    #[inline]
    fn match_keyword(ident: &str) -> Tag {
        match ident {
            "fn" => Tag::Fn,
            "if" => Tag::If,
            "else" => Tag::Else,
            "return" => Tag::Return,
            "while" => Tag::While,
            "for" => Tag::For,
            "let" => Tag::Let,
            "true" => Tag::Bool,
            "false" => Tag::Bool,
            "break" => Tag::Break,
            "contine" => Tag::Continue,
            _ => Tag::Ident,
        }
    }

    #[inline]
    /// Consume a byte out of the source
    ///
    /// This method is mostly used to advance over ASCII characters after
    /// peeking them. This is a bit faster than calling `self.next_ch` which tries 
    /// to advance by the next codepoint's length.
    fn bump(&mut self) {
        self.offset += 1;
    }

    /// Creates a string slice from the specified index to `self.offset`
    /// 
    /// # Safety
    /// - The index must not exceed the `self.offset`
    /// - Indexes must lie on UTF-8 sequence boundaries.
    #[inline]
    unsafe fn slice(&self, idx: u32) -> &'a str {
        self.slice_source(idx as usize, self.offset as usize)
    }

    /// Creates a string slice in the range, `start..end` of self.src
    /// 
    /// # Safety
    /// Callers of this function are responsible that these preconditions are satisfied:
    /// 
    /// - The starting index must not exceed the ending index;
    /// - Indexes must be within bounds of the original slice;
    /// - Indexes must lie on UTF-8 sequence boundaries.
    /// 
    /// Failing that, the returned string slice may reference invalid memory or violate the invariants communicated by the `str` type.
    #[inline]
    unsafe fn slice_source(&self, start: usize, end: usize) -> &'a str {
        use std::slice::from_raw_parts;
        use std::str::from_utf8_unchecked;

        from_utf8_unchecked(from_raw_parts(self.src.as_ptr().add(start), end - start))
    }

    // read next char without consuming it
    fn peek_off(&mut self) -> Option<char> {
        unsafe {
            // SAFETY: `self.offset` is always a char offset 
            let mut iter = self.src.get_unchecked(self.offset as usize..).as_bytes().iter();
            // SAFETY: `str` enforces valid UTF-8 requirement
            corechar::next_code_point(&mut iter)
                .map(|ch| char::from_u32_unchecked(ch))
        }
    }

    // Consume next char in the source
    #[inline] // A man can dream 
    pub(crate) fn next_ch(&mut self) -> Option<(u32, char)> {
        unsafe {
            // SAFETY: `self.offset` is always a char offset 
            let mut iter = self.src.get_unchecked(self.offset as usize..).as_bytes().iter();
            // SAFETY: `str` enforces valid UTF-8 requirement for #1 and #2
            match corechar::next_code_point(&mut iter) { // #1
                None => None,
                Some(ch) => {
                    // current byte offset of `ch`  
                    let index = self.offset;
                    let ch = char::from_u32_unchecked(ch); // #2
                    self.offset += ch.len_utf8() as u32;
                    Some((index, ch))
                }
            }
        } 
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

pub struct SourceLexer<'a>(pub Lexer<'a>);

impl<'a> Iterator for SourceLexer<'a> {
    type Item = SourceToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.0.next_token()?;
        let value = unsafe { self.0.slice_source(tok.pos as usize, tok.end as usize) } ; 
        Some(SourceToken::new(tok, value))
        
    }    
}

mod corechar {
    /// Mask of the value bits of a continuation byte.
    const CONT_MASK: u8 = 0b0011_1111;

    /// Returns the initial codepoint accumulator for the first byte.
    /// The first byte is special, only want bottom 5 bits for width 2, 4 bits
    /// for width 3, and 3 bits for width 4.
    #[inline]
    const fn utf8_first_byte(byte: u8, width: u32) -> u32 {
        (byte & (0x7F >> width)) as u32
    }

    /// Returns the value of `ch` updated with continuation byte `byte`.
    #[inline]
    const fn utf8_acc_cont_byte(ch: u32, byte: u8) -> u32 {
        (ch << 6) | (byte & CONT_MASK) as u32
    }

    /// Reads the next code point out of a byte iterator (assuming a
    /// UTF-8-like encoding).
    ///
    /// # Safety
    ///
    /// `bytes` must produce a valid UTF-8-like (UTF-8 or WTF-8) string
    #[inline]
    pub unsafe fn next_code_point<'a, I: Iterator<Item = &'a u8>>(bytes: &mut I) -> Option<u32> {
        // Decode UTF-8
        let x = *bytes.next()?;
        if x < 128 {
            return Some(x as u32);
        }

        // Multibyte case follows
        // Decode from a byte combination out of: [[[x y] z] w]
        // NOTE: Performance is sensitive to the exact formulation here
        let init = utf8_first_byte(x, 2);
        // SAFETY: `bytes` produces an UTF-8-like string,
        // so the iterator must produce a value here.
        let y = *bytes.next().unwrap_unchecked();
        let mut ch = utf8_acc_cont_byte(init, y);
        if x >= 0xE0 {
            // [[x y z] w] case
            // 5th bit in 0xE0 .. 0xEF is always clear, so `init` is still valid
            // SAFETY: `bytes` produces an UTF-8-like string,
            // so the iterator must produce a value here.
            let z = *bytes.next().unwrap_unchecked();
            let y_z = utf8_acc_cont_byte((y & CONT_MASK) as u32, z);
            ch = init << 12 | y_z;
            if x >= 0xF0 {
                // [x y z w] case
                // use only the lower 3 bits of `init`
                // SAFETY: `bytes` produces an UTF-8-like string,
                // so the iterator must produce a value here.
                let w = *bytes.next().unwrap_unchecked();
                ch = (init & 7) << 18 | utf8_acc_cont_byte(y_z, w);
            }
        }
        Some(ch)
    }
}

#[cfg(test)]
mod tests {
    use super::Tag::{self, *};

    use super::{SourceLexer, Lexer};

    fn make_lex<'a>(source: &'a str) -> SourceLexer {
        SourceLexer(Lexer::from(source)) 
    }

    fn lex<'a>(source: &'a str) -> Vec<(Tag, &'a str)> {
        make_lex(source).map(|tok| (tok.token.tag, tok.value)).collect()
    }

    const WHOLE_SOURCE: &str = r#"let PI = 3.14;

    fn area_circle(radius) {
        return PI * radius * radius;
    }
    
    let radius = int(input("What is the radius"));
    print(area_circle(radius));
    
    let students = [];
    
    while true {
        print("Enter student record");
        let name = input("Student Name: ");
        let age = int(input("Student Age: "));
        let class = input("Student's class");
    
    
        if age > 18 {
            return print("This person is too old");
        }
        if class.len() > 3 { return print("Invalid class name"); }
    
        students.push((name, age, class));
    
        if input("Exit? ") {
            return print("Exiting record system")
        }
    }"#;

    #[test]
    fn test_whole_source() {
        let tokens = lex(WHOLE_SOURCE);
        assert_eq!(
            tokens,
            vec![
                (Let, "let"),
                (Ident, "PI"),
                (Equal, "="),
                (Number, "3.14"),
                (Semicolon, ";"),
                (Fn, "fn"),
                (Ident, "area_circle"),
                (LParen, "("),
                (Ident, "radius"),
                (RParen, ")"),
                (LBrace, "{"),
                (Return, "return"),
                (Ident, "PI"),
                (Asterisk, "*"),
                (Ident, "radius"),
                (Asterisk, "*"),
                (Ident, "radius"),
                (Semicolon, ";"),
                (RBrace, "}"),
                (Let, "let"),
                (Ident, "radius"),
                (Equal, "="),
                (Ident, "int"),
                (LParen, "("),
                (Ident, "input"),
                (LParen, "("),
                (String, "\"What is the radius\""),
                (RParen, ")"),
                (RParen, ")"),
                (Semicolon, ";"),
                (Ident, "print"),
                (LParen, "("),
                (Ident, "area_circle"),
                (LParen, "("),
                (Ident, "radius"),
                (RParen, ")"),
                (RParen, ")"),
                (Semicolon, ";"),
                (Let, "let"),
                (Ident, "students"),
                (Equal, "="),
                (LBracket, "["),
                (RBracket, "]"),
                (Semicolon, ";"),
                (While, "while"),
                (Bool, "true"),
                (LBrace, "{"),
                (Ident, "print"),
                (LParen, "("),
                (String, "\"Enter student record\""),
                (RParen, ")"),
                (Semicolon, ";"),
                (Let, "let"),
                (Ident, "name"),
                (Equal, "="),
                (Ident, "input"),
                (LParen, "("),
                (String, "\"Student Name: \""),
                (RParen, ")"),
                (Semicolon, ";"),
                (Let, "let"),
                (Ident, "age"),
                (Equal, "="),
                (Ident, "int"),
                (LParen, "("),
                (Ident, "input"),
                (LParen, "("),
                (String, "\"Student Age: \""),
                (RParen, ")"),
                (RParen, ")"),
                (Semicolon, ";"),
                (Let, "let"),
                (Ident, "class"),
                (Equal, "="),
                (Ident, "input"),
                (LParen, "("),
                (String, "\"Student's class\""),
                (RParen, ")"),
                (Semicolon, ";"),
                (If, "if"),
                (Ident, "age"),
                (Greater, ">"),
                (Number, "18"),
                (LBrace, "{"),
                (Return, "return"),
                (Ident, "print"),
                (LParen, "("),
                (String, "\"This person is too old\""),
                (RParen, ")"),
                (Semicolon, ";"),
                (RBrace, "}"),
                (If, "if"),
                (Ident, "class"),
                (Dot, "."),
                (Ident, "len"),
                (LParen, "("),
                (RParen, ")"),
                (Greater, ">"),
                (Number, "3"),
                (LBrace, "{"),
                (Return, "return"),
                (Ident, "print"),
                (LParen, "("),
                (String, "\"Invalid class name\""),
                (RParen, ")"),
                (Semicolon, ";"),
                (RBrace, "}"),
                (Ident, "students"),
                (Dot, "."),
                (Ident, "push"),
                (LParen, "("),
                (LParen, "("),
                (Ident, "name"),
                (Comma, ","),
                (Ident, "age"),
                (Comma, ","),
                (Ident, "class"),
                (RParen, ")"),
                (RParen, ")"),
                (Semicolon, ";"),
                (If, "if"),
                (Ident, "input"),
                (LParen, "("),
                (String, "\"Exit? \""),
                (RParen, ")"),
                (LBrace, "{"),
                (Return, "return"),
                (Ident, "print"),
                (LParen, "("),
                (String, "\"Exiting record system\""),
                (RParen, ")"),
                (RBrace, "}"),
                (RBrace, "}"),
            ]
        );
    }

    #[test]
    fn test_number() {
        let tokens = lex("1243 4525 343454 445.3234 69.420 3.14");
        assert_eq!(
            tokens,
            vec![
                (Number, "1243"),
                (Number, "4525"),
                (Number, "343454"),
                (Number, "445.3234"),
                (Number, "69.420"),
                (Number, "3.14"),
            ]
        );
    }

    #[test]
    fn test_string() {
        let tokens = lex(r#"
            "hello" "jan" "opal" "please" "is" "phrase" "lex will ya" "35t8895749754397"
        "#);
        assert_eq!(
            tokens,
            vec![
                (String, r#""hello""#),
                (String, r#""jan""#),
                (String, r#""opal""#),
                (String, r#""please""#),
                (String, r#""is""#),
                (String, r#""phrase""#),
                (String, r#""lex will ya""#),
                (String, r#""35t8895749754397""#),
            ]
        );
    }

    #[test]
    fn test_ident() {
        let tokens = lex("___valid_identifier京 num i a tarzan player o_b_j_e_c_t");
        assert_eq!(
            tokens,
            vec![
                (Ident, "___valid_identifier京"),
                (Ident, "num"),
                (Ident, "i"),
                (Ident, "a"),
                (Ident, "tarzan"),
                (Ident, "player"),
                (Ident, "o_b_j_e_c_t"),
            ]
        );
    }

    #[test]
    fn test_keyword() {
        let tokens = lex("if fn else return while for let notkw true false");
        assert_eq!(
            tokens,
            vec![
                (If, "if"),
                (Fn, "fn"),
                (Else, "else"),
                (Return, "return"),
                (While, "while"),
                (For, "for"),
                (Let, "let"),
                (Ident, "notkw"),
                (Bool, "true"),
                (Bool, "false")
            ]
        );
    }

    #[test]
    fn test_operators() {
        let tokens = lex("+ += - -= * *= / /= = == ! != > >= < <=");
        assert_eq!(
            tokens,
            vec![
                (Plus, "+"),
                (PlusEqual, "+="),
                (Minus, "-"),
                (MinusEqual, "-="),
                (Asterisk, "*"),
                (AsteriskEqual, "*="),
                (Slash, "/"),
                (SlashEqual, "/="),
                (Equal, "="),
                (EqualEqual, "=="),
                (Bang, "!"),
                (BangEqual, "!="),
                (Greater, ">"),
                (GreaterEqual, ">="),
                (Less, "<"),
                (LessEqual, "<="),
            ]
        );
    }

    #[test]
    fn test_other_symbols() {
        let tokens = lex(". .. ;; ,, , })] [({");
        assert_eq!(
            tokens,
            vec![
                (Dot, "."),
                (Dot, "."),
                (Dot, "."),
                (Semicolon, ";"),
                (Semicolon, ";"),
                (Comma, ","),
                (Comma, ","),
                (Comma, ","),
                (RBrace, "}"),
                (RParen, ")"),
                (RBracket, "]"),
                (LBracket, "["),
                (LParen, "("),
                (LBrace, "{"),
            ]
        );
    }
}

