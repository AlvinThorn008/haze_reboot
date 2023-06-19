use crate::lexer::Lexer;
use crate::token::{Tag, Token};
use crate::ast::*;

/// Advance the lexer and return the expr
macro_rules! next_and_return {
    ($parser:ident, $exp:expr) => {{
        $parser.next();
        $exp
    }};
}

/// Advance the lexer without calling next
macro_rules! commit {
    ($parser:ident, $tok:ident) => {
        $parser.tokens.offset += $tok.value.len() as u32
    };
}

/// The Haze Parser
/// 
/// # Examples
pub struct Parser<'a> {
    /// Token stream created from lexing a source file/string
    pub(crate) tokens: Lexer<'a>
}

pub type Program = Vec<Stmt>;
type InfixParser = for<'a> fn(&mut Parser<'a>) -> Result<Expr, &'static str>;

impl<'a> Parser<'a> {
    /// Constructs a new parser given a source string
    /// 
    /// # Examples
    /// ```
    /// let bump = bumpalo::Bump::new();
    /// let mut parser = Parser::new("
    /// let g = 1 * 2 + 5;
    /// ", &bump);
    /// let tree = parser.parse();
    /// ```
    pub fn new(source: &'a str) -> Self {
        Self {
            tokens: Lexer::from(source)
        }
    }

    /// Produces a AST 
    pub fn parse(&mut self) -> Result<Program, &'static str> {
        let mut program = Program::new();

        while let Some(t) = self.peek() {
            let item = self.parse_statement()?;
            program.push(item);
        }

        return Ok(program);
    }

    fn parse_statement(&mut self) -> Result<Stmt, &'static str> {
        let tok = self.peek().expect("self.tokens shouldn't be consumed");

        match tok.tag {
            Tag::Fn => Ok(Stmt::FuncDecl(self.parse_func_decl()?)),
            Tag::Let => Ok(Stmt::VarDecl(self.parse_var_decl()?)),
            Tag::Semicolon => next_and_return!(self, Ok(Stmt::Empty(EmptyStmt(tok)))),
            _ => Ok(Stmt::Expr(self.parse_expr_stmt()?)),
        }
    }

/*     fn parse_item(&mut self) -> Result<Item, &'static str> {
        let tok = self.peek().expect("self.tokens shouldn't be consumed");

        match tok.tag {
            Tag::Fn => Ok(Item::FuncDecl(Box::new_in(self.bump, self.parse_func_decl()?))),
            _ => Err("Not an item")
        }
    } */

    fn parse_func_decl(&mut self) -> Result<FuncDecl, &'static str> {
        let _ = self.next().expect("self.tokens shouldn't be consumed"); // consume`fn` keyword
        let name = self
            .eat_token(Tag::Ident)
            .map(Ident)
            .ok_or("Identifier expected")?;
        let mut params: Vec<Ident> = Vec::new();
        self.eat_token(Tag::LParen).ok_or("Left paren expected")?;

        // Early return for functions without parameters
        match self.eat_token(Tag::RParen) {
            Some(_) => {
                return Ok(FuncDecl {
                    name,
                    params: params.into_boxed_slice(),
                    body: self.parse_block_stmt()?,
                })
            }
            None => {}
        };

        // Parameter parsing
        loop {
            params.push(
                self.eat_token(Tag::Ident)
                    .map(Ident)
                    .ok_or("Missing identifier")?,
            );

            // RParen or Comma is expected after an ident
            match self.peek() {
                Some(tok) if tok.tag == Tag::RParen => {
                    self.next();
                    break;
                }
                Some(tok) if tok.tag == Tag::Comma => {
                    self.next();
                }
                _ => return Err("Expected right paren or comma"),
            }
        }

        Ok(FuncDecl {
            name,
            params: params.into_boxed_slice(),
            body: self.parse_block_stmt()?,
        })
    }

    fn parse_var_decl(&mut self) -> Result<VarDecl, &'static str> {
        let _ = self.next().expect("self.tokens shouldn't be consumed"); // consume let keyword
        let name = self
            .eat_token(Tag::Ident)
            .map(Ident)
            .ok_or("Identifier expected")?;

        match self.peek() {
            Some(tok) if tok.tag == Tag::Equal => {
                self.next();
            }
            Some(tok) if tok.tag == Tag::Semicolon => {
                self.next();
                return Ok(VarDecl { name, value: None }); // uninitialized var decl
            }
            _ => return Err("Unexpected token"),
        };

        let value = Some(self.parse_expr()?);

        self.eat_token(Tag::Semicolon).ok_or("Expected `;`")?;

        Ok(VarDecl { name, value })
    }

    fn parse_expr_stmt(&mut self) -> Result<ExprStmt, &'static str> {
        let expr = self.parse_expr()?;
        match expr {
            Expr::If(_) | Expr::While(_) | Expr::Block(_) => self.lazy_eat(Tag::Semicolon),
            _ => { self.eat_token(Tag::Semicolon).ok_or("Expected semicolon after expression without block")?; }
        };
        
        Ok(ExprStmt { expr })
    }

    fn parse_block_stmt(&mut self) -> Result<BlockStmt, &'static str> {
        self.parse_block_expr()
            .map(|expr| BlockStmt { body: expr.body })
    }

    fn parse_block_expr(&mut self) -> Result<BlockExpr, &'static str> {
        self.eat_token(Tag::LBrace).ok_or("Expected `{`")?;

        let mut stmts = Vec::new();

        loop {
            match self.peek() {
                Some(tok) if tok.tag == Tag::RBrace => {
                    self.next();
                    break;
                }
                tok => {
                    stmts.push(self.parse_statement()?);
                }
                None => return Err("RBrace not found"),
            }
        }

        Ok(BlockExpr { body: stmts.into_boxed_slice() })
    }

    fn parse_expr(&mut self) -> Result<Expr, &'static str> {
        self.parse_expr_bp(0)
    }

    fn parse_if_expr(&mut self) -> Result<IfExpr, &'static str> {
        let _ = self.next().expect("self.tokens shouldn't be consumed"); // if keyword
        let condition = self.parse_expr()?;
        let consequence = self.parse_block_expr()?;
        let mut alternate: IfAlt = IfAlt::Null;

        // null alternate if no else token follows
        match self.peek() {
            Some(tok) if tok.tag == Tag::Else => {
                self.next();
            }
            _ => {
                return Ok(IfExpr {
                    condition,
                    consequence,
                    alternate,
                })
            }
        };

        match self.peek() {
            Some(tok) if tok.tag == Tag::If => {
                // Parse else if
                alternate = IfAlt::ElseIf(Box::new(self.parse_if_expr()?));
            }
            Some(tok) if tok.tag == Tag::LBrace => {
                // Parse else block
                alternate = IfAlt::Else(self.parse_block_expr()?);
            }
            _ => return Err("Abort!!! brace missing - ifs lost"),
        };

        Ok(IfExpr {
            condition,
            consequence,
            alternate,
        })
    }

    fn parse_while_expr(&mut self) -> Result<WhileExpr, &'static str> {
        let _ = self.next().expect("self.tokens shouldn't be consumed");
        let condition = self.parse_expr()?;
        let consequence = self.parse_block_expr()?;

        Ok(WhileExpr {
            condition,
            consequence,
        })
    }

    fn parse_return_expr(&mut self) -> Result<ReturnExpr, &'static str> {
        let _ = self.next().expect("self.tokens shouldn't be consumed"); // consume return keyword

        match self.peek() {
            Some(tok) if matches!(tok.tag, Tag::Semicolon | Tag::RBrace) => {
                Ok(ReturnExpr { value: None })
            }
            Some(tok) => Ok(ReturnExpr {
                value: Some(self.parse_expr()?),
            }),
            None => Err("Expected `:`, `}` or an operator"),
        }
    }

    fn parse_break_expr(&mut self) -> Result<BreakExpr, &'static str> {
        let _ = self.next().expect("self.tokens shouldn't be consumed"); // consume return keyword

        match self.peek() {
            Some(tok) if matches!(tok.tag, Tag::Semicolon | Tag::RBrace) => {
                Ok(BreakExpr { value: None })
            }
            Some(_) => Ok(BreakExpr {
                value: Some(self.parse_expr()?),
            }),
            None => Err("Expected `:`, `}` or an operator"),
        }
    }

    // Core expression parser based on Pratt parsing
    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Expr, &'static str> {
        let mut lhs = self.parse_prefix()?;

        loop {
            let op = match self.peek() {
                Some(t) if tag_is_binop(t.tag) => t,
                _ => break,
            };

            let (l_bp, r_bp) = expr::infix_bp(op.tag);

            if l_bp < min_bp {
                break;
            }

            self.next();
            let rhs = self.parse_expr_bp(r_bp)?;

            lhs = Expr::Infix(Box::new(Infix {
                left: lhs,
                op,
                right: rhs,
            }));
        }
        Ok(lhs)
    }

    fn parse_postfix(&mut self, tag: Tag) -> Option<(u8, InfixParser)> {

        macro_rules! infix_expr {
            ($self:ident,$bp:literal) => {{
                ($bp, |self_| { self_.parse_expr_bp($bp + 1) })
            }};
        }

        // Notice how the binding powers are consecutive odd numbers. 
        // This is because the right binding power of any infix operator is
        // its left binding power plus 1. This prevents clashing.
        // e.g. + and - have a left bp of 1 and a right bp of 2
        match tag {
            Tag::Minus | Tag::Plus => Some(infix_expr!(self, 1)),
            Tag::Slash | Tag::Asterisk => Some(infix_expr!(self, 3)),
            Tag::BangEqual
            | Tag::Greater
            | Tag::GreaterEqual
            | Tag::Less
            | Tag::LessEqual
            | Tag::EqualEqual => Some(infix_expr!(self, 5)),
            _ => None
        }
    }

    fn parse_prefix(&mut self) -> Result<Expr, &'static str> {
        let tok = self.peek().expect("self.tokens should not be consumed");
        match tok.tag {
            Tag::Ident => {
                self.next(); // ident
                let next_tok = if let Some(tok) = self.peek() { tok } else { return Ok(Expr::Id(Ident(tok))); };
                match next_tok.tag {
                    Tag::Equal => self.consume_assignment(tok),
                    Tag::LParen => self.consume_call_expr(tok),
                    _ => Ok(Expr::Id(Ident(tok)))
                }
            },
            Tag::String => next_and_return!(self, Ok(Expr::Str(Str(tok)))),
            Tag::Bool => next_and_return!(self, Ok(Expr::Bool(Bool(tok)))),
            Tag::Number => next_and_return!(self, Ok(Expr::Int(Int(tok)))),
            tag if tag_is_unaryop(tag) => {
                self.next();
                let ((), _r_bp) = expr::prefix_bp(tag);
                let rhs = self.parse_expr()?;
                Ok(Expr::Prefix(Box::new(Prefix {
                    op: tok,
                    right: rhs,
                })))
            }
            Tag::LParen => {
                self.next();
                let lhs = self.parse_expr()?;
                self.eat_token(Tag::RParen)
                    .ok_or("Missing closing parenthesis")?;
                Ok(Expr::Group(Box::new(Group(lhs))))
            }
            Tag::LBracket => self.consume_array_expr(),
            Tag::If => Ok(Expr::If(Box::new(self.parse_if_expr()?))),
            Tag::While => Ok(Expr::While(Box::new(self.parse_while_expr()?))),
            Tag::Return => Ok(Expr::Return(Box::new(self.parse_return_expr()?))),
            Tag::LBrace => Ok(Expr::Block(self.parse_block_expr()?)),
            Tag::Break => Ok(Expr::Break(Box::new(self.parse_break_expr()?))),
            _ => {
                return Err("Expected expression")
            }
        }
    }

    fn consume_assignment(&mut self, tok: Token) -> Result<Expr, &'static str> {
        self.next();
        Ok(Expr::Assign(Box::new(AssignExpr { 
            ident: Ident(tok), 
            value: self.parse_expr()? 
        })))
    }

    fn consume_call_expr(&mut self, tok: Token) -> Result<Expr, &'static str> {
        self.next(); // LParen
        let mut args = Vec::new();

        while self.eat_token(Tag::RParen).is_none() {
            let arg = match self.parse_expr() {
                Ok(expr) => expr,
                Err(_) => return Err("Expected Identifier or RParen")
            };

            args.push(arg);

            match self.peek() {
                Some(tok) if tok.tag == Tag::Comma => {
                    self.next();
                }
                Some(tok) if tok.tag == Tag::RParen => {
                    self.next();
                    break;
                }
                _ => {}
            }
        }
        Ok(Expr::Call(Box::new(CallExpr { 
            name: Ident(tok), 
            args: args.into_boxed_slice()
        })))
    }

    fn consume_array_expr(&mut self) -> Result<Expr, &'static str> {
        self.next(); // LParen
        let mut items = Vec::new();

        while self.eat_token(Tag::RBracket).is_none() {
            let item = match self.parse_expr() {
                Ok(expr) => expr,
                Err(_) => return Err("Expected Identifier or RParen")
            };

            items.push(item);

            match self.peek() {
                Some(tok) if tok.tag == Tag::Comma => {
                    self.next();
                }
                Some(tok) if tok.tag == Tag::RBracket => {
                    self.next();
                    break;
                }
                _ => {}
            }
        }
        Ok(Expr::Array(Box::new(ArrayExpr { 
            items: items.into_boxed_slice()
        })))
    }

    pub fn lazy_eat(&mut self, tag: Tag) {
        match self.peek() {
            Some(tok) if tok.tag == tag => {
                self.next();
            }
            _ => {}
        };
    }

    fn peek(& self) -> Option<Token> {
        self.tokens.clone().next()
    }

    fn eat_token(&mut self, token_tag: Tag) -> Option<Token> {
        (self.peek()?.tag == token_tag).then(|| self.tokens.next().unwrap())
    }

    fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }
}

mod expr {
    use super::Tag;

    pub fn infix_bp(tag: Tag) -> (u8, u8) {
        match tag {
            Tag::Minus | Tag::Plus => (1, 2),
            Tag::Slash | Tag::Asterisk => (3, 4),
            Tag::BangEqual
            | Tag::Greater
            | Tag::GreaterEqual
            | Tag::Less
            | Tag::LessEqual
            | Tag::EqualEqual => (5, 6),
            Tag::Dot => (8, 9),
            _ => panic!("tag should be a binary operator"),
        }
    }

    pub fn prefix_bp(tag: Tag) -> ((), u8) {
        match tag {
            Tag::Minus | Tag::Bang => ((), 7),
            _ => panic!("tag should be an unary operator"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;

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

    // fn bench_parser(b: &mut test::Bencher) {
    //     let bump = Bump::new();
    //     let parser = Parser::new("", &bump);

    //     b.bench(|| {
            
    //     })
    // }
}