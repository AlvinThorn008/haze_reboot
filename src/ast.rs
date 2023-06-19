use core::fmt;

use crate::{token::{Tag, Token}, lexer::Lexer};
// use std::convert::From;

pub fn tag_is_binop(tag: Tag) -> bool {
    match tag {
        Tag::Plus
        | Tag::Minus
        | Tag::Slash
        | Tag::Asterisk
        | Tag::Dot
        | Tag::Bang
        | Tag::BangEqual
        | Tag::Equal
        | Tag::EqualEqual
        | Tag::Greater
        | Tag::GreaterEqual
        | Tag::Less
        | Tag::LessEqual => true,

        _ => false,
    }
}

pub fn tag_is_unaryop(tag: Tag) -> bool {
    match tag {
        Tag::Minus | Tag::Bang => true,
        _ => false,
    }
}

pub fn tag_is_literal(tag: Tag) -> bool {
    match tag {
        Tag::Ident | Tag::String | Tag::Number | Tag::Bool => true,
        _ => false,
    }
}

// pub struct SourceAst<'s, 'b> {
//     pub(crate) source: &'s str,
//     pub(crate) nodes: Vec<'b, Node<'b>>,
// }

#[derive(Debug, Clone)]
pub enum Node {
    VarDecl(VarDecl),
    FuncDecl(FuncDecl),
    BlockStmt(BlockStmt),
    Expr(ExprStmt),
    EmptyStmt(EmptyStmt),

    Id(Ident),
    Str(Str),
    Bool(Bool),
    Int(Int),
    Infix(Infix),
    Prefix(Prefix),
    Group(Group),
    BlockExpr(BlockExpr),
    If(IfExpr),
    While(WhileExpr),
    Return(ReturnExpr),
    Assign(AssignExpr),
    Call(CallExpr),
    Array(ArrayExpr),
    Break(BreakExpr),
    
    ParamList(Box<Ident>),

    Null
}

#[derive(Debug, Clone)]
pub enum Stmt {
    VarDecl(VarDecl),
    FuncDecl(FuncDecl),
    Block(BlockStmt),
    Expr(ExprStmt),
    Empty(EmptyStmt),
    Null
}

#[derive(Debug, Clone)]
pub enum Expr {
    Id(Ident),
    Str(Str),
    Bool(Bool),
    Int(Int),
    Infix(Box<Infix>),
    Prefix(Box<Prefix>),
    Group(Box<Group>),
    Block(BlockExpr),
    If(Box<IfExpr>),
    While(Box<WhileExpr>),
    Return(Box<ReturnExpr>),
    Assign(Box<AssignExpr>),
    Call(Box<CallExpr>),
    Array(Box<ArrayExpr>),
    Break(Box<BreakExpr>),
    Null
}

impl From<Expr> for Node {
    fn from(expr: Expr) -> Self {
        use Expr::*;
        match expr {
            Id(inner) => Self::Id(inner),
            Str(inner) => Self::Str(inner),
            Bool(inner) => Self::Bool(inner),
            Int(inner) => Self::Int(inner),
            Infix(inner) => Self::Infix(*inner),
            Prefix(inner) => Self::Prefix(*inner),
            Group(inner) => Self::Group(*inner),
            Block(inner) => Self::BlockExpr(inner),
            If(inner) => Self::If(*inner),
            While(inner) => Self::While(*inner),
            Return(inner) => Self::Return(*inner),
            Assign(inner) => Self::Assign(*inner),
            Call(inner) => Self::Call(*inner),
            Array(inner) => Self::Array(*inner),
            Break(inner) => Self::Break(*inner),
            Null => Self::Null
        }
    }
}

impl From<Stmt> for Node {
    fn from(stmt: Stmt) -> Self {
        use Stmt::*;
        match stmt {
            VarDecl(inner) => Self::VarDecl(inner),
            FuncDecl(inner) => Self::FuncDecl(inner),
            Block(inner) => Self::BlockStmt(inner),
            Expr(inner) => Self::Expr(inner),
            Empty(inner) => Self::EmptyStmt(inner),
            Null => Self::Null
        }
    }
}

impl From<Option<Expr>> for Node { 
    fn from(value: Option<Expr>) -> Self {
        value.map_or(Node::Null, |x| Node::from(x))
    } 
}

#[derive(Debug, Clone)]
pub struct Ident(pub Token);
#[derive(Debug, Clone)]
pub struct Str(pub Token);
#[derive(Debug, Clone)]
pub struct Bool(pub Token);
#[derive(Debug, Clone)]
pub struct Int(pub Token);
#[derive(Debug, Clone)]
pub struct Group(pub Expr);
#[derive(Debug, Clone)]
pub struct Infix {
    pub left: Expr,
    pub op: Token,
    pub right: Expr,
}
#[derive(Debug, Clone)]
pub struct Prefix {
    pub op: Token,
    pub right: Expr,
}
#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub body: Box<[Stmt]>,
}
#[derive(Debug, Clone)]
pub struct IfExpr {
    pub condition: Expr,
    pub consequence: BlockExpr,
    pub alternate: IfAlt,
}

#[derive(Debug, Clone)]
pub enum IfAlt {
    ElseIf(Box<IfExpr>),
    Else(BlockExpr),
    Null
}
#[derive(Debug, Clone)]
pub struct WhileExpr {
    pub condition: Expr,
    pub consequence: BlockExpr,
}
#[derive(Debug, Clone)]
pub struct ReturnExpr {
    pub value: Option<Expr>,
}
#[derive(Debug, Clone)] 
pub struct AssignExpr {
    pub ident: Ident,
    pub value: Expr
}
#[derive(Debug, Clone)] 
pub struct CallExpr {
    pub name: Ident,
    pub args: Box<[Expr]>,
}
#[derive(Debug, Clone)]
pub struct ArrayExpr {
    pub items: Box<[Expr]>
}
#[derive(Debug, Clone)]
pub struct BreakExpr {
    pub value: Option<Expr>,
}
#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: Ident,
    pub params: Box<[Ident]>,
    pub body: BlockStmt,
}
#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub body: Box<[Stmt]>,
}
#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: Ident,
    pub value: Option<Expr>,
}
#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub expr: Expr,
}
#[derive(Debug, Clone)]
pub struct EmptyStmt(pub Token);

