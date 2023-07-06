use crate::{token::{Tag, Token}};
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
pub enum Node<'ast> {
    VarDecl(&'ast VarDecl),
    FuncDecl(&'ast FuncDecl),
    BlockStmt(BlockStmt),
    ExprStmt(ExprStmt),
    Empty(EmptyStmt),

    Id(Ident),
    Str(Str),
    Bool(Bool),
    Int(Int),
    Infix(&'ast Infix),
    Prefix(&'ast Prefix),
    Group(&'ast Group),
    Block(BlockExpr),
    If(&'ast IfExpr),
    While(&'ast WhileExpr),
    Return(&'ast ReturnExpr),
    Assign(&'ast AssignExpr),
    Call(&'ast CallExpr),
    Array(&'ast ArrayExpr),
    Break(&'ast BreakExpr),
    
    ParamList(&'ast [Ident]),

    Null
}

#[derive(Debug, Clone)]
pub enum Stmt {
    VarDecl(Box<VarDecl>),
    FuncDecl(Box<FuncDecl>),
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

impl<'ast> From<&'ast Expr> for Node<'ast> {
    fn from(expr: &'ast Expr) -> Self {
        use Expr::*;
        match expr {
            Id(inner) => Self::Id(inner.clone()),
            Str(inner) => Self::Str(inner.clone()),
            Bool(inner) => Self::Bool(inner.clone()),
            Int(inner) => Self::Int(inner.clone()),
            Infix(inner) => Self::Infix(inner.as_ref()),
            Prefix(inner) => Self::Prefix(inner.as_ref()),
            Group(inner) => Self::Group(inner.as_ref()),
            Block(inner) => Self::Block(inner.clone()),
            If(inner) => Self::If(inner.as_ref()),
            While(inner) => Self::While(inner.as_ref()),
            Return(inner) => Self::Return(inner.as_ref()),
            Assign(inner) => Self::Assign(inner.as_ref()),
            Call(inner) => Self::Call(inner.as_ref()),
            Array(inner) => Self::Array(inner.as_ref()),
            Break(inner) => Self::Break(inner.as_ref()),
            Null => Self::Null
        }
    }
}

impl<'ast> From<&'ast Stmt> for Node<'ast> {
    fn from(stmt: &'ast Stmt) -> Self {
        use Stmt::*;
        match stmt {
            VarDecl(inner) => Self::VarDecl(inner.as_ref()),
            FuncDecl(inner) => Self::FuncDecl(inner.as_ref()),
            Block(inner) => Self::BlockStmt(inner.clone()),
            Expr(inner) => Self::ExprStmt(inner.clone()),
            Empty(inner) => Self::Empty(inner.clone()),
            Null => Self::Null
        }
    }
}

impl<'ast> From<Option<&'ast Expr>> for Node<'ast> { 
    fn from(value: Option<&'ast Expr>) -> Self {
        value.map_or(Node::Null, Node::from)
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

