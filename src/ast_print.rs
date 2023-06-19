use crate::ast::*;
use crate::token::Token;

pub fn print_stmts(stmts: Vec<Stmt>, source: &str) -> String {
    format!("[{}]", stmts.iter().map(|s| stmt(&s, source)).collect::<Vec<_>>()
    .join(","))
}

fn resolve_token(tok: Token, source: &str) -> String { source[tok.pos as usize..tok.end as usize].replace("\"", "\\\"") }

fn var_decl(node: &VarDecl, source: &str) -> String {
    format!(r#"{{"VarDecl":{{"name":"{}","value":{}}}}}"#, 
        resolve_token(node.name.0, source),
        node.value.as_ref().map(|n| expr(&n, source)).unwrap_or(String::from("null"))
    )
}

fn func_decl(node: &FuncDecl, source: &str) -> String {
    format!(r#"{{"FuncDecl":{{"name":"{}","params":{:?},"body":{}}}}}"#,
        resolve_token(node.name.0, source),
        node.params.iter().map(|n| resolve_token(n.0, source))
            .collect::<Vec<_>>(),
        block_stmt(&node.body, source)
    )
}

fn block_stmt(node: &BlockStmt, source: &str) -> String { 
    format!(r#"[{}]"#, 
        node.body.iter().map(|n| stmt(&n, source))
        .collect::<Vec<_>>().join(",")
    )
}

fn expr_stmt(node: &ExprStmt, source: &str) -> String { 
    format!(r#"{{"ExprStmt":{}}}"#, 
        expr(&node.expr, source)
    )
}

fn empty_stmt(node: &EmptyStmt, source: &str) -> String {
    format!(r#"{{"EmptyStmt":"{}"}}"#,
        resolve_token(node.0, source)
    )
}

fn infix_expr(node: &Infix, source: &str) -> String {
    format!(r#"{{"Infix":{{"left":{},"op":"{}","right":{}}}}}"#,
        expr(&node.left, source),
        resolve_token(node.op, source),
        expr(&node.right, source),
    )
}

fn prefix_expr(node: &Prefix, source: &str) -> String {
    format!(r#"{{"Prefix":{{"op":"{}","right":{}}}}}"#,
        resolve_token(node.op, source),
        expr(&node.right, source),
    )
}

fn if_expr(node: &IfExpr, source: &str) -> String {
    format!(r#"{{"IfExpr":{{"condition":{},"consequence":{},"alternate":{}}}}}"#,
        expr(&node.condition, source),
        block_expr(&node.consequence, source),
        match &node.alternate {
            IfAlt::Else(inner) => block_expr(inner, source),
            IfAlt::ElseIf(inner) => if_expr(inner, source),
            IfAlt::Null => String::from("null")
        }
    )
}

fn while_expr(node: &WhileExpr, source: &str) -> String {
    format!(r#"{{"WhileExpr":{{"condition":{},"consequence":{}}}}}"#,
        expr(&node.condition, source),
        block_expr(&node.consequence, source)
    )
}

fn return_expr(node: &ReturnExpr, source: &str) -> String {
    format!(r#"{{"ReturnExpr":{{"value":{}}}}}"#, 
        node.value.as_ref().map(|n| expr(&n, source)).unwrap_or(String::from("null"))
    )
}

fn assign_expr(node: &AssignExpr, source: &str) -> String {
    format!(r#"{{"AssignExpr":{{"ident":"{}","value":{}}}}}"#,
        resolve_token(node.ident.0, source),
        expr(&node.value, source)
    )
}

fn call_expr(node: &CallExpr, source: &str) -> String {
    format!(r#"{{"CallExpr":{{"ident":"{}","args":[{}]}}}}"#,
        resolve_token(node.name.0, source),
        node.args.iter().map(|n| expr(&n, source))
            .collect::<Vec<_>>()
            .join(",")
    )
}

fn array_expr(node: &ArrayExpr, source: &str) -> String {
    format!(r#"{{"ArrayExpr":{{"items":[{}]}}}}"#,
        node.items.iter().map(|n| expr(&n, source))
            .collect::<Vec<_>>()
            .join(",")
    )
}

fn break_expr(node: &BreakExpr, source: &str) -> String {
    format!(r#"{{"BreakExpr":{{"value":{}}}}}"#, 
        node.value.as_ref().map(|n| expr(&n, source)).unwrap_or(String::from("null"))
    )
}

fn block_expr(node: &BlockExpr, source: &str) -> String { 
    format!(r#"[{}]"#, 
        node.body.iter().map(|n| stmt(&n, source))
        .collect::<Vec<_>>().join(",")
    )
}
fn stmt(node: &Stmt, source: &str) -> String {  
    match node {
        Stmt::VarDecl(inner) => var_decl(&inner, source),
        Stmt::FuncDecl(inner) => func_decl(&inner, source),
        Stmt::Block(inner) => block_stmt(&inner, source),
        Stmt::Expr(inner) => format!(r#"{{"ExprStmt":{}}}"#, expr(&inner.expr, source)),
        Stmt::Empty(inner) => empty_stmt(&inner, source),
        Stmt::Null => format!(r#""null""#)
    }
}
fn expr(node: &Expr, source: &str) -> String { 
    match node {
        Expr::Id(Ident(tok)) |
        Expr::Int(Int(tok)) |
        Expr::Str(Str(tok)) |
        Expr::Bool(Bool(tok)) => format!(r#""{}""#, resolve_token(*tok, source)),

        Expr::Infix(inner) => infix_expr(&inner, source),
        Expr::Prefix(inner) => prefix_expr(&inner, source),
        Expr::Group(inner) => format!(r#"{{"GroupExpr":{}}}"#, expr(&inner.0, source)),
        Expr::Block(inner) => block_expr(&inner, source),
        Expr::If(inner) => if_expr(&inner, source),
        Expr::While(inner) => while_expr(&inner, source),
        Expr::Return(inner) => return_expr(&inner, source),
        Expr::Assign(inner) => assign_expr(&inner, source),
        Expr::Call(inner) => call_expr(&inner, source),
        Expr::Array(inner) => array_expr(&inner, source),
        Expr::Break(inner) => break_expr(&inner, source),
        Expr::Null => format!(r#""null""#)
    }
}



// fn stringify_ast(top_nodes: Vec<Stmt>, source: &str) -> String {
//     let mut result = String::from('[');
//     for node in top_nodes.iter() {
//         match node {
//             Stmt::VarDecl(inner) => {
//                 write!(&mut result, r#"{{VarDecl: {{name:"{}",value:"{}""#, 
//                 inner.name.0,
//                 inner.value).unwrap();
//             },
//             Stmt::FuncDecl(inner) => todo!(),
//             Stmt::Block(inner) => todo!(),
//             Stmt::Expr(inner) => todo!(),
//             Stmt::Empty(inner) => todo!(),
//         }
//     }
//     result.push(']');
// }