use crate::token::Token;
use crate::ast::*;

// fn visit(tree: &[Stmt], source: &str, visitor: ()) {
//     // TODO: Use some statistics to figure out a good starting capacity
//     let mut stack = Vec::<Node>::with_capacity(20);
//     // Sigh: Too many clones
//     stack.extend(tree.into_iter().cloned().map(Node::from));

//     while stack.len() > 0 {
//         let node = stack.pop().unwrap();

//         match node {
//             Node::VarDecl(inner) => stack.extend_from_slice(&[
//                 Node::from(inner.value),
//                 Node::Id(inner.name),
//             ]),
//             Node::FuncDecl(inner) => stack.extend_from_slice(&[
//                 Node::BlockStmt(inner.body),
//                 Node::from(inner.params)
//             ]),
//             Node::Block(inner) => todo!(),
//             Node::Expr(inner) => todo!(),
//             Node::Empty(inner) => todo!(),
//         }

//     }
// }