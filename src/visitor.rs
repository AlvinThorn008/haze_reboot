use crate::token::Token;
use crate::ast::*;

#[allow(unused)]
trait Visit {

    fn visit_statement(&mut self, stmt: &Stmt) {}
    fn visit_func_decl(&mut self, node: &FuncDecl) {}
    fn visit_var_decl(&mut self, node: &VarDecl) {}
    fn visit_block_stmt(&mut self, node: &BlockStmt) {}
    fn visit_empty_stmt(&mut self, node: &EmptyStmt) {}
    fn visit_expr_stmt(&mut self, node: &ExprStmt) {}

    fn visit_ident(&mut self, node: &Ident) {}
    fn visit_string(&mut self, node: &Str) {}
    fn visit_bool(&mut self, node: &Bool) {}
    fn visit_int(&mut self, node: &Int) {}
    fn visit_infix(&mut self, node: &Infix) {}
    fn visit_prefix(&mut self, node: &Prefix) {}

    fn visit_group(&mut self, node: &Group) {}
    fn visit_block_expr(&mut self, node: &BlockExpr) {}
    fn visit_if_expr(&mut self, node: &IfExpr) {}
    fn visit_while_expr(&mut self, node: &WhileExpr) {}
    fn visit_return_expr(&mut self, node: &ReturnExpr) {}
    fn visit_assign_expr(&mut self, node: &AssignExpr) {}
    fn visit_call_expr(&mut self, node: &CallExpr) {}
    fn visit_break_expr(&mut self, node: &BreakExpr) {}

    fn visit_param_list(&mut self, node: &[Ident]) {}
    fn visit_stmt_list(&mut self, node: &[Stmt]) {}
}

trait VisitChildren : Visit {
    fn visit_children(&mut self, node: Self) where Self: Sized {
        
    }
}

fn visit(tree: &[Stmt], source: &str, mut visitor: impl Visit) {
    // TODO: Use some statistics to figure out a good starting capacity
    let mut stack = Vec::<Node>::with_capacity(20);
    // Sigh: Too many clones
    stack.extend(tree.into_iter().map(|stmt| Node::from(stmt)));

    while stack.len() > 0 {
        let node = stack.pop().unwrap();

        match node {
            Node::VarDecl(inner) => {
                visitor.visit_var_decl(inner);
                stack.extend_from_slice(&[
                    Node::from(inner.value.as_ref()),
                    Node::Id(inner.name)
                ]);
            }
            Node::FuncDecl(inner) => {
                visitor.visit_func_decl(inner);
                stack.extend_from_slice(&[
                    Node::BlockStmt(inner.body),
                    Node::ParamList(&inner.params),
                    Node::Id(inner.name)
                ]);
            }
            Node::BlockStmt(inner) => {
                visitor.visit_block_stmt(&inner);
                stack.extend(inner.body
                    .into_iter().rev()
                    .map(Node::from)
                );
            }
            Node::ExprStmt(inner) => {
                visitor.visit_expr_stmt(&inner);
                stack.push(Node::from(&inner.expr));
            }
            Node::Empty(inner) => {
                visitor.visit_empty_stmt(&inner);
                // No need to put token on stack
            },
            Node::Id(inner) => visitor.visit_ident(&inner),
            Node::Str(inner) => visitor.visit_string(&inner),
            Node::Bool(inner) => visitor.visit_bool(&inner),
            Node::Int(inner) => visitor.visit_int(&inner),
            Node::Infix(inner) => visitor.visit_infix(&inner),
            Node::Prefix(inner) => visitor.visit_prefix(&inner),
            Node::Group(inner) => {
                visitor.visit_group(&inner);
                stack.push(Node::from(&inner.0))
            }
            Node::Block(inner) => {
                visitor.visit_block_expr(&inner);
                stack.extend(inner.body
                    .into_iter().rev()
                    .map(Node::from)
                );
            }
            Node::If(inner) => {
                visitor.visit_if_expr(&inner);
                stack.extend_from_slice(&[
                    Node::IfAlt(inner.alternate),
                    Node::BlockExpr(inner.consequence),
                    Node::from(inner.condition)
                ]);
            }
            Node::IfAlt(_inner) => {} // forgo
            Node::While(inner) => {
                visitor.visit_while_expr(&inner);
                stack.extend_from_slice(&[
                    Node::BlockExpr(inner.consequence),
                    Node::from(inner.condition)
                ]);
            }
            Node::Return(inner) => {
                visitor.visit_return_expr(&inner);
                stack.push(Node::from(inner.value));
            }
            Node::Assign(inner) => {
                visitor.visit_assign_expr(&inner);
                stack.extend_from_slice(&[
                    Node::from(inner.value),
                    Node::Id(inner.ident)
                ]);
            }
            Node::Call(inner) => {
                visitor.visit_call_expr(&inner);
                // stack.extend_from_slice(&[
                //     //Node::ParamList(inner.args),
                //     Node::Id(inner.name)
                // ]);
            }
            Node::Break(inner) => {
                visitor.visit_break_expr(&inner);
                stack.push(Node::from(inner.value));
            }

            _ => {}
        }

    }
}

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