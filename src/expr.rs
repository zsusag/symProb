use crate::syntax::ExprKind;

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Expr {
    root: ExprNode,
    //vars: BTreeSet<String>, // Unsure if I need...
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct ExprNode {
    e: ExprKind,
    children: Vec<ExprNode>,
}

impl Expr {
    pub fn new(root: ExprNode) -> Self {
        Expr { root }
    }
}

impl ExprNode {
    pub fn new(e: ExprKind, children: Vec<ExprNode>) -> Self {
        ExprNode { e, children }
    }

    pub fn new_leaf(e: ExprKind) -> Self {
        ExprNode {
            e,
            children: Vec::new(),
        }
    }

    pub fn get_mut_e(&mut self) -> &mut ExprKind {
        &mut self.e
    }

    pub fn get_e(&self) -> &ExprKind {
        &self.e
    }
}
