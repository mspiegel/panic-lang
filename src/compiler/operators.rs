use syn::ExprBinary;
use syn::parse_quote;
use syn::visit_mut::VisitMut;

pub struct PanicToRustOperators;

impl VisitMut for PanicToRustOperators {
    fn visit_expr_binary_mut(&mut self, node: &mut ExprBinary) {
        self.visit_expr_mut(&mut node.left);
        self.visit_expr_mut(&mut node.right);
        match node.op {
            syn::BinOp::Add(_) |
            syn::BinOp::Sub(_) |
            syn::BinOp::Mul(_) |
            syn::BinOp::Div(_) |
            syn::BinOp::Rem(_) => {
                let lhs = &node.left;
                let op = node.op;
                let rhs = &node.right;
                *node = parse_quote!(ack!(#lhs) #op ack!(#rhs));
            }
            syn::BinOp::AddAssign(_) |
            syn::BinOp::SubAssign(_) |
            syn::BinOp::MulAssign(_) |
            syn::BinOp::DivAssign(_) |
            syn::BinOp::RemAssign(_) => {
                let lhs = &node.left;
                let op = node.op;
                let rhs = &node.right;
                *node = parse_quote!(#lhs #op ack!(#rhs));
            }
            _ => { }
        }
    }
}