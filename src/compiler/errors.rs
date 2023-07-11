use syn::ItemFn;
use syn::parse_quote;
use syn::visit_mut::VisitMut;

use super::types::is_anxious;


pub struct PanicCompilerErrors;

impl VisitMut for PanicCompilerErrors {

    fn visit_item_fn_mut(&mut self, node: &mut ItemFn) {
        if "main" == node.sig.ident.to_string() {
            return
        }
        match &node.sig.output {
            syn::ReturnType::Default => {
                node.block.stmts.insert(0, 
                    parse_quote!(
                        compiler_error!("non-inline functions must return a value");));
            }
            syn::ReturnType::Type(_, typ) => {
                if !is_anxious(typ) {
                    node.block.stmts.insert(0, 
                        parse_quote!(
                            compiler_error!("non-inline functions must return an anxious value");));
                }
            }
        }
    }

}