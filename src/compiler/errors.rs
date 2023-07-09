use syn::ItemFn;
use syn::parse_quote;
use syn::visit_mut::VisitMut;

use super::types::RUST_ANXIOUS_TYPES;


pub struct PanicCompilerErrors;

impl VisitMut for PanicCompilerErrors {

    fn visit_item_fn_mut(&mut self, node: &mut ItemFn) {
        match &node.sig.output {
            syn::ReturnType::Default => {
                node.block.stmts.insert(0, 
                    parse_quote!(
                        compiler_error!("non-inline functions must return a value");));
            }
            syn::ReturnType::Type(_, typ) => {
                let error = match *typ.clone() {
                    syn::Type::Path(type_path) => {
                        match type_path.path.get_ident() {
                            Some(ident) => 
                                !RUST_ANXIOUS_TYPES.contains(&ident.to_string()),
                            None => true,
                        }
                    }
                    _ => true,
                };
                if error {
                    node.block.stmts.insert(0, 
                        parse_quote!(
                            compiler_error!("non-inline functions must return an anxious value");));
                }
            }
        }
    }

}