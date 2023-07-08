#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;

use quote::quote;
use syn::visit_mut::VisitMut;
use syn::File;
use syn::Ident;
use syn::PathSegment;
use syn::Type;

mod types;

struct PanicToRustTypes;

lazy_static! {
    static ref PANIC_TO_RUST_TYPES: HashMap<String, String> = HashMap::from([
        ("i32".to_string(), "Int32".to_string()),
        ("bool".to_string(), "Bool".to_string()),
        ("()".to_string(), "Unit".to_string()),
        ("nom_i32".to_string(), "i32".to_string()),
        ("nom_bool".to_string(), "bool".to_string()),
        ("nom_()".to_string(), "()".to_string()),
    ]);
}

impl VisitMut for PanicToRustTypes {
    fn visit_type_mut(&mut self, node: &mut Type) {
        if let Type::Path(ref mut type_path) = node {
            let path = &mut type_path.path;
            let ident = path.get_ident();
            if let Some(ident) = ident {
                let span = ident.span();
                let new_type = PANIC_TO_RUST_TYPES.get(&ident.to_string());
                match new_type {
                    Some(new_type) => {
                        *path.segments.first_mut().unwrap() =
                            PathSegment::from(Ident::new(new_type, span))
                    }
                    None => panic!("Unknown type {}", ident),
                };
            }
        }
    }
}

fn main() {
    let code = quote! {
        fn add1(x: i32) -> i32 {
            x + 1
        }
    };

    let mut syntax_tree: File = syn::parse2(code).unwrap();
    PanicToRustTypes.visit_file_mut(&mut syntax_tree);
    println!("{}", quote!(#syntax_tree));
}
