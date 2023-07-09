#[macro_use]
extern crate lazy_static;

use quote::quote;
use syn::File;
use syn::visit_mut::VisitMut;

mod compiler;
mod types;

use crate::compiler::operators::PanicToRustOperators;
use crate::compiler::types::PanicToRustTypes;

fn pretty_print(ts: &proc_macro2::TokenStream) -> String {
    let file = syn::parse_file(&ts.to_string()).unwrap();
    prettyplease::unparse(&file)
}

fn main() {
    let code = quote! {
        fn add1(x: nom_i32) -> i32 {
            x + 1
        }
        fn add_xyz(x: nom_i32, y: nom_i32, z: nom_i32) -> i32 {
            x + y + z
        }
    };

    let mut syntax_tree: File = syn::parse2(code).unwrap();
    PanicToRustTypes.visit_file_mut(&mut syntax_tree);
    PanicToRustOperators.visit_file_mut(&mut syntax_tree);
    println!("{}", pretty_print(&quote!(#syntax_tree)));
}

