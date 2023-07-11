use std::error::Error;
use std::io::stdin;
use std::io::Read;

use quote::quote;
use syn::visit_mut::VisitMut;

mod compiler;
mod types;

use crate::compiler::errors::PanicCompilerErrors;
use crate::compiler::operators::PanicToRustOperators;
use crate::compiler::types::PanicToRustTypes;

fn pretty_print(ts: &proc_macro2::TokenStream) -> String {
    let file = syn::parse_file(&ts.to_string()).unwrap();
    prettyplease::unparse(&file)
}

fn main() -> Result<(), Box<dyn Error>> {
    /*
    let code = quote! {
        fn add1(x: nom_i32) -> i32 {
            x + 1
        }
        fn add_xyz(x: nom_i32, y: nom_i32, z: nom_i32) -> i32 {
            x + y + z
        }
        fn only_inline_functions_are_allowed_to_be_void() {

        }
        fn only_inline_functions_are_allowed_to_be_return_nom_values() -> nom_i32 {

        }
    };
    */

    let mut content = String::new();
    stdin().lock().read_to_string(&mut content)?;
    let mut ast = syn::parse_file(&content)?;
    PanicToRustTypes.visit_file_mut(&mut ast);
    PanicToRustOperators.visit_file_mut(&mut ast);
    PanicCompilerErrors.visit_file_mut(&mut ast);
    println!("{}", pretty_print(&quote!(#ast)));
    return Ok(());
}
