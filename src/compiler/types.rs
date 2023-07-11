use proc_macro2::TokenStream;
use quote::quote;
use std::str::FromStr;
use syn::visit_mut::VisitMut;
use syn::Type;

pub struct PanicToRustTypes;

pub fn is_anxious(node: &Type) -> bool {
    let type_string = quote!(#node).to_string();
    type_string.starts_with("Anxious < ") && type_string.ends_with(" >")
}

impl VisitMut for PanicToRustTypes {
    fn visit_type_mut(&mut self, node: &mut Type) {
        let type_string = quote!(#node).to_string();
        match type_string.strip_prefix("nom_") {
            Some(type_string) => {
                *node = Type::Verbatim(TokenStream::from_str(type_string).unwrap());
            }
            None => {
                let type_string = format!("Anxious<{}>", type_string);
                *node = Type::Verbatim(TokenStream::from_str(&type_string).unwrap());
            }
        }
    }
}
