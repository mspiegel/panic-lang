use std::collections::HashMap;
use std::collections::HashSet;

use syn::visit_mut::VisitMut;
use syn::Type;
use syn::PathSegment;
use syn::Ident;

pub struct PanicToRustTypes;

lazy_static! {
    pub static ref PANIC_TO_RUST_NOMINAL_TYPES: HashMap<String, String> = HashMap::from([
        ("nom_i32".to_string(), "i32".to_string()),
        ("nom_bool".to_string(), "bool".to_string()),
        ("nom_()".to_string(), "()".to_string()),
    ]);
    pub static ref PANIC_TO_RUST_ANXIOUS_TYPES: HashMap<String, String> = HashMap::from([
        ("i32".to_string(), "Int32".to_string()),
        ("bool".to_string(), "Bool".to_string()),
        ("()".to_string(), "Unit".to_string()),
    ]);
    pub static ref PANIC_TO_RUST_TYPES: HashMap<String, String> = {
        let mut m = HashMap::new();
        m.extend(PANIC_TO_RUST_NOMINAL_TYPES.clone().into_iter());
        m.extend(PANIC_TO_RUST_ANXIOUS_TYPES.clone().into_iter());
        m
    };
    pub static ref PANIC_NOMINAL_TYPES: HashSet<String> = HashSet::from_iter(PANIC_TO_RUST_NOMINAL_TYPES.keys().cloned());
    pub static ref PANIC_ANXIOUS_TYPES: HashSet<String> = HashSet::from_iter(PANIC_TO_RUST_ANXIOUS_TYPES.keys().cloned());
    pub static ref RUST_NOMINAL_TYPES: HashSet<String> = HashSet::from_iter(PANIC_TO_RUST_NOMINAL_TYPES.values().cloned());
    pub static ref RUST_ANXIOUS_TYPES: HashSet<String> = HashSet::from_iter(PANIC_TO_RUST_ANXIOUS_TYPES.values().cloned());
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