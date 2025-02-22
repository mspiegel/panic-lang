#[derive(Debug, Clone, Eq)]
pub enum Type {
    Undetermined,
    EmptyList,
    Boolean,
    Integer,
    Character,
    LiteralString,
    Marker { uuid: u64 },
    Ref(Box<Type>),
    Slice(Box<Type>),
    Meta,
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Undetermined, Type::Undetermined) => true,
            (Type::EmptyList, Type::EmptyList) => true,
            (Type::Boolean, Type::Boolean) => true,
            (Type::Integer, Type::Integer) => true,
            (Type::Character, Type::Character) => true,
            (Type::LiteralString, Type::LiteralString) => true,
            (Type::Marker { uuid: a }, Type::Marker { uuid: b }) => a == b,
            (Type::Ref(a), Type::Ref(b)) => a == b,
            (Type::Slice(a), Type::Slice(b)) => a == b,
            (Type::Meta, Type::Meta) => true,
            _ => false,
        }
    }
}
