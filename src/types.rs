#[derive(Debug, Clone)]
pub enum Type {
    Undetermined,
    EmptyList,
    Boolean,
    Integer,
    Character,
    LiteralString,
    Marker {
        uuid: u64,
    },
    // Value
    // Reference
    // Resource
    Newtype {
        uuid: u64,
        inner: Box<Type>,
    },
    Ref(Box<Type>),
    Slice(Box<Type>),
    Function {
        arguments: Vec<Type>,
        retval: Box<Type>,
    },
    Union {
        default: Box<Type>,
        variants: Vec<Type>,
    },
    Meta,
}
