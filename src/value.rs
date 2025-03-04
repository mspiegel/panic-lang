use num_bigint::BigInt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    EmptyList,
    Integer(BigInt),
    Boolean(bool),
    Character(char),
    String(String),
}
