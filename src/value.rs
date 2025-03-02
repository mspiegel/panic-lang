use num_bigint::BigInt;

#[derive(Debug)]
pub enum Value {
    EmptyList,
    Integer(BigInt),
    Boolean(bool),
    Character(char),
    String(String),
}
