use enumset::EnumSet;
use enumset::EnumSetType;

#[allow(dead_code)]
#[derive(EnumSetType, Debug)]
pub enum PanicEnum {
    StackOverflow,
    HeapOverflow,
    IntegerOverflow,
    IntegerDivisionByZero,
    ArrayIndexOutOfBounds,
}

pub type Panic = EnumSet<PanicEnum>;
