use super::panic::PanicType;

pub enum Anxious<T> {
    Nom(T),
    Panic(PanicType),
}

impl <T> From<PanicType> for Anxious<T> {
    fn from(item: PanicType) -> Self {
        Anxious::Panic(item)
    }
}