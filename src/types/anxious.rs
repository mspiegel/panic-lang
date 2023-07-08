use std::fmt::Debug;
use super::panic::PanicType;

pub enum Anxious<T> {
    Nom(T),
    Panic(PanicType),
}

impl<T> From<PanicType> for Anxious<T> {
    fn from(item: PanicType) -> Self {
        Anxious::Panic(item)
    }
}

impl<T: Debug> Debug for Anxious<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nom(arg0) => f.debug_tuple("Nom").field(arg0).finish(),
            Self::Panic(arg0) => f.debug_tuple("Panic").field(arg0).finish(),
        }
    }
}

pub trait AnxiousFactory {
    type Output;
    type Input: Into<Self::Output>;

    fn convert(item: Self::Input) -> Self::Output;
}

impl<T: Into<Anxious<T>>> AnxiousFactory for Anxious<T> {
    type Output = Anxious<T>;
    type Input = T;

    fn convert(item: Self::Input) -> Self::Output {
        item.into()
    }
}