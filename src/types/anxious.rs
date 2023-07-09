use super::panic::PanicType;
use std::fmt::Debug;

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
    fn convert(self) -> Self::Output;
}

#[macro_export]
macro_rules! ack {
    ($e:expr) => { AnxiousFactory::convert($e) };
}