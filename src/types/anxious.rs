use super::panic::panic;
use std::fmt::Debug;
use std::fmt::Display;

pub enum Anxious<T> {
    Nom(T),
    Panic(panic),
}

impl<T> From<panic> for Anxious<T> {
    fn from(item: panic) -> Self {
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

impl<T: Display> Display for Anxious<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nom(val) => val.fmt(f),
            Self::Panic(p) => p.fmt(f),
        }
    }
}

pub trait AnxiousFactory {
    type Output;
    fn convert(self) -> Self::Output;
}

#[macro_export]
macro_rules! ack {
    ($e:expr) => {
        AnxiousFactory::convert($e)
    };
}
