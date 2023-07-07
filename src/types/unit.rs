use crate::types::panic::Panic;

use super::anxious::Anxious;

#[allow(dead_code)]
#[derive(PartialEq, Eq, Debug)]
pub struct Unit(pub Result<(), Panic>);

impl From<()> for Unit {
    fn from(item: ()) -> Self {
        Unit(Ok(item))
    }
}

impl From<Panic> for Unit {
    fn from(item: Panic) -> Self {
        Unit(Err(item))
    }
}

impl Anxious for Unit {
    type Output = Unit;

    fn value(self) -> Self::Output {
        return Self::Output::from(self)
    }
}

impl Anxious for () {
    type Output = Unit;

    fn value(self) -> Self::Output {
        return Self::Output::from(self)
    }
}