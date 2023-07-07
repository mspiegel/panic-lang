use crate::types::panic::Panic;

use super::anxious::Anxious;

#[allow(dead_code)]
#[derive(PartialEq, Eq, Debug)]
pub struct Bool(pub Result<bool, Panic>);

impl From<bool> for Bool {
    fn from(item: bool) -> Self {
        Bool(Ok(item))
    }
}

impl From<Panic> for Bool {
    fn from(item: Panic) -> Self {
        Bool(Err(item))
    }
}

impl Anxious for Bool {
    type Output = Bool;

    fn value(self) -> Self::Output {
        return Self::Output::from(self)
    }
}

impl Anxious for bool {
    type Output = Bool;

    fn value(self) -> Self::Output {
        return Self::Output::from(self)
    }
}