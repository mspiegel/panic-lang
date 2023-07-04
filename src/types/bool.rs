use crate::types::panic::Panic;

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