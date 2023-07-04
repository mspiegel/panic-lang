use crate::types::panic::Panic;

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