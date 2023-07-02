use crate::types::unit::Unit;

#[allow(dead_code)]
#[derive(PartialEq, Eq, Debug)]
pub struct NominalUnit(pub ());

#[allow(clippy::from_over_into)]
impl Into<Unit> for NominalUnit {
    fn into(self) -> Unit {
        match self {
            NominalUnit(val) => Unit(Ok(val))
        }
    }
}