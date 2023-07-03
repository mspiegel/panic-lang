use crate::types::unit::Unit;

#[allow(dead_code)]
#[derive(PartialEq, Eq, Debug)]
pub struct NominalUnit(pub ());

impl NominalUnit {
    #[inline(always)]
    pub fn anxious(self) -> Unit {
        Unit(Ok(()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_anxious() {
        assert_eq!(NominalUnit(()).anxious(), Unit(Ok(())));
    }
}
