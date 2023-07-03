use crate::types::panic::Panic;

#[allow(dead_code)]
#[derive(PartialEq, Eq, Debug)]
pub struct Unit(pub Result<(), Panic>);

impl Unit {
    #[inline(always)]
    pub fn anxious(self) -> Unit {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_anxious() {
        assert_eq!(Unit(Ok(())).anxious(), Unit(Ok(())));
    }
}
