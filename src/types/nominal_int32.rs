use crate::types::int32::Int32;

#[allow(dead_code)]
#[derive(PartialEq, Eq, Debug)]
pub struct NominalInt32(pub i32);

impl NominalInt32 {
    #[inline(always)]
    pub fn anxious(self) -> Int32 {
        Int32(Ok(self.0))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_anxious() {
        assert_eq!(NominalInt32(0).anxious(), Int32(Ok(0)));
    }
}
