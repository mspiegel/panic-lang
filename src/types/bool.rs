use crate::types::panic::Panic;

#[allow(dead_code)]
#[derive(PartialEq, Eq, Debug)]
pub struct Bool(pub Result<bool, Panic>);

impl Bool {

    #[inline(always)]
    pub fn anxious(self) -> Bool {
        return self;
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_anxious() {
        assert_eq!(Bool(Ok(true)).anxious(), Bool(Ok(true)));
        assert_eq!(Bool(Ok(false)).anxious(), Bool(Ok(false)));        
    }

}