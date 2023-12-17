#[allow(dead_code)]
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Panic {
    ThisIsFine,
    StackOverflow, // not yet implemented
    HeapOverflow,  // not yet implemented
    IntegerOverflow,
    IntegerDivisionByZero,
    ArrayIndexOutOfBounds, // not yet implemented
}

#[cfg(test)]
mod panic_test {
    use super::*;

    #[test]
    fn test_clone() {
        assert_eq!(Panic::ThisIsFine.clone(), Panic::ThisIsFine.clone())
    }
}
