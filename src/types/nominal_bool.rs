use crate::types::bool::Bool;

#[derive(PartialEq, Eq, Debug)]
pub struct NominalBool(pub bool);

#[allow(clippy::from_over_into)]
impl Into<Bool> for NominalBool {
    fn into(self) -> Bool {
        match self {
            NominalBool(val) => Bool(Ok(val))
        }
    }
}

#[allow(unused_macros)]
macro_rules! and {
    ($x:expr, $y:expr) => (
        match $x {
            NominalBool(true) => $y,
            NominalBool(false) => NominalBool(false),
        }
    )
}

#[allow(unused_macros)]
macro_rules! or {
    ($x:expr, $y:expr) => (
        match $x {
            NominalBool(true) => NominalBool(true),
            NominalBool(false) => $y,
        }
    )
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_and() {
        assert_eq!(and!(NominalBool(false), NominalBool(false)), NominalBool(false));
        assert_eq!(and!(NominalBool(false), NominalBool(true)), NominalBool(false));
        assert_eq!(and!(NominalBool(true), NominalBool(false)), NominalBool(false));
        assert_eq!(and!(NominalBool(true), NominalBool(true)), NominalBool(true));
    }

    #[test]
    fn test_or() {
        assert_eq!(or!(NominalBool(false), NominalBool(false)), NominalBool(false));
        assert_eq!(or!(NominalBool(false), NominalBool(true)), NominalBool(true));
        assert_eq!(or!(NominalBool(true), NominalBool(false)), NominalBool(true));
        assert_eq!(or!(NominalBool(true), NominalBool(true)), NominalBool(true));
    }

}