use crate::types::panic::Panic;
use crate::types::panic::PanicEnum;
use enumset::enum_set;
use std::ops;

#[allow(dead_code)]
#[derive(PartialEq, Eq, Debug)]
pub struct Int32(pub Result<i32, Panic>);

impl ops::Add for Int32 {
    type Output = Int32;

    #[inline(always)]
    fn add(self, other: Int32) -> Int32 {
        let output = match (self.0, other.0) {
            (Err(p1), Err(p2)) => Err(p1.union(p2)),
            (Err(p1), _) => Err(p1),
            (_, Err(p2)) => Err(p2),
            (Ok(a), Ok(b)) => {
                let (result, ok) = a.overflowing_add(b);
                if ok {
                    Ok(result)
                } else {
                    Err(enum_set!(PanicEnum::IntegerOverflow))
                }
            }
        };
        Int32(output)
    }
}

impl ops::Sub for Int32 {
    type Output = Int32;

    #[inline(always)]
    fn sub(self, other: Int32) -> Int32 {
        let output = match (self.0, other.0) {
            (Err(p1), Err(p2)) => Err(p1.union(p2)),
            (Err(p1), _) => Err(p1),
            (_, Err(p2)) => Err(p2),
            (Ok(a), Ok(b)) => {
                let (result, ok) = a.overflowing_sub(b);
                if ok {
                    Ok(result)
                } else {
                    Err(enum_set!(PanicEnum::IntegerOverflow))
                }
            }
        };
        Int32(output)
    }
}

impl ops::Mul for Int32 {
    type Output = Int32;

    #[inline(always)]
    fn mul(self, other: Int32) -> Int32 {
        let output = match (self.0, other.0) {
            (Err(p1), Err(p2)) => Err(p1.union(p2)),
            (Err(p1), _) => Err(p1),
            (_, Err(p2)) => Err(p2),
            (Ok(a), Ok(b)) => {
                let (result, ok) = a.overflowing_mul(b);
                if ok {
                    Ok(result)
                } else {
                    Err(enum_set!(PanicEnum::IntegerOverflow))
                }
            }
        };
        Int32(output)
    }
}

impl ops::Div for Int32 {
    type Output = Int32;

    #[inline(always)]
    fn div(self, other: Int32) -> Int32 {
        let output = match (self.0, other.0) {
            (Err(p1), Err(p2)) => Err(p1.union(p2)),
            (Err(p1), _) => Err(p1),
            (_, Err(p2)) => Err(p2),
            (Ok(_), Ok(0)) => Err(enum_set!(PanicEnum::IntegerDivisionByZero)),
            (Ok(a), Ok(b)) => {
                let (result, ok) = a.overflowing_div(b);
                if ok {
                    Ok(result)
                } else {
                    Err(enum_set!(PanicEnum::IntegerOverflow))
                }
            }
        };
        Int32(output)
    }
}

impl ops::Rem for Int32 {
    type Output = Int32;

    #[inline(always)]
    fn rem(self, other: Int32) -> Int32 {
        let output = match (self.0, other.0) {
            (Err(p1), Err(p2)) => Err(p1.union(p2)),
            (Err(p1), _) => Err(p1),
            (_, Err(p2)) => Err(p2),
            (Ok(_), Ok(0)) => Err(enum_set!(PanicEnum::IntegerDivisionByZero)),
            (Ok(a), Ok(b)) => {
                let (result, ok) = a.overflowing_rem(b);
                if ok {
                    Ok(result)
                } else {
                    Err(enum_set!(PanicEnum::IntegerOverflow))
                }
            }
        };
        Int32(output)
    }
}

impl ops::Neg for Int32 {
    type Output = Int32;

    #[inline(always)]
    fn neg(self) -> Int32 {
        let output = match self.0 {
            Err(p) => Err(p),
            Ok(a) => {
                let (result, ok) = a.overflowing_neg();
                if ok {
                    Ok(result)
                } else {
                    Err(enum_set!(PanicEnum::IntegerOverflow))
                }
            }
        };
        Int32(output)
    }
}

impl Int32 {
    #[inline(always)]
    pub fn anxious(self) -> Int32 {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_anxious() {
        assert_eq!(Int32(Ok(0)).anxious(), Int32(Ok(0)));
    }
}
