use crate::types::panic::Panic;
use crate::types::panic::PanicEnum;
use enumset::enum_set;
use std::ops;

#[allow(dead_code)]
pub struct Int32(pub Result<i32, Panic>);

impl<T: Into<Int32>> ops::Add<T> for Int32 {
    type Output = Int32;

    fn add(self, other: T) -> Int32 {
        let output = match (self.0, other.into().0) {
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

impl<T: Into<Int32>> ops::Sub<T> for Int32 {
    type Output = Int32;

    fn sub(self, other: T) -> Int32 {
        let output = match (self.0, other.into().0) {
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

impl<T: Into<Int32>> ops::Mul<T> for Int32 {
    type Output = Int32;

    fn mul(self, other: T) -> Int32 {
        let output = match (self.0, other.into().0) {
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

impl<T: Into<Int32>> ops::Div<T> for Int32 {
    type Output = Int32;

    fn div(self, other: T) -> Int32 {
        let output = match (self.0, other.into().0) {
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

impl<T: Into<Int32>> ops::Rem<T> for Int32 {
    type Output = Int32;

    fn rem(self, other: T) -> Int32 {
        let output = match (self.0, other.into().0) {
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
