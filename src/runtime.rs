use std::ops;

use enumset::enum_set;
use enumset::EnumSet;
use enumset::EnumSetType;

#[allow(dead_code)]
#[derive(EnumSetType, Debug)]
pub enum PanicEnum {
    StackOverflow,
    HeapOverflow,
    IntegerOverflow,
    IntegerDivisionByZero,
    ArrayIndexOutOfBounds,
}

pub type Panic = EnumSet<PanicEnum>;

#[allow(dead_code)]
pub struct Int32(Result<i32, Panic>);

#[allow(dead_code)]
pub struct Bool(Result<bool, Panic>);

#[allow(dead_code)]
pub struct Unit(Result<(), Panic>);

impl ops::Add for Int32 {
    type Output = Int32;

    fn add(self, other: Int32) -> Int32 {
        let output = match (self.0, other.0) {
            (Err(p1), Err(p2)) => Err(p1 | p2),
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

    fn sub(self, other: Int32) -> Int32 {
        let output = match (self.0, other.0) {
            (Err(p1), Err(p2)) => Err(p1 | p2),
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

    fn mul(self, other: Int32) -> Int32 {
        let output = match (self.0, other.0) {
            (Err(p1), Err(p2)) => Err(p1 | p2),
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

    fn div(self, other: Int32) -> Int32 {
        let output = match (self.0, other.0) {
            (Err(p1), Err(p2)) => Err(p1 | p2),
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

    fn rem(self, other: Int32) -> Int32 {
        let output = match (self.0, other.0) {
            (Err(p1), Err(p2)) => Err(p1 | p2),
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
