use enumset::enum_set;
use std::ops;

use super::anxious::Anxious;
use super::anxious::Anxious::Nom;
use super::anxious::Anxious::Panic;
use super::panic::PanicEnum;

impl From<i32> for Anxious<i32> {
    fn from(item: i32) -> Self {
        Nom(item)
    }
}

impl ops::Add for Anxious<i32> {
    type Output = Anxious<i32>;

    #[inline(always)]
    fn add(self, other: Anxious<i32>) -> Anxious<i32> {
        match (self, other) {
            (Panic(p1), Panic(p2)) => Panic(p1.union(p2)),
            (Panic(p1), _) => Panic(p1),
            (_, Panic(p2)) => Panic(p2),
            (Nom(a), Nom(b)) => {
                let (result, overflow) = a.overflowing_add(b);
                if !overflow {
                    Nom(result)
                } else {
                    Panic(enum_set!(PanicEnum::IntegerOverflow))
                }
            }
        }
    }
}

impl ops::Sub for Anxious<i32> {
    type Output = Anxious<i32>;

    #[inline(always)]
    fn sub(self, other: Anxious<i32>) -> Anxious<i32> {
        match (self, other) {
            (Panic(p1), Panic(p2)) => Panic(p1.union(p2)),
            (Panic(p1), _) => Panic(p1),
            (_, Panic(p2)) => Panic(p2),
            (Nom(a), Nom(b)) => {
                let (result, overflow) = a.overflowing_sub(b);
                if !overflow {
                    Nom(result)
                } else {
                    Panic(enum_set!(PanicEnum::IntegerOverflow))
                }
            }
        }
    }
}

impl ops::Mul for Anxious<i32> {
    type Output = Anxious<i32>;

    #[inline(always)]
    fn mul(self, other: Anxious<i32>) -> Anxious<i32> {
        match (self, other) {
            (Panic(p1), Panic(p2)) => Panic(p1.union(p2)),
            (Panic(p1), _) => Panic(p1),
            (_, Panic(p2)) => Panic(p2),
            (Nom(a), Nom(b)) => {
                let (result, overflow) = a.overflowing_mul(b);
                if !overflow {
                    Nom(result)
                } else {
                    Panic(enum_set!(PanicEnum::IntegerOverflow))
                }
            }
        }
    }
}

impl ops::Div for Anxious<i32> {
    type Output = Anxious<i32>;

    #[inline(always)]
    fn div(self, other: Anxious<i32>) -> Anxious<i32> {
        match (self, other) {
            (Panic(p1), Panic(p2)) => Panic(p1.union(p2)),
            (Panic(p1), _) => Panic(p1),
            (_, Panic(p2)) => Panic(p2),
            (Nom(_), Nom(0)) => Panic(enum_set!(PanicEnum::IntegerDivisionByZero)),
            (Nom(a), Nom(b)) => {
                let (result, overflow) = a.overflowing_div(b);
                if !overflow {
                    Nom(result)
                } else {
                    Panic(enum_set!(PanicEnum::IntegerOverflow))
                }
            }
        }
    }
}

impl ops::Rem for Anxious<i32> {
    type Output = Anxious<i32>;

    #[inline(always)]
    fn rem(self, other: Anxious<i32>) -> Anxious<i32> {
        match (self, other) {
            (Panic(p1), Panic(p2)) => Panic(p1.union(p2)),
            (Panic(p1), _) => Panic(p1),
            (_, Panic(p2)) => Panic(p2),
            (Nom(_), Nom(0)) => Panic(enum_set!(PanicEnum::IntegerDivisionByZero)),
            (Nom(a), Nom(b)) => {
                let (result, overflow) = a.overflowing_rem(b);
                if !overflow {
                    Nom(result)
                } else {
                    Panic(enum_set!(PanicEnum::IntegerOverflow))
                }
            }
        }
    }
}

impl ops::Neg for Anxious<i32> {
    type Output = Anxious<i32>;

    #[inline(always)]
    fn neg(self) -> Anxious<i32> {
        match self {
            Panic(p) => Panic(p),
            Nom(a) => {
                let (result, overflow) = a.overflowing_neg();
                if !overflow {
                    Nom(result)
                } else {
                    Panic(enum_set!(PanicEnum::IntegerOverflow))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_debug() {
        assert_eq!("Nom(0)", format!("{:?}", Anxious::Nom(0)));
    }

    #[test]
    fn test_add() {
        assert_eq!("Nom(3)", format!("{:?}", Anxious::Nom(1) + Anxious::Nom(2)));
        assert_eq!(
            "Panic(EnumSet(IntegerOverflow))",
            format!("{:?}", Anxious::Nom(i32::MAX) + Anxious::Nom(1))
        );
    }
}
