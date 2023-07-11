use std::ops;

use super::anxious::Anxious;
use super::anxious::Anxious::Nom;
use super::anxious::Anxious::Panic;
use super::anxious::AnxiousFactory;
use super::panic::panic;

impl From<i32> for Anxious<i32> {
    fn from(item: i32) -> Self {
        Nom(item)
    }
}

impl AnxiousFactory for i32 {
    type Output = Anxious<i32>;
    fn convert(self) -> Self::Output {
        self.into()
    }
}

impl AnxiousFactory for Anxious<i32> {
    type Output = Anxious<i32>;
    fn convert(self) -> Self::Output {
        self.into()
    }
}

impl ops::Add for Anxious<i32> {
    type Output = Anxious<i32>;

    #[inline(always)]
    fn add(self, other: Anxious<i32>) -> Anxious<i32> {
        match (self, other) {
            (Panic(p1), _) => Panic(p1),
            (_, Panic(p2)) => Panic(p2),
            (Nom(a), Nom(b)) => {
                let (result, overflow) = a.overflowing_add(b);
                if !overflow {
                    Nom(result)
                } else {
                    Panic(panic::IntegerOverflow)
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
            (Panic(p1), _) => Panic(p1),
            (_, Panic(p2)) => Panic(p2),
            (Nom(a), Nom(b)) => {
                let (result, overflow) = a.overflowing_sub(b);
                if !overflow {
                    Nom(result)
                } else {
                    Panic(panic::IntegerOverflow)
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
            (Panic(p1), _) => Panic(p1),
            (_, Panic(p2)) => Panic(p2),
            (Nom(a), Nom(b)) => {
                let (result, overflow) = a.overflowing_mul(b);
                if !overflow {
                    Nom(result)
                } else {
                    Panic(panic::IntegerOverflow)
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
            (Panic(p1), _) => Panic(p1),
            (_, Panic(p2)) => Panic(p2),
            (Nom(_), Nom(0)) => Panic(panic::IntegerDivisionByZero),
            (Nom(a), Nom(b)) => {
                let (result, overflow) = a.overflowing_div(b);
                if !overflow {
                    Nom(result)
                } else {
                    Panic(panic::IntegerOverflow)
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
            (Panic(p1), _) => Panic(p1),
            (_, Panic(p2)) => Panic(p2),
            (Nom(_), Nom(0)) => Panic(panic::IntegerDivisionByZero),
            (Nom(a), Nom(b)) => {
                let (result, overflow) = a.overflowing_rem(b);
                if !overflow {
                    Nom(result)
                } else {
                    Panic(panic::IntegerOverflow)
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
                    Panic(panic::IntegerOverflow)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ack;

    #[test]
    fn test_debug() {
        assert_eq!("Nom(0)", format!("{:?}", Anxious::Nom(0)));
    }

    #[test]
    fn test_ack() {
        assert_eq!("Nom(0)", format!("{:?}", ack!(0)));
        assert_eq!("Nom(0)", format!("{:?}", ack!(Anxious::Nom(0))));
    }

    #[test]
    fn test_add() {
        assert_eq!("Nom(3)", format!("{:?}", Anxious::Nom(1) + Anxious::Nom(2)));
        assert_eq!(
            "Panic(IntegerOverflow)",
            format!("{:?}", Anxious::Nom(i32::MAX) + Anxious::Nom(1))
        );
    }
}
