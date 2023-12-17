macro_rules! anxious_int_impl {
    ($SelfT:ident, $ActualT:ident, $module:ident) => {
        pub use $module::*;

        mod $module {
            use crate::*;
            use core::fmt;
            use core::ops;

            impl From<$ActualT> for $SelfT {
                fn from(item: $ActualT) -> $SelfT {
                    $SelfT(Ok(item))
                }
            }

            impl From<Panic> for $SelfT {
                fn from(item: Panic) -> $SelfT {
                    $SelfT(Err(item))
                }
            }

            impl $SelfT {
                pub const MAX: $SelfT = $SelfT(Ok($ActualT::MAX));

                pub const MIN: $SelfT = $SelfT(Ok($ActualT::MIN));

                #[must_use = "this returns the result of the operation, \
                      without modifying the original"]
                #[inline]
                pub const fn checked_add(self, rhs: $SelfT) -> $SelfT {
                    let inner = match (self.0, rhs.0) {
                        (Err(e), _) => Err(e),
                        (_, Err(e)) => Err(e),
                        (Ok(a), Ok(b)) => match a.checked_add(b) {
                            Some(val) => Ok(val),
                            None => Err(Panic::IntegerOverflow),
                        },
                    };
                    $SelfT(inner)
                }

                #[must_use = "this returns the result of the operation, \
                      without modifying the original"]
                #[inline]
                pub const fn checked_sub(self, rhs: $SelfT) -> $SelfT {
                    let inner = match (self.0, rhs.0) {
                        (Err(e), _) => Err(e),
                        (_, Err(e)) => Err(e),
                        (Ok(a), Ok(b)) => match a.checked_sub(b) {
                            Some(val) => Ok(val),
                            None => Err(Panic::IntegerOverflow),
                        },
                    };
                    $SelfT(inner)
                }

                #[must_use = "this returns the result of the operation, \
                    without modifying the original"]
                #[inline]
                pub const fn checked_mul(self, rhs: $SelfT) -> $SelfT {
                    let inner = match (self.0, rhs.0) {
                        (Err(e), _) => Err(e),
                        (_, Err(e)) => Err(e),
                        (Ok(a), Ok(b)) => match a.checked_mul(b) {
                            Some(val) => Ok(val),
                            None => Err(Panic::IntegerOverflow),
                        },
                    };
                    $SelfT(inner)
                }

                #[must_use = "this returns the result of the operation, \
                      without modifying the original"]
                #[inline]
                pub const fn checked_div(self, rhs: $SelfT) -> $SelfT {
                    let inner = match (self.0, rhs.0) {
                        (Err(e), _) => Err(e),
                        (_, Err(e)) => Err(e),
                        (Ok(_), Ok(0)) => Err(Panic::IntegerDivisionByZero),
                        (Ok(a), Ok(b)) => match a.checked_div(b) {
                            Some(val) => Ok(val),
                            None => Err(Panic::IntegerOverflow),
                        },
                    };
                    $SelfT(inner)
                }

                #[inline]
                pub const fn checked_rem(self, rhs: $SelfT) -> $SelfT {
                    let inner = match (self.0, rhs.0) {
                        (Err(e), _) => Err(e),
                        (_, Err(e)) => Err(e),
                        (Ok(_), Ok(0)) => Err(Panic::IntegerDivisionByZero),
                        (Ok(a), Ok(b)) => match a.checked_rem(b) {
                            Some(val) => Ok(val),
                            None => Err(Panic::IntegerOverflow),
                        },
                    };
                    $SelfT(inner)
                }

                #[inline]
                pub const fn max(self, rhs: $SelfT) -> $SelfT {
                    let inner = match (self.0, rhs.0) {
                        (Err(e), _) => Err(e),
                        (_, Err(e)) => Err(e),
                        (Ok(a), Ok(b)) => Ok(if a > b { a } else { b }),
                    };
                    $SelfT(inner)
                }

                #[inline]
                pub const fn min(self, rhs: $SelfT) -> $SelfT {
                    let inner = match (self.0, rhs.0) {
                        (Err(e), _) => Err(e),
                        (_, Err(e)) => Err(e),
                        (Ok(a), Ok(b)) => Ok(if a < b { a } else { b }),
                    };
                    $SelfT(inner)
                }

                #[inline]
                pub const fn abs(self) -> $SelfT {
                    let inner = match self.0 {
                        Err(e) => Err(e),
                        Ok(a) => match a.checked_abs() {
                            Some(val) => Ok(val),
                            None => Err(Panic::IntegerOverflow),
                        },
                    };
                    $SelfT(inner)
                }

                #[inline]
                pub const fn neg(self) -> $SelfT {
                    let inner = match self.0 {
                        Err(e) => Err(e),
                        Ok(a) => match a.checked_neg() {
                            Some(val) => Ok(val),
                            None => Err(Panic::IntegerOverflow),
                        },
                    };
                    $SelfT(inner)
                }

                #[inline]
                pub const fn div_euclid(self, rhs: $SelfT) -> $SelfT {
                    let inner = match (self.0, rhs.0) {
                        (Err(e), _) => Err(e),
                        (_, Err(e)) => Err(e),
                        (Ok(_), Ok(0)) => Err(Panic::IntegerDivisionByZero),
                        (Ok(a), Ok(b)) => match a.checked_div_euclid(b) {
                            Some(val) => Ok(val),
                            None => Err(Panic::IntegerOverflow),
                        },
                    };
                    $SelfT(inner)
                }
            }

            impl ops::Add<$SelfT> for $SelfT {
                type Output = $SelfT;

                fn add(self, rhs: $SelfT) -> $SelfT {
                    self.checked_add(rhs)
                }
            }

            impl ops::Sub<$SelfT> for $SelfT {
                type Output = $SelfT;

                fn sub(self, rhs: $SelfT) -> $SelfT {
                    self.checked_sub(rhs)
                }
            }

            impl ops::Mul<$SelfT> for $SelfT {
                type Output = $SelfT;

                fn mul(self, rhs: $SelfT) -> $SelfT {
                    self.checked_mul(rhs)
                }
            }

            impl ops::Div<$SelfT> for $SelfT {
                type Output = $SelfT;

                fn div(self, rhs: $SelfT) -> $SelfT {
                    self.checked_div(rhs)
                }
            }

            impl ops::Rem<$SelfT> for $SelfT {
                type Output = $SelfT;

                fn rem(self, rhs: $SelfT) -> $SelfT {
                    self.checked_rem(rhs)
                }
            }

            impl fmt::Debug for $SelfT {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    match self.0 {
                        Ok(val) => write!(f, "{:?}", val),
                        Err(err) => write!(f, "{:?}", err),
                    }
                }
            }

            #[cfg(test)]
            mod test {
                extern crate alloc;
                use alloc::format;

                use super::*;

                impl $SelfT {
                    fn structural_eq(self, rhs: $SelfT) -> bool {
                        self.0 == rhs.0
                    }
                }

                macro_rules! structural_eq {
                    ($lhs:expr, $rhs:expr) => {
                        $lhs.structural_eq($rhs)
                    };
                }

                #[test]
                fn test_add() {
                    assert!(structural_eq!(
                        $SelfT::from(Panic::ThisIsFine) + $SelfT::from(2),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(1) + $SelfT::from(Panic::ThisIsFine),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(1) + $SelfT::from(2),
                        $SelfT::from(3)
                    ));
                    assert!(structural_eq!(
                        $SelfT::MAX + $SelfT::from(1),
                        $SelfT::from(Panic::IntegerOverflow)
                    ));
                }

                #[test]
                fn test_sub() {
                    assert!(structural_eq!(
                        $SelfT::from(Panic::ThisIsFine) - $SelfT::from(2),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(1) - $SelfT::from(Panic::ThisIsFine),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(3) - $SelfT::from(2),
                        $SelfT::from(1)
                    ));
                    assert!(structural_eq!(
                        $SelfT::MIN - $SelfT::from(1),
                        $SelfT::from(Panic::IntegerOverflow)
                    ));
                }

                #[test]
                fn test_mul() {
                    assert!(structural_eq!(
                        $SelfT::from(Panic::ThisIsFine) * $SelfT::from(2),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(1) * $SelfT::from(Panic::ThisIsFine),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(2) * $SelfT::from(3),
                        $SelfT::from(6)
                    ));
                    assert!(structural_eq!(
                        $SelfT::MAX * $SelfT::MAX,
                        $SelfT::from(Panic::IntegerOverflow)
                    ));
                }

                #[test]
                fn test_div() {
                    assert!(structural_eq!(
                        $SelfT::from(Panic::ThisIsFine) / $SelfT::from(2),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(1) / $SelfT::from(Panic::ThisIsFine),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(3) / $SelfT::from(2),
                        $SelfT::from(1)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(1) / $SelfT::from(0),
                        $SelfT::from(Panic::IntegerDivisionByZero)
                    ));
                    assert!(structural_eq!(
                        $SelfT::MIN / $SelfT::from(-1),
                        $SelfT::from(Panic::IntegerOverflow)
                    ));
                }

                #[test]
                fn test_rem() {
                    assert!(structural_eq!(
                        $SelfT::from(Panic::ThisIsFine) % $SelfT::from(2),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(1) % $SelfT::from(Panic::ThisIsFine),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(3) % $SelfT::from(2),
                        $SelfT::from(1)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(1) % $SelfT::from(0),
                        $SelfT::from(Panic::IntegerDivisionByZero)
                    ));
                    assert!(structural_eq!(
                        $SelfT::MIN % $SelfT::from(-1),
                        $SelfT::from(Panic::IntegerOverflow)
                    ));
                }

                #[test]
                fn test_max() {
                    assert!(structural_eq!(
                        $SelfT::from(Panic::ThisIsFine).max($SelfT::from(1)),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(2).max($SelfT::from(Panic::ThisIsFine)),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(2).max($SelfT::from(1)),
                        $SelfT::from(2)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(1).max($SelfT::from(2)),
                        $SelfT::from(2)
                    ));
                }

                #[test]
                fn test_min() {
                    assert!(structural_eq!(
                        $SelfT::from(Panic::ThisIsFine).min($SelfT::from(1)),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(2).min($SelfT::from(Panic::ThisIsFine)),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(2).min($SelfT::from(1)),
                        $SelfT::from(1)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(1).min($SelfT::from(2)),
                        $SelfT::from(1)
                    ));
                }

                #[test]
                fn test_abs() {
                    assert!(structural_eq!(
                        $SelfT::from(Panic::ThisIsFine).abs(),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!($SelfT::from(0).abs(), $SelfT::from(0)));
                    assert!(structural_eq!(
                        $SelfT::MIN.abs(),
                        $SelfT::from(Panic::IntegerOverflow)
                    ));
                }

                #[test]
                fn test_neg() {
                    assert!(structural_eq!(
                        $SelfT::from(Panic::ThisIsFine).neg(),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!($SelfT::from(0).neg(), $SelfT::from(0)));
                    assert!(structural_eq!(
                        $SelfT::MIN.neg(),
                        $SelfT::from(Panic::IntegerOverflow)
                    ));
                }

                #[test]
                fn test_div_euclid() {
                    assert!(structural_eq!(
                        $SelfT::from(Panic::ThisIsFine).div_euclid($SelfT::from(1)),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(2).div_euclid($SelfT::from(Panic::ThisIsFine)),
                        $SelfT::from(Panic::ThisIsFine)
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(7).div_euclid($SelfT::from(4)),
                        $SelfT::from(1)
                    ));
                    assert!(structural_eq!(
                        $SelfT::MIN.div_euclid($SelfT::from(0)),
                        $SelfT::from(Panic::IntegerDivisionByZero)
                    ));
                    assert!(structural_eq!(
                        $SelfT::MIN.div_euclid($SelfT::from(-1)),
                        $SelfT::from(Panic::IntegerOverflow)
                    ));
                }

                #[test]
                fn test_clone() {
                    assert!(structural_eq!(
                        $SelfT::from(0).clone(),
                        $SelfT::from(0).clone()
                    ));
                    assert!(structural_eq!(
                        $SelfT::from(Panic::ThisIsFine).clone(),
                        $SelfT::from(Panic::ThisIsFine).clone()
                    ));
                }

                #[test]
                fn test_debug() {
                    assert_eq!(format!("{:?}", $SelfT::from(1)), "1");
                    assert_eq!(
                        format!("{:?}", $SelfT::from(Panic::ThisIsFine)),
                        "ThisIsFine"
                    );
                }
            }
        }
    };
}
