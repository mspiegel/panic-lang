#[cfg(test)]
macro_rules! anxious_test_binop {
    ($anxious_int_func:ident, $int_func:ident, $op:tt) => {
        #[test]
        fn $anxious_int_func() {
            for lhs in i8::MIN..=i8::MAX {
                for rhs in i8::MIN..=i8::MAX {
                    _ = AnxiousI8::from(lhs) $op AnxiousI8::from(rhs);
                }
                _ = AnxiousI8::from(Panic::ThisIsFine) $op AnxiousI8::from(lhs);
                _ = AnxiousI8::from(lhs) $op AnxiousI8::from(Panic::ThisIsFine);
            }
            _ = AnxiousI8::from(Panic::ThisIsFine) $op AnxiousI8::from(Panic::ThisIsFine);
        }

        #[test]
        fn $int_func() {
            let panics = std::panic::catch_unwind(||
                for lhs in i8::MIN..=i8::MAX {
                    for rhs in i8::MIN..=i8::MAX {
                        _ = lhs $op rhs;
                    }
                }
            ).is_err();
            assert!(panics);
        }
    }
}

#[cfg(test)]
macro_rules! anxious_test_binfun {
    ($anxious_int_func:ident, $int_func:ident, $fun:ident) => {
        #[test]
        fn $anxious_int_func() {
            for lhs in i8::MIN..=i8::MAX {
                for rhs in i8::MIN..=i8::MAX {
                    _ = AnxiousI8::from(lhs).$fun(AnxiousI8::from(rhs));
                }
                _ = AnxiousI8::from(Panic::ThisIsFine).$fun(AnxiousI8::from(lhs));
                _ = AnxiousI8::from(lhs).$fun(AnxiousI8::from(Panic::ThisIsFine));
            }
            _ = AnxiousI8::from(Panic::ThisIsFine).$fun(AnxiousI8::from(Panic::ThisIsFine));
        }

        #[test]
        fn $int_func() {
            let panics = std::panic::catch_unwind(|| {
                for lhs in i8::MIN..=i8::MAX {
                    for rhs in i8::MIN..=i8::MAX {
                        _ = lhs.$fun(rhs);
                    }
                }
            })
            .is_err();
            assert!(panics);
        }
    };
}

#[cfg(test)]
macro_rules! anxious_test_unaryfun {
    ($anxious_int_func:ident, $int_func:ident, $fun:ident) => {
        #[test]
        fn $anxious_int_func() {
            for val in i8::MIN..=i8::MAX {
                _ = AnxiousI8::from(val).$fun();
            }
            _ = AnxiousI8::from(Panic::ThisIsFine).$fun();
        }

        #[test]
        fn $int_func() {
            let panics = std::panic::catch_unwind(|| {
                for val in i8::MIN..=i8::MAX {
                    _ = val.$fun();
                }
            })
            .is_err();
            assert!(panics);
        }
    };
}
