#![cfg_attr(not(test), no_std)]

mod panic;
#[macro_use]
mod anxious_int;
#[macro_use]
mod nominal_int;
#[macro_use]
mod test;

use panic::Panic;

macro_rules! anxious_int_decl {
    ($SelfT:ident, $ActualT:ident) => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone)]
        pub struct $SelfT(pub(crate) Result<$ActualT, Panic>);
    };
}

anxious_int_decl! {AnxiousI8, i8}
anxious_int_decl! {AnxiousI16, i16}
anxious_int_decl! {AnxiousI32, i32}
anxious_int_decl! {AnxiousI64, i64}
anxious_int_decl! {AnxiousI128, i128}
anxious_int_decl! {AnxiousISize, isize}

anxious_int_impl! {AnxiousI8, i8, anxious_i8}
anxious_int_impl! {AnxiousI16, i16, anxious_i16}
anxious_int_impl! {AnxiousI32, i32, anxious_i32}
anxious_int_impl! {AnxiousI64, i64, anxious_i64}
anxious_int_impl! {AnxiousI128, i128, anxious_i128}
anxious_int_impl! {AnxiousISize, isize, anxious_isize}

macro_rules! nominal_int_decl {
    ($SelfT:ident, $ActualT:ident) => {
        #[allow(non_camel_case_types)]
        #[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Copy, Clone)]
        pub struct $SelfT(pub(crate) $ActualT);
    };
}

nominal_int_decl! {NominalI8, i8}
nominal_int_decl! {NominalI16, i16}
nominal_int_decl! {NominalI32, i32}
nominal_int_decl! {NominalI64, i64}
nominal_int_decl! {NominalI128, i128}
nominal_int_decl! {NominalISize, isize}

nominal_int_impl! {NominalI8, i8, nominal_i8}
nominal_int_impl! {NominalI16, i16, nominal_i16}
nominal_int_impl! {NominalI32, i32, nominal_i32}
nominal_int_impl! {NominalI64, i64, nominal_i64}
nominal_int_impl! {NominalI128, i128, nominal_i128}
nominal_int_impl! {NominalISize, isize, nominal_isize}

#[cfg(test)]
mod total_operations {
    use crate::*;
    use std::ops::Neg;

    anxious_test_binop! {test_anxious_i8_add, test_i8_add, +}
    anxious_test_binop! {test_anxious_i8_sub, test_i8_sub, -}
    anxious_test_binop! {test_anxious_i8_mul, test_i8_mul, *}
    anxious_test_binop! {test_anxious_i8_div, test_i8_div, /}
    anxious_test_binop! {test_anxious_i8_rem, test_i8_rem, %}

    anxious_test_binfun! {test_anxious_i8_div_euclid, test_i8_div_eucvlid, div_euclid}

    anxious_test_unaryfun! {test_anxious_i8_neg, test_i8_neg, neg}
    anxious_test_unaryfun! {test_anxious_i8_abs, test_i8_abs, abs}
}
