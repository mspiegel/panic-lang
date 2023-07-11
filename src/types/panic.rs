#[allow(non_camel_case_types)]
#[allow(dead_code)]
#[derive(Debug)]
pub enum panic {
    StackOverflow, // not yet implemented
    HeapOverflow,  // not yet implemented
    IntegerOverflow,
    IntegerDivisionByZero,
    ArrayIndexOutOfBounds, // not yet implemented
}
