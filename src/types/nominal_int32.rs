use crate::types::int32::Int32;


#[allow(dead_code)]
#[derive(PartialEq, Eq, Debug)]
pub struct NominalInt32(pub i32);

#[allow(clippy::from_over_into)]
impl Into<Int32> for NominalInt32 {
    fn into(self) -> Int32 {
        match self {
            NominalInt32(val) => Int32(Ok(val))
        }
    }
}