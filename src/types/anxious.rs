pub trait Anxious {
    type Output;

    fn value(self) -> Self::Output;
}