use super::anxious::Anxious::Nom;
use super::anxious::Anxious;

impl From<bool> for Anxious<bool> {
    fn from(item: bool) -> Self {
        Nom(item)
    }
}