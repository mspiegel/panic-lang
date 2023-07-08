use super::anxious::Anxious;
use super::anxious::Anxious::Nom;

impl From<bool> for Anxious<bool> {
    fn from(item: bool) -> Self {
        Nom(item)
    }
}
