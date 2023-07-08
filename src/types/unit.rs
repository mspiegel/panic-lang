use super::anxious::Anxious::Nom;
use super::anxious::Anxious;

impl From<()> for Anxious<()> {
    fn from(item: ()) -> Self {
        Nom(item)
    }
}